/**
 * Copyright 2020 Florian Kramer
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include "HttpServer.h"

#include <chrono>
#include <cstdlib>
#include <ctime>
#include <fstream>
#include <nlohmann/json.hpp>
#include <thread>

#include "Logger.h"
#include "Os.h"
#include "Random.h"
HttpServer::HttpServer(std::shared_ptr<UserManager> authenticator,
                       const std::string &base_dir)
    : user_manager(authenticator), _base_dir(base_dir), _err(nullptr) {
  _cert_path = base_dir + "/cert/certificate.pem";
  _key_path = base_dir + "/cert/key.pem";
  _dh_param_path = base_dir + "/cert/dh.pem";
}

HttpServer::~HttpServer() {}

void HttpServer::setWSSHandler(std::shared_ptr<WebSocketHandler> wss_handler) {
  _wss_handler = wss_handler;
}

void HttpServer::registerRequestHandler(
    const std::string &path, RequestType type,
    std::shared_ptr<RequestHandler> handler) {
  if (type == RequestType::GET) {
    _get_request_handlers.push_back({std::regex(path), handler});
  } else {
    _post_request_handlers.push_back({std::regex(path), handler});
  }
}

HttpServer::HttpResponse HttpServer::onGet(const HttpRequest &req) {
  HttpResponse resp;
  resp.setError(404, "Not Found");
  try {
    UserManager::UserPtr user;
    std::string cookies = req.getHeader("cookie");
    if (req.path != "/auth") {
      if (!(user = user_manager->authenticateViaCookies(cookies))) {
        if (req.path == "/") {
          resp.setError(307, "Not Authenticated");
          resp.setHeader("Location", "/auth");
        } else {
          resp.setError(404, "Page not found");
        }
        return resp;
      }
    }
    std::string realpath = req.path;

    // Call any registered request handlers
    for (std::pair<std::regex, std::shared_ptr<RequestHandler>> &r :
         _get_request_handlers) {
      if (std::regex_match(realpath, r.first)) {
        return r.second->onRequest(user, req);
      }
    }

    if (realpath == "/") {
      realpath = "/index.html";
    }
    // The auth page is not served from the filesystem
    if (realpath == "/auth") {
      if (user_manager->authenticateViaCookies(cookies)) {
        resp.status_code = 307;
        resp.setHeader("Location", "/");
        resp.setBody("Authentication successfull.");
      } else {
        resp.status_code = 200;
        resp.setBody(AUTH_PAGE);
        resp.setHeader("Content-Type", "text/html");
      }
    } else if (realpath == "/auth/list") {
      if (user_manager->authenticateViaCookies(cookies)) {
        nlohmann::json data;
        std::vector<UserManager::PublicUserInfo> users =
            user_manager->listUsers();
        nlohmann::json users_json = nlohmann::json::array();
        for (const UserManager::PublicUserInfo &user : users) {
          nlohmann::json user_json;
          user_json["id"] = user.id;
          user_json["name"] = user.name;

          nlohmann::json permissions = nlohmann::json::array();
          if (user.permissions &
              (int64_t(1) << int64_t(UserManager::Permission::MODIFY_USERS))) {
            permissions.push_back("modify-users");
          }
          if (user.permissions &
              (int64_t(1) << int64_t(UserManager::Permission::ADMIN))) {
            permissions.push_back("admin");
          }
          user_json["permissions"] = permissions;
          users_json.push_back(user_json);
        }
        data["users"] = users_json;
        resp.status_code = 200;
        resp.setBody(data.dump());
        resp.setHeader("Content-Type", "application/json");
      } else {
        resp.status_code = 404;
        resp.setBody("Not Found");
      }
    } else if (realpath == "/auth/logout") {
      UserManager::UserPtr user;
      if ((user = user_manager->authenticateViaCookies(cookies))) {
        resp.status_code = 200;
        resp.setBody("Ok");
        resp.setHeader("Set-Cookie", user->createClearCookieHeader());
      } else {
        resp.setError(404, "Not Found");
      }
    } else if (realpath == "/auth/self") {
      UserManager::UserPtr user;
      if ((user = user_manager->authenticateViaCookies(cookies)) != nullptr) {
        nlohmann::json user_json;
        user_json["id"] = user->id();
        user_json["name"] = user->name();

        nlohmann::json permissions = nlohmann::json::array();
        if (user->hasPermission(UserManager::Permission::MODIFY_USERS)) {
          permissions.push_back("modify-users");
        }
        if (user->hasPermission(UserManager::Permission::ADMIN)) {
          permissions.push_back("admin");
        }
        user_json["permissions"] = permissions;
        resp.status_code = 200;
        resp.setBody(user_json.dump());
        resp.setHeader("Content-Type", "application/json");
      } else {
        resp.setError(404, "Not Found");
      }
    } else {
      {
        realpath = _serve_dir + realpath;
        realpath = os::realpath(realpath);
      }
      LOG_DEBUG << "GET: " << realpath << LOG_END;

      if (realpath.substr(0, _serve_dir.size()) != _serve_dir) {
        LOG_WARN << "Got a request for a file outside the basepath " << realpath
                 << LOG_END;
        resp.setError(403, "Forbidden");
        return resp;
      }

      resp.fromFile(realpath);
    }
  } catch (const std::exception &e) {
    LOG_ERROR
        << "HttpServer::get An error occurred while handling a request for "
        << req.path << " " << e.what() << LOG_END;
  } catch (...) {
    LOG_ERROR << "HttpServer::get An unkndown error occurred while handling "
                 "a request for "
              << req.path << LOG_END;
  }
  return resp;
}

HttpServer::HttpResponse HttpServer::onPost(const HttpRequest &req) {
  using nlohmann::json;
  HttpResponse resp;
  resp.setError(404, "Not Found");
  try {
    std::string cookies = req.getHeader("cookie");
    UserManager::UserPtr user = user_manager->authenticateViaCookies(cookies);
    if (req.path != "/auth") {
      if (!user) {
        resp.setError(404, "Not Found");
        return resp;
      }
    } else {
      LOG_DEBUG << "Handling an authentification attempt" << LOG_END;
      // got an authentification attempt
      json data = json::parse(req.getBodyString());
      if (user == nullptr &&
          (!data.contains("name") || !data.contains("password"))) {
        resp.setError(404, "Not Found");
        return resp;
      }
      if (user == nullptr) {
        // attempt to authenticate the user
        std::string name = data["name"].get<std::string>();
        std::string password = data["password"].get<std::string>();
        LOG_DEBUG << "Trying to authenticate " << name << LOG_END;
        user = user_manager->authenticateViaLogin(name, password);
        if (user != nullptr) {
          LOG_DEBUG << "Authenticated the user" << LOG_END;
          resp.status_code = 200;
          resp.setBody("Ok");
          resp.setHeader("Set-Cookie", user->createSetCookieHeader());
        } else {
          resp.setError(404, "Not Found");
        }
        return resp;
      }
      if (!data.contains("action")) {
        // No action specified and the user is already logged in. Simply
        // confirm the login
        resp.status_code = 200;
        resp.setBody("Ok");
        resp.setHeader("Set-Cookie", user->createSetCookieHeader());
        return resp;
      }
      std::string action = data.at("action").get<std::string>();
      if (action == "CreateUser") {
        if (!user->hasPermission(UserManager::Permission::MODIFY_USERS)) {
          resp.setError(404, "Not Found");
          return resp;
        }

        std::string new_name = data.at("new-name").get<std::string>();
        std::string new_password = data.at("new-password").get<std::string>();
        std::vector<UserManager::Permission> new_permissions;
        for (json perm : data.at("new-permissions")) {
          std::string perm_str = perm.get<std::string>();
          if (perm_str == "modify-users") {
            new_permissions.push_back(UserManager::Permission::MODIFY_USERS);
          } else if (perm_str == "admin") {
            if (user->hasPermission(UserManager::Permission::ADMIN)) {
              new_permissions.push_back(UserManager::Permission::ADMIN);
            } else {
              LOG_WARN << "A user named " << user->name()
                       << " tried to create an admin account without being"
                          "an admin."
                       << LOG_END;
            }
          }
        }

        user_manager->createUser(new_name, new_password, new_permissions);
        resp.status_code = 200;
        resp.setBody("Ok");
        return resp;
      } else if (action == "DeleteUser") {
        if (!user->hasPermission(UserManager::Permission::MODIFY_USERS)) {
          resp.setError(404, "Not Found");
          return resp;
        }
        int64_t to_delete_id = data.at("to-delete").get<int64_t>();
        UserManager::UserPtr to_delete = user_manager->loadUser(to_delete_id);
        if (to_delete == nullptr) {
          resp.setError(404, "Not Found");
          return resp;
        }
        if (to_delete->hasPermission(UserManager::Permission::ADMIN) &&
            !user->hasPermission(UserManager::Permission::ADMIN)) {
          resp.setError(400,
                        "You do not have the permissions required to delete an "
                        "admin account.");
          return resp;
        }
        user_manager->deleteUser(to_delete->id());
        resp.status_code = 200;
        resp.setBody("Ok");
        return resp;
      } else if (action == "SetPermissions") {
        if (!user->hasPermission(UserManager::Permission::MODIFY_USERS)) {
          resp.setError(404, "Not Found");
          return resp;
        }
        int64_t to_modify_id = data.at("to-modify").get<int64_t>();
        UserManager::UserPtr to_modify = user_manager->loadUser(to_modify_id);
        if (to_modify == nullptr) {
          resp.setError(400, "No user with id " + std::to_string(to_modify_id));
          return resp;
        }
        if (to_modify->hasPermission(UserManager::Permission::ADMIN) &&
            !user->hasPermission(UserManager::Permission::ADMIN)) {
          resp.setError(400,
                        "You do not have the permissions required to modify an "
                        "admin account.");
          return resp;
        }
        std::vector<UserManager::Permission> new_permissions;
        for (json perm : data.at("new-permissions")) {
          std::string perm_str = perm.get<std::string>();
          if (perm_str == "modify-users") {
            new_permissions.push_back(UserManager::Permission::MODIFY_USERS);
          } else if (perm_str == "admin") {
            if (user->hasPermission(UserManager::Permission::ADMIN)) {
              new_permissions.push_back(UserManager::Permission::ADMIN);
            } else {
              LOG_WARN
                  << "A user named " << user->name()
                  << " tried to create an admin account without being an admin."
                  << LOG_END;
            }
          }
        }
        to_modify->setPermissions(new_permissions);
        resp.status_code = 200;
        resp.setBody("Ok");
        return resp;
      } else if (action == "SetPassword") {
        int64_t to_modify_id = data.at("to-modify").get<int64_t>();
        UserManager::UserPtr to_modify = user_manager->loadUser(to_modify_id);
        if (to_modify == nullptr) {
          resp.setError(404, "Not Found");
          return resp;
        }
        if (!user->hasPermission(UserManager::Permission::MODIFY_USERS) &&
            to_modify->id() != user->id()) {
          resp.setError(404, "Not Found");
          return resp;
        }
        std::string new_password = data.at("new-password").get<std::string>();
        user->setPassword(new_password);
        resp.status_code = 200;
        resp.setBody("Ok");
        return resp;
      }
      int64_t to_modify_id = data.at("to-modify").get<int64_t>();
      UserManager::UserPtr to_modify = user_manager->loadUser(to_modify_id);
      if (to_modify == nullptr) {
        resp.setError(400, "No user with id " + std::to_string(to_modify_id));
        return resp;
      }
      if (to_modify->hasPermission(UserManager::Permission::ADMIN) &&
          !user->hasPermission(UserManager::Permission::ADMIN)) {
        resp.setError(
            400,
            "You do not have the permissions required to modify an admin "
            "account.");
        return resp;
      }
      std::string new_password = data.at("new-password").get<std::string>();
      to_modify->setPassword(new_password);
      resp.status_code = 200;
      resp.setBody("Ok");
      return resp;
    }

    std::string realpath = req.path;
    // Call any registered request handlers
    for (std::pair<std::regex, std::shared_ptr<RequestHandler>> &r :
         _post_request_handlers) {
      if (std::regex_match(realpath, r.first)) {
        return r.second->onRequest(user, req);
      }
    }
  } catch (const std::exception &e) {
    LOG_ERROR
        << "HttpServer::post An error occurred while handling a request for "
        << req.path << " " << e.what() << LOG_END;
  } catch (...) {
    LOG_ERROR << "HttpServer::post An unkndown error occurred while handling "
                 "a request for "
              << req.path << LOG_END;
  }
  return resp;
}

void HttpServer::run() {
  constexpr int PORT = 8082;
  if (_base_dir == ".") {
    _base_dir = os::getcwd();
  }
  _serve_dir = _base_dir;
  _serve_dir += "/html";
  _serve_dir = os::realpath(_serve_dir);

  /**
   * @brief Handles both get as well as post requests
   */
  auto handleRequest = [this](uWS::HttpResponse<true> *resp,
                              uWS::HttpRequest *req) {
    // WE need to read req now, as it won't be available later. We then create
    // a copy for the lamda that fires once all data is available.
    HttpRequest http_req_src(req);

    std::shared_ptr<std::ostringstream> buffer = std::make_shared<std::ostringstream>();
    resp->onData([this, buffer, resp, http_req_src](std::string_view data,
                                                  bool is_last) {
      size_t new_size = size_t(buffer->tellp()) + data.size();
      if (new_size > (5 << 20)) {
        resp->end("Requests over 5 MiB are not supported");
      }
      buffer->write(data.data(), data.size());
      if (is_last) {
        HttpRequest http_req(http_req_src);
        // Write our response
        std::string req_body = buffer->str();
        http_req.body = req_body.c_str();
        http_req.body_length = req_body.size();

        HttpResponse http_resp;
        if (http_req.type == RequestType::GET) {
          http_resp = onGet(http_req);
        } else {
          http_resp = onPost(http_req);
        }

        resp->writeStatus(std::to_string(http_resp.status_code));

        for (const auto &header : http_resp.headers) {
          resp->writeHeader(header.first.c_str(), header.second.c_str());
        }

        std::string_view body(http_resp.body.data(), http_resp.body.size());
        resp->end(body);
      }
    });
    resp->onAborted([]() {});
  };

  std::thread test([]() {
    uWS::App()
        .get("/*",
             [](uWS::HttpResponse<false> *resp, uWS::HttpRequest *req) {
               LOG_DEBUG << "Got a request for " << req->getUrl() << LOG_END;
               resp->writeStatus("200 Ok");
               resp->end("Hello World!");
             })
        .listen(8080, [](auto *token) {})
        .run();
  });
  test.detach();

  us_socket_context_options_t opt = {};
  opt.cert_file_name = _cert_path.c_str();
  opt.key_file_name = _key_path.c_str();
  opt.dh_params_file_name = _dh_param_path.c_str();
  uWS::SSLApp(opt)
      .get("/*", handleRequest)
      .post("/*", handleRequest)
      .ws<size_t>(
          "/*",
          {.upgrade =
               [this](uWS::HttpResponse<true> *resp, uWS::HttpRequest *req,
                      struct us_socket_context_t *ctx) {
                 HttpRequest http_req(req);
                 std::optional<size_t> client_id =
                     _wss_handler->onClientHandshake(http_req);
                 if (!client_id) {
                   resp->writeStatus("404 Not Found");
                   resp->end();
                 } else {
                   // This is the libraries default handler
                   std::string_view secWebSocketKey =
                       req->getHeader("sec-websocket-key");
                   std::string_view secWebSocketProtocol =
                       req->getHeader("sec-websocket-protocol");
                   std::string_view secWebSocketExtensions =
                       req->getHeader("sec-websocket-extensions");
                   size_t client_id_val = *client_id;
                   resp->upgrade<size_t>(std::move(client_id_val),
                                         secWebSocketKey, secWebSocketProtocol,
                                         secWebSocketExtensions, ctx);
                 }
               },
           .open =
               [this](HTTPWebSocket *ws) {
                 size_t client_id = *((size_t *)ws->getUserData());
                 _wss_handler->onClientConnected(client_id, WsConnection(ws));
               },
           .message =
               [this](HTTPWebSocket *ws, std::string_view message,
                      uWS::OpCode opCode) {
                 size_t client_id = *((size_t *)ws->getUserData());
                 std::optional<WsResponse> response =
                     _wss_handler->onMessage(client_id, ws, message);
                 if (response) {
                   ws->send(response->body, (uWS::OpCode)response->op_code,
                            true);
                 }
               },
           .drain =
               [](HTTPWebSocket *ws) {
                 /* Check ws->getBufferedAmount() here */
               },
           .close =
               [this](HTTPWebSocket *ws, int code, std::string_view message) {
                 size_t client_id = *((size_t *)ws->getUserData());
                 _wss_handler->onClientDisconnected(client_id);
               }

          })
      .listen(PORT,
              [](us_listen_socket_t *token) {
                if (!token) {
                  LOG_ERROR << "Unable to listen to web traffic on port "
                            << PORT << "." << std::endl;
                  exit(1);
                }
              })
      .run();
}

std::string HttpServer::guessMimeType(const std::string &path,
                                      const std::string &def) {
  size_t pos = path.rfind('.');
  if (pos == std::string::npos) {
    return def;
  }
  std::string ending = path.substr(pos + 1);

  if (ending == "js") {
    return "application/javascript";
  } else if (ending == "css") {
    return "text/css";
  } else if (ending == "png") {
    return "image/png";
  } else if (ending == "jpeg") {
    return "image/jpeg";
  } else if (ending.empty() || ending == "htm" || ending == "html") {
    return "text/html";
  } else if (ending == "svg") {
    return "image/svg+xml";
  } else if (ending == "json") {
    return "application/json";
  } else if (ending == "ogg") {
    return "audio/ogg";
  } else if (ending == "opus") {
    return "audio/opus";
  } else if (ending == "mp3") {
    return "audio/mpeg";
  } else if (ending == "wav") {
    return "audio/wav";
  } else {
    return "application/octet-stream";
  }
}

// HttpRequest
// =============================================================================

HttpServer::HttpRequest::HttpRequest(uWS::HttpRequest *msg) {
  // Read the method and headers
  // The body may not yet be available.
  path = std::string(msg->getUrl());
  if (msg->getMethod() == "get") {
    type = RequestType::GET;
  } else if (msg->getMethod() == "post") {
    type = RequestType::POST;
  } else {
    LOG_ERROR << "HttpServer::onMgEvent: Unknown message type "
              << msg->getMethod() << LOG_END;
  }

  for (const auto &header : *msg) {
    headers[std::string(header.first)] = std::string(header.second);
  }
}

std::string HttpServer::HttpRequest::getHeader(const std::string &name) const {
  auto it = headers.find(name);
  if (it != headers.end()) {
    return it->second;
  }
  return "";
}

std::string HttpServer::HttpRequest::getBodyString() const {
  return std::string(body, body_length);
}

// HttpResponse
// =============================================================================

void HttpServer::HttpResponse::setBody(const std::string &s) {
  body.resize(s.size());
  std::memcpy(body.data(), s.c_str(), s.size());
}

void HttpServer::HttpResponse::setError(int error_code,
                                        const std::string &msg) {
  status_code = error_code;
  setBody(msg);
}

void HttpServer::HttpResponse::setMimeType(const std::string &mime_type) {
  headers["Content-Type"] = mime_type;
}

void HttpServer::HttpResponse::fromFile(const std::string &path) {
  std::ifstream in(path);
  if (!in.is_open()) {
    setError(404, "Not Found");
  } else {
    status_code = 200;
    std::string mime_type = HttpServer::guessMimeType(path);
    setMimeType(mime_type);

    in.seekg(0, std::ios::end);
    size_t size = in.tellg();
    in.seekg(0, std::ios::beg);
    body.resize(size);
    in.read(body.data(), size);
  }
}

void HttpServer::HttpResponse::setHeader(const std::string &key,
                                         const std::string &value) {
  headers[key] = value;
}

HttpServer::WsConnection::WsConnection(HTTPWebSocket *socket)
    : _socket(socket) {}

void HttpServer::WsConnection::send(std::string_view data, WsOpCode opCode) {
  _socket->send(data, (uWS::OpCode)opCode, true);
}

const std::string HttpServer::AUTH_PAGE =
    R"(
<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8"/>
    <title>Authenticate</title>
    <style>
      body {
        color: white;
        background-color: #292929;
      }

      #center {
        margin: auto;
        text-align: center;
        width: 25%;
        top: 50%;
        left: 50%;
        position: absolute;
        transform: translate(-50%, -50%);
      }
    </style>
  </head>
  <body>
    <div id="center">
      <noscript>
        This website does not work without javascript enabled. If you attempt
        to login without javascript your password will not be hashed before
        transmission.
      </noscript>
      <p>
        This site uses cookies. By pressing Login you accept that a cookie
        called `auth` will be set for the duration of 1 year.
      </p>
      <form action="/auth" method="post" id="login-form">
        <div><input name="name" placeholder="username" type="text" id="input-name"></div>
        <div><input name="password" placeholder="password" type="password" id="input-password"></div>
        <div id="wrong-credentials" style="color: red; display: none;">The username or password is wrong.</div>
        <div><input type="submit" value="Login"></div>
      </form>
    </div>
    <script>
      (function() {
        let form = document.getElementById('login-form');
        let input_name = document.getElementById('input-name')
        let input_password = document.getElementById('input-password')
        let wrong_credentials = document.getElementById('wrong-credentials')

        form.addEventListener('submit', async (event) => {
          event.preventDefault();
          const name = input_name.value
          const password = input_password.value
          const password_utf8 = new TextEncoder('utf-8').encode(name + password)
          let password_hash_bytes = await crypto.subtle.digest('SHA-256', password_utf8)
          let password_array = Array.from(new Uint8Array(password_hash_bytes))
          let password_hex = password_array.map(x => ('00' + x.toString(16)).slice(-2)).join('').toUpperCase();
          var req = new XMLHttpRequest()
          req.open('POST', '/auth', true)
          req.setRequestHeader('Content-Type', 'application/json')
          req.onreadystatechange = () => {
            if (req.readyState != 4) {
              return
            }
            if (req.status == 200) {
              window.location = '/'
            } else {
              wrong_credentials.style.display = 'block'
              console.log('Unable to authenticate ' + req.status)
            }
          };
          req.send(JSON.stringify(
            {
              'name': name,
              'password': password_hex
            }
          ))
        });
      }());
    </script>
  </body>
</html>
)";
