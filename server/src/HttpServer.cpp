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
                       const std::string &base_dir, bool do_keycheck)
    : user_manager(authenticator),
      _base_dir(base_dir),
      _do_keycheck(do_keycheck) {
  std::string cert_path = base_dir + "/cert/certificate.pem";
  std::string key_path = base_dir + "/cert/key.pem";
  _server =
      std::make_unique<httplib::SSLServer>(cert_path.c_str(), key_path.c_str());
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

void HttpServer::run() {
  std::string key = Random::secureRandomString(32);
  std::string basepath;
  if (_base_dir == ".") {
    _base_dir = os::getcwd();
  }
  basepath = _base_dir;
  basepath += "/html";
  basepath = os::realpath(basepath);
  if (!_do_keycheck) {
    LOG_INFO << "Http server keychecking is disabled" << LOG_END;
  } else {
    LOG_INFO << "The key is: " << key << LOG_END;
  }

  _server->Get(".*", [this, &key, &basepath](const httplib::Request &req,
                                             httplib::Response &resp) {
    try {
      std::string cookies = req.get_header_value("Cookie");
      if (_do_keycheck && req.path != "/auth") {
        if (!user_manager->authenticateViaCookies(cookies)) {
          if (req.path == "/") {
            resp.status = 307;
            resp.set_header("Location", "/auth");
            resp.body = "Not Authenticated.";
          } else {
            resp.status = 404;
            resp.body = "Page not found";
          }
          return;
        }
      }
      std::string realpath = req.path;

      // Call any registered request handlers
      for (std::pair<std::regex, std::shared_ptr<RequestHandler>> &r :
           _get_request_handlers) {
        if (std::regex_match(realpath, r.first)) {
          r.second->onRequest(req, resp);
          return;
        }
      }

      if (realpath == "/") {
        realpath = "/index.html";
      }
      // The auth page is not served from the filesystem
      if (realpath == "/auth") {
        if (!_do_keycheck || user_manager->authenticateViaCookies(cookies)) {
          resp.status = 307;
          resp.set_header("Location", "/");
          resp.body = "Authentication successfull.";
        } else {
          resp.status = 200;
          resp.body = AUTH_PAGE;
          resp.set_header("Content-Type", "text/html");
        }
        return;
      }
      {
        realpath = basepath + realpath;
        realpath = os::realpath(realpath);
      }
      LOG_DEBUG << "GET: " << realpath << LOG_END;

      if (realpath.substr(0, basepath.size()) != basepath) {
        LOG_WARN << "Got a request for a file outside the basepath " << realpath
                 << LOG_END;
        resp.body = "403 forbidden";
        resp.status = 403;
        return;
      }

      std::string mimetype = guessMimeType(realpath);
      LOG_DEBUG << realpath << " has mimetype " << mimetype << LOG_END;
      std::ifstream in(realpath);
      if (!in.is_open()) {
        LOG_WARN << "HttpServer::get : Got a request for " << realpath
                 << " but the file doesn't exist." << LOG_END;
        resp.body = "404 not found";
        resp.status = 404;
        return;
      }
      in.seekg(0, std::ios::end);
      size_t filesize = in.tellg();
      in.seekg(0, std::ios::beg);
      std::vector<char> buffer(filesize, '0');
      in.read(buffer.data(), buffer.size());

      resp.status = 200;
      resp.set_content(buffer.data(), buffer.size(), mimetype.c_str());
    } catch (const std::exception &e) {
      LOG_ERROR
          << "HttpServer::get An error occurred while handling a request for "
          << req.path << " " << e.what() << LOG_END;
    } catch (...) {
      LOG_ERROR << "HttpServer::get An unkndown error occurred while handling "
                   "a request for "
                << req.path << LOG_END;
    }
  });

  _server->Post(".*", [this](const httplib::Request &req,
                             httplib::Response &resp) {
    using nlohmann::json;
    try {
      std::string cookies = req.get_header_value("Cookie");
      if (req.path != "/auth") {
        if (_do_keycheck) {
          if (!user_manager->authenticateViaCookies(cookies)) {
            resp.status = 404;
            resp.body = "Page not found";
            return;
          }
        }
      } else {
        LOG_DEBUG << "Handling an authentification attempt" << LOG_END;
        // got an authentification attempt
        json data = json::parse(req.body);
        if (!data.contains("name") || !data.contains("password")) {
          resp.status = 404;
          resp.body = "Page not found";
          return;
        }
        std::string name = data["name"].get<std::string>();
        std::string password = data["password"].get<std::string>();
        LOG_DEBUG << "Trying to authenticate " << name << LOG_END;
        UserManager::UserPtr user =
            user_manager->authenticateViaLogin(name, password);
        if (user != nullptr) {
          LOG_DEBUG << "Authenticated the user" << LOG_END;
          resp.status = 200;
          resp.body = "Ok";
          resp.set_header("Set-Cookie", user->createSetCookieHeader());
          return;
        }
        LOG_DEBUG << "No user with that username and password" << LOG_END;
        resp.status = 404;
        resp.body = "Page not found";
        return;
      }

      std::string realpath = req.path;
      // Call any registered request handlers
      for (std::pair<std::regex, std::shared_ptr<RequestHandler>> &r :
           _post_request_handlers) {
        if (std::regex_match(realpath, r.first)) {
          r.second->onRequest(req, resp);
          return;
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
  });

  while (true) {
    try {
      LOG_INFO << "Starting the http server on 8082..." << LOG_END;
      _server->listen("0.0.0.0", 8082);
      std::this_thread::sleep_for(std::chrono::seconds(15));
    } catch (const std::exception &e) {
      LOG_ERROR << "Unable to listen on 0.0.0.0:8082 : " << e.what() << LOG_END;
      std::this_thread::sleep_for(std::chrono::seconds(15));
    }
  }
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
          console.log('raw', password)
          const password_utf8 = new TextEncoder('utf-8').encode(name + password)
          let password_hash_bytes = await crypto.subtle.digest('SHA-256', password_utf8)
          console.log('hashed', password_hash_bytes, ' ', password_hash_bytes.byteLength)
          let password_array = Array.from(new Uint8Array(password_hash_bytes))
          console.log('Uint8 length: ', password_array.length)
          let password_hex = password_array.map(x => ('00' + x.toString(16)).slice(-2)).join('').toUpperCase();
          console.log('hex: ', password_hex)
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
