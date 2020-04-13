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

#include <linux/limits.h>
#include <unistd.h>

#include <chrono>
#include <cstdlib>
#include <ctime>
#include <fstream>
#include <thread>

#include "Logger.h"
#include "Random.h"

HttpServer::HttpServer(std::shared_ptr<Authenticator> authenticator,
                       bool do_keycheck)
    : _authenticator(authenticator),
      _do_keycheck(do_keycheck),
      _server("./cert/certificate.pem", "./cert/key.pem") {}

void HttpServer::registerRequestHandler(
    const std::string &path, RequestType type,
    std::shared_ptr<RequestHandler> handler) {
  httplib::Server::Handler callback =
      std::bind(&RequestHandler::onRequest, handler.get(),
                std::placeholders::_1, std::placeholders::_2);
  if (type == RequestType::GET) {
    _server.Get(path.c_str(), callback);
  } else {
    _server.Post(path.c_str(), callback);
  }
}

void HttpServer::run() {
  std::string key = Random::secureRandomString(32);
  std::string basepath;
  {
    char buffer[PATH_MAX];
    getcwd(buffer, PATH_MAX);
    basepath = buffer;
  }
  basepath += "/html";
  {
    char buffer[PATH_MAX];
    realpath(basepath.c_str(), buffer);
    basepath = buffer;
  }
  if (!_do_keycheck) {
    LOG_INFO << "Http server keychecking is disabled" << LOG_END;
  } else {
    LOG_INFO << "The key is: " << key << LOG_END;
  }

  _server.Get(".*", [this, &key, &basepath](const httplib::Request &req,
                                            httplib::Response &resp) {
    std::string cookies = req.get_header_value("Cookie");
    if (_do_keycheck && req.path != "/auth") {
      if (!_authenticator->authenticateFromCookies(cookies)) {
        resp.status = 404;
        resp.body = "Page not found";
        return;
      }
    }
    std::string realpath = req.path;
    if (realpath == "/") {
      realpath = "/index.html";
    }
    // The auth page is not served from the filesystem
    if (realpath == "/auth") {
      if (!_do_keycheck || _authenticator->authenticateFromCookies(cookies)) {
        resp.status = 307;
        resp.set_header("Location", "/");
        resp.body = "Authentication is disabled.";
      } else {
        if (req.has_param("cookie_consent") &&
            req.get_param_value("cookie_consent") == "yes") {
          if (req.has_param("key") && req.get_param_value("key") == key) {
            std::string token = _authenticator->addAuthenticated("unknown");
            resp.status = 307;
            resp.set_header("Location", "/");
            resp.set_header("Set-Cookie",
                            _authenticator->createSetCookieHeader(token));
            resp.body = "Authentication successfull";
          } else {
            resp.status = 401;
            resp.body = "Missing or invalid key";
          }
        } else {
          // assemble the auth link
          std::vector<char> page;
          page.resize(AUTH_PAGE.size() - 2 + key.size() + 1, ' ');
          if (req.has_param("key")) {
            // copy the current key into the template
            sprintf(page.data(), AUTH_PAGE.data(), key.data());
          } else {
            sprintf(page.data(), AUTH_PAGE.data(), "not_provided");
          }
          resp.status = 200;
          resp.body = page.data();
          resp.set_header("Content-Type", "text/html");
        }
      }
      return;
    }
    {
      realpath = basepath + realpath;
      char buffer[PATH_MAX];
      ::realpath(realpath.c_str(), buffer);
      realpath = buffer;
    }
    LOG_DEBUG << "GET: " << realpath << LOG_END;

    if (realpath.substr(0, basepath.size()) != basepath) {
      LOG_DEBUG << "Forbidden" << LOG_END;
      resp.body = "403 forbidden";
      resp.status = 403;
      return;
    }

    std::string mimetype = guessMimeType(realpath);
    LOG_DEBUG << realpath << " has mimetype " << mimetype << LOG_END;
    std::ifstream in(realpath);
    if (!in.is_open()) {
      LOG_DEBUG << "Not found" << LOG_END;
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
  });

  while (true) {
    try {
      LOG_INFO << "Starting the http server on 8082..." << LOG_END;
      _server.listen("0.0.0.0", 8082);
      std::this_thread::sleep_for(std::chrono::seconds(15));
    } catch (const std::exception &e) {
      LOG_ERROR << "Unable to listen on 0.0.0.0:8082 : " << e.what() << LOG_END;
      std::this_thread::sleep_for(std::chrono::seconds(15));
    }
  }
}

std::string HttpServer::guessMimeType(const std::string &path) {
  size_t pos = path.rfind('.');
  if (pos == std::string::npos) {
    return "text/html";
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
  } else {
    return "application/octet-stream";
  }
}

const std::string HttpServer::AUTH_PAGE = R"(
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
      <p>
        This site uses cookies. By pressing accept you accept that a cookie called `auth` will be set for the duration of 1 year.
      </p>
      <a href="?cookie_consent=yes&key=%s">Accept</a>
  </div>
  </body>
</html>

)";
