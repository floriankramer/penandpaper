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

#define CPPHTTPLIB_OPENSSL_SUPPORT
#include <httplib.h>
#include <linux/limits.h>
#include <unistd.h>

#include <chrono>
#include <cstdlib>
#include <ctime>
#include <fstream>
#include <thread>

#include "Logger.h"

HttpServer::HttpServer(bool do_keycheck) : _do_keycheck(do_keycheck) { run(); }

void HttpServer::run() {
  httplib::SSLServer server("./cert/certificate.pem", "./cert/key.pem");

  std::string key = genKey();
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

  server.Get(".*", [this, &key, &basepath](const httplib::Request &req,
                                           httplib::Response &resp) {
    std::string realpath = req.path;
    if (realpath == "/") {
      realpath = "/index.html";
    }
    if (realpath == "/index.html") {
      if (_do_keycheck &&
          (!req.has_param("key") || req.get_param_value("key") != key)) {
        resp.body = "Invalid or missing key";
        resp.status = 200;
        return;
      }
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
      LOG_INFO << "Stating the http server on 8082..." << LOG_END;
      server.listen("0.0.0.0", 8082);
    } catch (const std::exception &e) {
      LOG_ERROR << "Unable to listen on 0.0.0.0:8082 : " << e.what() << LOG_END;
      std::this_thread::sleep_for(std::chrono::seconds(15));
    }
  }
}

std::string HttpServer::genKey() {
  unsigned int seed = time(NULL);
  std::string key(32, ' ');
  for (size_t i = 0; i < 32; i++) {
    key[i] = 'A' + rand_r(&seed) % 25;
  }
  return key;
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
  } else if (ending.empty() || ending == "svg") {
    return "image/svg+xml";
  } else {
    return "application/octet-stream";
  }
}
