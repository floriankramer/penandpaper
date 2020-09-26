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

#pragma once

#include <memory>
#include <string>
#define CPPHTTPLIB_OPENSSL_SUPPORT
#include <httplib.h>

#include <regex>

#include "UserManager.h"

class HttpServer {
 public:
  enum class RequestType { GET, POST };
  class RequestHandler {
   public:
    virtual void onRequest(const httplib::Request &req,
                           httplib::Response &resp) = 0;
  };

  HttpServer(std::shared_ptr<UserManager> authenticator,
             const std::string &base_dir);

  void registerRequestHandler(const std::string &path, RequestType type,
                              std::shared_ptr<RequestHandler> handler);

  void run();

  /**
   * @brief Tries to guess the mime type based upon the file extension. Returns
   * def if the file does not have an extension. Returns
   * "application/octet-stream" for an unknown file ending.
   **/
  static std::string guessMimeType(const std::string &path,
                                   const std::string &def = "text/html");

 private:
  std::vector<std::pair<std::regex, std::shared_ptr<RequestHandler>>>
      _get_request_handlers;
  std::vector<std::pair<std::regex, std::shared_ptr<RequestHandler>>>
      _post_request_handlers;
  std::shared_ptr<UserManager> user_manager;
  std::unique_ptr<httplib::SSLServer> _server;
  std::string _base_dir;

  static const std::string AUTH_PAGE;
};
