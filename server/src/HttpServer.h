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

#include <App.h>

#include <memory>
#include <regex>
#include <string>

#include "UserManager.h"

class HttpServer {
 public:
  typedef uWS::WebSocket<true, true> HTTPWebSocket;

  enum class RequestType { GET, POST };

  class HttpRequest {
   public:
    HttpRequest(){};
    HttpRequest(uWS::HttpRequest *msg);

    std::string getHeader(const std::string &name) const;
    std::string getBodyString() const;

    RequestType type;
    std::string path;
    std::unordered_map<std::string, std::string> headers;

    const char *body;
    size_t body_length;
  };

  class HttpResponse {
   public:
    void setBody(const std::string &s);
    void setError(int error_code, const std::string &msg);
    void setMimeType(const std::string &mime_type);
    void fromFile(const std::string &path);

    void setHeader(const std::string &key, const std::string &value);

    std::unordered_map<std::string, std::string> headers;

    std::vector<char> body;
    int status_code;
  };

  enum WsOpCode : unsigned char {
    TEXT = 1,
    BINARY = 2,
    CLOSE = 8,
    PING = 9,
    PONG = 10
  };

  struct WsResponse {
    std::string body;
    WsOpCode op_code;
  };

  struct WsConnection {
   public:
    WsConnection(HTTPWebSocket *socket);

    void send(std::string_view data, WsOpCode opCode);

   private:
    HTTPWebSocket *_socket;
  };

  class RequestHandler {
   public:
    virtual HttpResponse onRequest(UserManager::UserPtr user,
                                   const HttpRequest &req) = 0;
  };

  class WebSocketHandler {
   public:
    /**
     * @brief This callback can be used to deny connections. Return false if
     * the connection should be rejected.
     */
    virtual std::optional<size_t> onClientHandshake(const HttpRequest &req) = 0;
    virtual void onClientConnected(size_t client_id, WsConnection conn) = 0;
    virtual void onClientDisconnected(size_t client_id) = 0;
    virtual std::optional<WsResponse> onMessage(size_t client_id, WsConnection conn, std::string_view data) = 0;
  };

  HttpServer(std::shared_ptr<UserManager> authenticator,
             const std::string &base_dir);
  virtual ~HttpServer();

  void setWSSHandler(std::shared_ptr<WebSocketHandler> wss_handler);

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
  HttpResponse onGet(const HttpRequest &req);
  HttpResponse onPost(const HttpRequest &req);

  std::shared_ptr<WebSocketHandler> _wss_handler;

  std::vector<std::pair<std::regex, std::shared_ptr<RequestHandler>>>
      _get_request_handlers;
  std::vector<std::pair<std::regex, std::shared_ptr<RequestHandler>>>
      _post_request_handlers;
  std::shared_ptr<UserManager> user_manager;
  std::string _base_dir;
  std::string _serve_dir;

  struct mg_context *_ctx;

  std::string _cert_path;
  std::string _key_path;
  std::string _dh_param_path;
  const char *_err;

  static const std::string AUTH_PAGE;
};
