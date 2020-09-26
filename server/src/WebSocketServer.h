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

#include <functional>
#include <string>
#include <vector>

#define ASIO_STANDALONE
#include <memory>
#include <websocketpp/config/asio.hpp>
#include <websocketpp/server.hpp>

#include "UserManager.h"

class WebSocketServer {
  typedef websocketpp::config::asio_tls ServerConfig;
  typedef websocketpp::server<websocketpp::config::asio_tls> Server;
  typedef std::shared_ptr<asio::ssl::context> ssl_ctx_pt;

 public:
  enum class ResponseType {
    BROADCAST,  // send new data to all
    FORWARD,    // send the received message to all
    RETURN,     // send to sender
    SILENCE
  };

  struct Response {
    std::string text;
    ResponseType type;
  };

  typedef std::function<Response(const std::string &, UserManager::UserPtr)>
      OnMsgHandler_t;
  typedef std::function<Response(UserManager::UserPtr)> OnConnectHandler_t;

 public:
  WebSocketServer(std::shared_ptr<UserManager> user_manager,
                  OnMsgHandler_t on_msg, OnConnectHandler_t on_connect,
                  std::string base_dir);

  void broadcast(const std::string &data);

 private:
  void run();

  void handleResponse(const Response &response,
                      websocketpp::connection_hdl &initiator);

  std::shared_ptr<UserManager> user_manager;
  std::vector<websocketpp::connection_hdl> _connections;

  Server _socket;

  OnMsgHandler_t _on_msg;
  OnConnectHandler_t _on_connect;

  std::string _base_dir;

  std::unordered_map<void*, UserManager::UserPtr> _connection_users;
};
