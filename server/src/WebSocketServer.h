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
#include <memory>
#include <string>
#include <vector>

#include "HttpServer.h"
#include "UserManager.h"

class WebSocketServer : public HttpServer::WebSocketHandler {
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

  typedef std::function<Response(std::string_view , UserManager::UserPtr)>
      OnMsgHandler_t;
  typedef std::function<Response(UserManager::UserPtr)> OnConnectHandler_t;

 public:
  WebSocketServer(std::shared_ptr<UserManager> user_manager,
                  OnMsgHandler_t on_msg, OnConnectHandler_t on_connect,
                  std::string base_dir);

  void broadcast(const std::string &data);

  virtual std::optional<size_t> onClientHandshake(
      const HttpServer::HttpRequest &req) override;
  virtual void onClientConnected(size_t client_id,
                                 HttpServer::WsConnection conn) override;
  virtual void onClientDisconnected(size_t client_id) override;
  virtual std::optional<HttpServer::WsResponse> onMessage(size_t client_id,
                                           HttpServer::WsConnection conn,
                                           std::string_view data) override;

 private:
  void handleResponse(const Response &response,
                      HttpServer::WsConnection &initiator);

  std::shared_ptr<UserManager> user_manager;
  std::vector<std::pair<size_t, HttpServer::WsConnection>> _connections;

  OnMsgHandler_t _on_msg;
  OnConnectHandler_t _on_connect;

  std::string _base_dir;

  std::unordered_map<size_t, UserManager::UserPtr> _connection_users;
  size_t _next_connection_id;
};
