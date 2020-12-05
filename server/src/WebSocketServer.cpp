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

#include "WebSocketServer.h"

#include <chrono>
#include <memory>
#include <thread>

#include "Logger.h"

WebSocketServer::WebSocketServer(std::shared_ptr<UserManager> authenticator,
                                 OnMsgHandler_t on_msg,
                                 OnConnectHandler_t on_connect,
                                 std::string base_dir)
    : user_manager(authenticator),
      _on_msg(on_msg),
      _on_connect(on_connect),
      _base_dir(base_dir),
      _next_connection_id(0) {}

std::optional<size_t> WebSocketServer::onClientHandshake(
    const HttpServer::HttpRequest &req) {
  try {
    // Authenticate the new user
    std::string cookies = req.getHeader("cookie");
    UserManager::UserPtr user = user_manager->authenticateViaCookies(cookies);
    if (user == nullptr) {
      LOG_DEBUG << "WebSocketServer::onClientHandshake: An unauthorized client "
                   "connected"
                << LOG_END;
      return std::nullopt;
    }
    _connection_users[_next_connection_id] = user;
    LOG_DEBUG
        << "WebSocketServer::onClientHandshake: Accepted a client handshake"
        << LOG_END;
    return _next_connection_id++;
  } catch (const std::exception &e) {
    LOG_ERROR << "Error while handling a new client: " << e.what() << LOG_END;
  } catch (...) {
    LOG_ERROR << "Unknown error while handling a new client." << LOG_END;
  }
  return std::nullopt;
}

void WebSocketServer::onClientConnected(size_t client_id,
                                        HttpServer::WsConnection conn) {
  try {
    UserManager::UserPtr user = _connection_users[client_id];
    _connections.push_back({client_id, conn});

    Response resp = _on_connect(user);
    handleResponse(resp, conn);
    LOG_DEBUG << "A new client named " << user->name() << " connected " << LOG_END;
  } catch (const std::exception &e) {
    LOG_ERROR << "Error while handling a new client: " << e.what() << LOG_END;
  } catch (...) {
    LOG_ERROR << "Unknown error while handling a new client." << LOG_END;
  }
}

void WebSocketServer::onClientDisconnected(size_t client_id) {
  LOG_DEBUG << "A client disconnected" << LOG_END;
  _connection_users.erase(client_id);

  for (size_t i = 0; i < _connections.size(); ++i) {
    if (_connections[i].first == client_id) {
      std::iter_swap(_connections.begin() + i, _connections.end() - 1);
      _connections.erase(_connections.end() - 1);
      break;
    }
  }
}

std::optional<HttpServer::WsResponse> WebSocketServer::onMessage(
    size_t client_id, HttpServer::WsConnection conn, std::string_view data) {
  try {
    UserManager::UserPtr user = _connection_users[client_id];
    if (user->isDeleted()) {
      // The user no longer exists
      return HttpServer::WsResponse{"Your account was deleted.",
                                    HttpServer::WsOpCode::CLOSE};
    }
    Response resp = _on_msg(data, user);
    if (resp.type == ResponseType::RETURN) {
      return HttpServer::WsResponse{resp.text, HttpServer::WsOpCode::TEXT};
    } else {
      if (resp.type == ResponseType::FORWARD) {
        resp.text = std::string(data);
      }
      handleResponse(resp, conn);
    }
  } catch (const std::exception &e) {
    LOG_ERROR << "Error while handling a message: " << e.what() << LOG_END;
  } catch (...) {
    LOG_ERROR << "Unknown error while handling a message." << LOG_END;
  }
  return std::nullopt;
}

void WebSocketServer::handleResponse(const Response &response,
                                     HttpServer::WsConnection &initiator) {
  switch (response.type) {
    case ResponseType::FORWARD:
    case ResponseType::BROADCAST: {
      broadcast(response.text);
    } break;
    case ResponseType::RETURN: {
      initiator.send(response.text, HttpServer::WsOpCode::TEXT);
    } break;
    case ResponseType::SILENCE:
      // Do nothing
      break;
  }
}

void WebSocketServer::broadcast(const std::string &data) {
  for (std::pair<size_t, HttpServer::WsConnection> &conn : _connections) {
    conn.second.send(data, HttpServer::WsOpCode::TEXT);
  }
}
