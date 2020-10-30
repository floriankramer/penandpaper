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
#include <mutex>
#include <string>
#include <vector>
#include <string_view>

#include "Doodad.h"
#include "IdGenerator.h"
#include "Packet.h"
#include "Player.h"
#include "PluginManager.h"
#include "Token.h"
#include "UserManager.h"
#include "WebSocketServer.h"
#include "building/BuildingManager.h"

class Simulation {
  using MemberMsgHandler_t = std::function<WebSocketServer::Response(
      const Packet &, UserManager::UserPtr)>;
  //      WebSocketServer::Response (Simulation::*)(const nlohmann::json &);

  struct Color {
    uint8_t r, g, b;
  };

 public:
  Simulation();

  void setWebSocketServer( std::shared_ptr<WebSocketServer> wss);
  void setPluginManager(PluginManager *pm);

  WebSocketServer::Response onNewClient(UserManager::UserPtr user);
  WebSocketServer::Response onMessage(std::string_view message,
                                      UserManager::UserPtr user);

  /**
   * @return  The player with the uid or nullptr if no such player exists.
   */
  Player *getPlayer(const std::string &uid);

  void sendChatToAll(const std::string &msg);

  bool checkPermissions(UserManager::UserPtr user, Permissions min_perms);

 private:
  void broadcastClients();

  WebSocketServer::Response onCreateToken(const Packet &j,
                                          UserManager::UserPtr user);
  WebSocketServer::Response onMoveToken(const Packet &j,
                                        UserManager::UserPtr user);
  WebSocketServer::Response onRenameToken(const Packet &j,
                                        UserManager::UserPtr user);
  WebSocketServer::Response onDeleteToken(const Packet &j,
                                          UserManager::UserPtr user);
  WebSocketServer::Response onChat(const Packet &j, UserManager::UserPtr user);
  WebSocketServer::Response onCreateDoodadLine(const Packet &j,
                                               UserManager::UserPtr user);
  WebSocketServer::Response onClearDoodads(const Packet &j,
                                           UserManager::UserPtr user);
  WebSocketServer::Response onClearTokens(const Packet &j,
                                          UserManager::UserPtr user);
  WebSocketServer::Response onTokenToggleFoe(const Packet &j,
                                             UserManager::UserPtr user);
  WebSocketServer::Response onInitSession(const Packet &j,
                                          UserManager::UserPtr user);
  WebSocketServer::Response onSetUsername(const Packet &j,
                                          UserManager::UserPtr user);

  Token *tokenById(uint64_t id);

  std::vector<std::string> splitWs(const std::string &s);

  std::string cmdRollDice(const std::string &who,
                          const std::vector<std::string> &cmd);
  std::string cmdSetname(const std::string &who, const std::string &uid,
                         const std::vector<std::string> &cmd);
  std::string cmdSetAttributes(const std::string &who, const std::string &uid,
                               const std::vector<std::string> &cmd);
  std::string cmdHelp(const std::string &who, const std::string &uid,
                      const std::vector<std::string> &cmd);
  std::string cmdAudio(const std::string &who, const std::string &uid,
                       const std::vector<std::string> &cmd);

  Color nextColor();

  size_t _next_color;

  unsigned int _rand_seed;

  std::vector<Token> _tokens;
  std::vector<DoodadLine> _doodad_lines;

  std::vector<Player> _players;

  std::mutex _simulation_mutex;

  std::string _tiles_path;

  std::unordered_map<std::string, MemberMsgHandler_t> _msg_handlers;
  std::unordered_map<std::string, MemberMsgHandler_t> _msg_handlers_building;

   std::shared_ptr<WebSocketServer> _web_socket_server;

  BuildingManager _building_manager;
  IdGenerator _id_generator;

  PluginManager *_plugin_manager;

  static const int NUM_COLORS = 11;
  static const Color COLORS[NUM_COLORS];
};
