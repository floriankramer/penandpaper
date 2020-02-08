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

#include <string>
#include <vector>
#include <mutex>

#include "Doodad.h"
#include "Token.h"
#include "WebSocketServer.h"

class Simulation {

  using MemberMsgHandler_t =
      WebSocketServer::Response (Simulation::*)(const nlohmann::json &);

  enum class Permissions { PLAYER, GAMEMASTER };

  struct Player {
    size_t id;
    std::string uid;
    Permissions permissions;
    std::string name;
  };

  struct Color {
    uint8_t r, g, b;
  };

 public:
  Simulation();

  WebSocketServer::Response onNewClient();
  WebSocketServer::Response onMessage(const std::string &msg);

 private:

  WebSocketServer::Response onCreateToken(const nlohmann::json &j);
  WebSocketServer::Response onMoveToken(const nlohmann::json &j);
  WebSocketServer::Response onDeleteToken(const nlohmann::json &j);
  WebSocketServer::Response onChat(const nlohmann::json &j);
  WebSocketServer::Response onCreateDoodadLine(const nlohmann::json &j);
  WebSocketServer::Response onClearDoodads(const nlohmann::json &j);
  WebSocketServer::Response onClearTokens(const nlohmann::json &j);
  WebSocketServer::Response onTokenToggleFoe(const nlohmann::json &j);
  WebSocketServer::Response onInitSession(const nlohmann::json &j);
  WebSocketServer::Response onSetUsername(const nlohmann::json &j);
  WebSocketServer::Response onSetBuilding(const nlohmann::json &j);
  WebSocketServer::Response onToggleDoor(const nlohmann::json &j);

  Token *tokenById(uint64_t id);

  std::vector<std::string> splitWs(const std::string &s);

  std::string cmdRollDice(const std::string &who, const std::vector<std::string> &cmd);
  std::string cmdSetname(const std::string &who, const std::string &uid, const std::vector<std::string> &cmd);
  std::string cmdHelp(const std::string &who, const std::string &uid, const std::vector<std::string> &cmd);

  /**
   * @return  The player with the uid or nullptr if no such player exists.
   */
  Player *getPlayer(const std::string &uid);

  bool checkPermissions(const nlohmann::json &packet, Permissions min_perm);

  Color nextColor();

  size_t _next_id;
  size_t _next_color;

  unsigned int _rand_seed;

  std::vector<Token> _tokens;
  std::vector<DoodadLine> _doodad_lines;

  std::vector<Player> _players;

  nlohmann::json _building_json;

  std::mutex _simulation_mutex;

  static const int NUM_COLORS = 11;
  static const Color COLORS[NUM_COLORS];

  static const std::unordered_map<std::string, MemberMsgHandler_t> MSG_HANDLERS;
};
