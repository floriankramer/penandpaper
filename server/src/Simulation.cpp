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

#ifdef WIN32
#define _CRT_RAND_S
#endif
#include "Simulation.h"

#include <cstdlib>
#include <unordered_map>

#include "Logger.h"

const Simulation::Color Simulation::COLORS[Simulation::NUM_COLORS] = {
    {240, 50, 50},   // red
    {176, 30, 90},   // burgund
    {201, 20, 201},  // pink
    {120, 61, 196},  // purple
    {24, 100, 171},  // blue
    {24, 172, 171},  // turquoise
    {8, 127, 91},    // blue-green
    {92, 148, 13},   // red-green
    {217, 72, 15},   // orange
    {129, 96, 65},   // brown
    {201, 201, 30}   // yellow
};

Simulation::Simulation()
    : _next_color(0),
      _building_manager(&_id_generator),
      _web_socket_server(nullptr),
      _plugin_manager(nullptr) {
  _rand_seed = time(NULL);
  using std::placeholders::_1;
  using std::placeholders::_2;
  _msg_handlers = {
      {"CreateToken", std::bind(&Simulation::onCreateToken, this, _1, _2)},
      {"MoveToken", std::bind(&Simulation::onMoveToken, this, _1, _2)},
      {"DeleteToken", std::bind(&Simulation::onDeleteToken, this, _1, _2)},
      {"Chat", std::bind(&Simulation::onChat, this, _1, _2)},
      {"CreateDoodadLine",
       std::bind(&Simulation::onCreateDoodadLine, this, _1, _2)},
      {"ClearDoodads", std::bind(&Simulation::onClearDoodads, this, _1, _2)},
      {"ClearTokens", std::bind(&Simulation::onClearTokens, this, _1, _2)},
      {"TokenToggleFoe",
       std::bind(&Simulation::onTokenToggleFoe, this, _1, _2)},
      {"InitSession", std::bind(&Simulation::onInitSession, this, _1, _2)},
      {"SetUsername", std::bind(&Simulation::onSetUsername, this, _1, _2)},
      {"RenameToken", std::bind(&Simulation::onRenameToken, this, _1, _2)}};
  _building_manager.registerPackets(&_msg_handlers_building);
}

void Simulation::setWebSocketServer(std::shared_ptr<WebSocketServer> wss) {
  _web_socket_server = wss;
}

void Simulation::setPluginManager(PluginManager *pm) {
  _plugin_manager = pm;
  _plugin_manager->setWriteToChat(
      std::bind(&Simulation::sendChatToAll, this, std::placeholders::_1));
  _plugin_manager->setBroadcastPacket(std::bind(
      &WebSocketServer::broadcast, _web_socket_server, std::placeholders::_1));
}

Token *Simulation::tokenById(uint64_t id) {
  for (Token &t : _tokens) {
    if (t.id() == id) {
      return &t;
    }
  }
  return nullptr;
}

WebSocketServer::Response Simulation::onNewClient(UserManager::UserPtr user) {
  std::lock_guard<std::mutex> simulation_mutex_lock(_simulation_mutex);

  using nlohmann::json;
  std::string answer_str;
  try {
    // Assemble an init package
    json answer;
    answer["type"] = "Init";
    json data;

    std::vector<json> _encoded_tokens;
    for (Token &t : _tokens) {
      _encoded_tokens.emplace_back(t.serialize());
    }
    data["tokens"] = _encoded_tokens;

    std::vector<json> _encoded_doodads;
    for (DoodadLine &d : _doodad_lines) {
      _encoded_doodads.emplace_back(d.serialize());
    }
    data["doodads"] = _encoded_doodads;

    data["building"] = _building_manager.toJson();

    data["nextColor"] = _next_color;

    data["tiles"] = _tiles_path;

    data["plugins"] = _plugin_manager->pluginNames();

    answer["data"] = data;
    answer_str = answer.dump();
  } catch (const std::exception &e) {
    LOG_ERROR << "Unable to assemble an init packet for the new client: "
              << e.what() << LOG_END;
  }

  return {answer_str, WebSocketServer::ResponseType::RETURN};
}

bool Simulation::checkPermissions(UserManager::UserPtr user,
                                  Permissions min_perms) {
  Player *p = getPlayer(user->uid());
  if (p != nullptr) {
    return p->permissions == min_perms;
  }
  return false;
}

void Simulation::broadcastClients() {
  using nlohmann::json;
  if (_web_socket_server == nullptr) {
    return;
  }
  json r;
  r["type"] = "PlayerList";
  json data;
  for (const Player &p : _players) {
    json player;
    player["name"] = p.name;
    player["permissions"] = int(p.permissions);
    player["uid"] = p.uid;
    data.push_back(player);
  }
  r["data"] = data;
  _web_socket_server->broadcast(r.dump());
}

WebSocketServer::Response Simulation::onMessage(std::string_view msg,
                                                UserManager::UserPtr user) {
  std::lock_guard<std::mutex> simulation_mutex_lock(_simulation_mutex);
  using nlohmann::json;
  try {
    json j = json::parse(msg);
    std::string type = j.at("type");
    LOG_DEBUG << "Received a message of type " << type << " : " << j.dump()
              << LOG_END;
    std::unordered_map<std::string, MemberMsgHandler_t>::const_iterator
        handler_it = _msg_handlers.find(type);
    if (handler_it != _msg_handlers.end()) {
      // call the handler
      return (handler_it->second)(Packet(j), user);
    } else if ((handler_it = _msg_handlers_building.find(type)) !=
               _msg_handlers_building.end()) {
      Packet packet(j);
      // All building packages require gm permissions
      if (!checkPermissions(user, Permissions::GAMEMASTER)) {
        return packet.makeMissingPermissionsResponse();
      }
      return (handler_it->second)(packet, user);
    } else if (_plugin_manager->hasPacketHandler(type)) {
      // Let the plugin handle the packet
      std::pair<WebSocketServer::ResponseType, std::string> p =
          _plugin_manager->handlePacket(type, j);
      WebSocketServer::Response r;
      r.type = p.first;
      r.text = p.second;
      return r;
    }

    WebSocketServer::Response r;
    r.type = WebSocketServer::ResponseType::RETURN;
    r.text = "Unknown message type " + type;
    LOG_WARN << "Received a message of unknown type " << type << LOG_END;
    return r;
  } catch (const std::exception &e) {
    LOG_ERROR << "Error when handling a msg from a client " << e.what() << "\n"
              << msg << LOG_END;
  }

  WebSocketServer::Response r;
  r.type = WebSocketServer::ResponseType::RETURN;
  r.text = "Error handling a message.";
  return r;
}

Simulation::Color Simulation::nextColor() {
  Color c = COLORS[_next_color];
  _next_color++;
  _next_color %= NUM_COLORS;
  return c;
}

WebSocketServer::Response Simulation::onCreateToken(const Packet &j,
                                                    UserManager::UserPtr user) {
  if (!checkPermissions(user, Permissions::GAMEMASTER)) {
    return j.makeMissingPermissionsResponse();
  }
  _tokens.emplace_back();
  float x = j.json().at("data").at("x");
  float y = j.json().at("data").at("y");
  Token &t = _tokens.back();
  t.x() = x;
  t.y() = y;
  t.id() = _id_generator();
  t.radius() = 0.25;
  t.is_enemy() = false;
  Color c = nextColor();
  t.r() = c.r;
  t.g() = c.g;
  t.b() = c.b;

  nlohmann::json response;
  response["type"] = "CreateToken";
  response["data"] = t.serialize();

  return {response.dump(), WebSocketServer::ResponseType::BROADCAST};
}

WebSocketServer::Response Simulation::onMoveToken(const Packet &j,
                                                  UserManager::UserPtr user) {
  nlohmann::json data = j.json().at("data");
  uint64_t id = data.at("id").get<uint64_t>();
  Token *t = tokenById(id);
  if (t != nullptr) {
    t->x() = data.at("x").get<float>();
    t->y() = data.at("y").get<float>();
    t->rotation() = data.at("rotation").get<float>();
  }
  return {"", WebSocketServer::ResponseType::FORWARD};
}

WebSocketServer::Response Simulation::onRenameToken(const Packet &j,
                                                    UserManager::UserPtr user) {
  if (!checkPermissions(user, Permissions::GAMEMASTER)) {
    return j.makeMissingPermissionsResponse();
  }

  nlohmann::json data = j.json().at("data");
  uint64_t id = data.at("id").get<uint64_t>();
  Token *t = tokenById(id);
  if (t != nullptr) {
    t->name() = data.at("new_name").get<std::string>();
  } else {
    LOG_WARN
        << "Received a rename request from the gm for an unknown token with id "
        << id << std::endl;
  }
  return {"", WebSocketServer::ResponseType::FORWARD};
}

WebSocketServer::Response Simulation::onDeleteToken(const Packet &j,
                                                    UserManager::UserPtr user) {
  if (!checkPermissions(user, Permissions::GAMEMASTER)) {
    return j.makeMissingPermissionsResponse();
  }
  bool did_delete = false;
  nlohmann::json data = j.json().at("data");
  uint64_t id = data.at("id").get<uint64_t>();
  for (size_t i = 0; i < _tokens.size(); i++) {
    if (_tokens[i].id() == id) {
      LOG_DEBUG << "Deleted token with id " << id << LOG_END;
      std::iter_swap(_tokens.begin() + i,
                     _tokens.begin() + (_tokens.size() - 1));
      _tokens.erase(_tokens.end());
      did_delete = true;
      break;
    }
  }
  if (!did_delete) {
    LOG_WARN << "A client requested deletion of token " << id
             << " but no token with that id exists." << LOG_END;
  }
  return {"", WebSocketServer::ResponseType::FORWARD};
}

WebSocketServer::Response Simulation::onChat(const Packet &j,
                                             UserManager::UserPtr user) {
  using nlohmann::json;
  std::string uid = user->uid();
  std::string msg = j.json().at("data").at("message");
  std::string sender = "unknown";

  Player *player = getPlayer(uid);
  if (player) {
    sender = player->name;
  }
  if (msg[0] == '/') {
    WebSocketServer::Response resp;
    json resp_json = j.json();
    resp_json["data"]["sender"] = "The Server";
    resp.type = WebSocketServer::ResponseType::BROADCAST;

    // The message is a command
    std::vector<std::string> parts = splitWs(msg);

    bool handled_by_plugin = false;
    if (_plugin_manager != nullptr) {
      std::string cmd = parts[0].substr(1);
      if (_plugin_manager->hasCommand(cmd)) {
        handled_by_plugin = true;
        std::pair<WebSocketServer::ResponseType, std::string> plugin_response =
            _plugin_manager->handleCommand(parts);
        resp_json["data"]["message"] = plugin_response.second;
        resp.type = plugin_response.first;
      }
    }

    if (!handled_by_plugin) {
      const std::string &cmd = parts[0];
      if (cmd == "/roll") {
        resp_json["data"]["message"] = cmdRollDice(sender, parts);
      } else if (cmd == "/rollp") {
        resp_json["data"]["message"] = cmdRollDice("You", parts);
        resp.type = WebSocketServer::ResponseType::RETURN;
      } else if (cmd == "/setname") {
        resp_json["data"]["message"] = cmdSetname(sender, uid, parts);
      } else if (cmd == "/help") {
        resp_json["data"]["message"] = cmdHelp(sender, uid, parts);
        resp.type = WebSocketServer::ResponseType::RETURN;
      } else if (cmd == "/audio") {
        resp_json["data"]["message"] = cmdAudio(sender, uid, parts);
        resp.type = WebSocketServer::ResponseType::RETURN;
      } else if (cmd == "/gm") {
        if (player) {
          player->permissions = Permissions::GAMEMASTER;
        }
        resp.type = WebSocketServer::ResponseType::SILENCE;
      } else if (cmd == "/settiles") {
        if (!checkPermissions(user, Permissions::GAMEMASTER)) {
          return j.makeMissingPermissionsResponse();
        }
        if (parts.size() == 2) {
          const std::string &path = parts[1];
          _tiles_path = path;
          json r;
          r["type"] = "SetTiles";
          r["data"]["path"] = path;
          return {r.dump(), WebSocketServer::ResponseType::BROADCAST};
        }
        resp_json["data"]["message"] = "Expected exactly one argument: <path>";
        resp.type = WebSocketServer::ResponseType::RETURN;
      } else if (cmd == "/cleartiles") {
        if (!checkPermissions(user, Permissions::GAMEMASTER)) {
          return j.makeMissingPermissionsResponse();
        }
        _tiles_path = "";
        json r;
        r["type"] = "ClearTiles";
        r["data"] = {};
        return {r.dump(), WebSocketServer::ResponseType::BROADCAST};
      } else {
        resp_json["data"]["message"] = "Unknown command '" + parts[0] + "'";
        resp.type = WebSocketServer::ResponseType::RETURN;
      }
    }

    if (resp.type == WebSocketServer::ResponseType::RETURN) {
      resp_json["data"]["sender"] = "The Server to you";
    }
    resp.text = resp_json.dump();
    return resp;
  } else if (msg.empty()) {
    return {"", WebSocketServer::ResponseType::SILENCE};
  } else {
    json modified = j.json();
    modified["data"]["sender"] = sender;
    return {modified.dump(), WebSocketServer::ResponseType::BROADCAST};
  }
}

WebSocketServer::Response Simulation::onCreateDoodadLine(
    const Packet &j, UserManager::UserPtr user) {
  using nlohmann::json;
  if (!checkPermissions(user, Permissions::GAMEMASTER)) {
    return j.makeMissingPermissionsResponse();
  }
  _doodad_lines.emplace_back(&_id_generator);
  DoodadLine &d = _doodad_lines.back();
  d.deserialize(j.json().at("data"));

  json response;
  response["type"] = "CreateDoodadLine";
  response["data"] = d.serialize();

  return {response.dump(), WebSocketServer::ResponseType::BROADCAST};
}

WebSocketServer::Response Simulation::onClearDoodads(
    const Packet &j, UserManager::UserPtr user) {
  if (!checkPermissions(user, Permissions::GAMEMASTER)) {
    return j.makeMissingPermissionsResponse();
  }
  _doodad_lines.clear();
  return {"", WebSocketServer::ResponseType::FORWARD};
}

WebSocketServer::Response Simulation::onClearTokens(const Packet &j,
                                                    UserManager::UserPtr user) {
  if (!checkPermissions(user, Permissions::GAMEMASTER)) {
    return j.makeMissingPermissionsResponse();
  }
  _tokens.clear();
  return {"", WebSocketServer::ResponseType::FORWARD};
}

WebSocketServer::Response Simulation::onTokenToggleFoe(
    const Packet &j, UserManager::UserPtr user) {
  if (!checkPermissions(user, Permissions::GAMEMASTER)) {
    return j.makeMissingPermissionsResponse();
  }
  nlohmann::json data = j.json().at("data");
  uint64_t id = data.at("id").get<uint64_t>();
  Token *t = tokenById(id);
  if (t != nullptr) {
    t->is_enemy() = !t->is_enemy();
  }

  return {"", WebSocketServer::ResponseType::FORWARD};
}

WebSocketServer::Response Simulation::onInitSession(const Packet &j,
                                                    UserManager::UserPtr user) {
  using nlohmann::json;
  json req_data = j.json().at("data");
  std::string uid = user->uid();
  Player *player = getPlayer(uid);

  if (player == nullptr) {
    LOG_INFO << "A new player with uid " << uid << " connected." << LOG_END;
    // A new player connected
    _players.emplace_back();
    player = &_players.back();
    player->uid = uid;
    player->id = _players.size() - 1;
    player->permissions =
        _players.size() == 1 ? Permissions::GAMEMASTER : Permissions::PLAYER;
    player->name = user->name();
  } else {
    LOG_INFO << "A player with uid " << uid << " and name " << player->name
             << " reconnected." << LOG_END;
  }
  broadcastClients();

  // encode the player
  json response;
  response["type"] = "Session";

  json resp_data;
  resp_data["id"] = player->id;
  resp_data["name"] = player->name;
  resp_data["permissions"] = (int)player->permissions;

  response["data"] = resp_data;

  return {response.dump(), WebSocketServer::ResponseType::RETURN};
}

WebSocketServer::Response Simulation::onSetUsername(const Packet &j,
                                                    UserManager::UserPtr user) {
  std::string uid = user->uid();
  std::string newname = j.json().at("data").at("name");
  if (newname == "The Server") {
    newname = "not The Server";
  }

  LOG_INFO << "The player with uid " << uid << " is now called " << newname
           << LOG_END;

  Player *p = getPlayer(uid);
  if (p) {
    p->name = newname;
  }
  broadcastClients();
  return {"", WebSocketServer::ResponseType::SILENCE};
}

std::string Simulation::cmdRollDice(const std::string &who,
                                    const std::vector<std::string> &cmd) {
  using nlohmann::json;
  // Trigger a roll dice sound
  json packet;
  packet["type"] = "PlayAudio";
  json data;
  unsigned int seed = time(NULL);
  int sound_variant = (rand_r(&seed) % 4) + 1;
  data["src"] = "/audio/ui/dice_" + std::to_string(sound_variant) + ".ogg";
  packet["data"] = data;
  _web_socket_server->broadcast(packet.dump());

  // Roll the dice
  std::ostringstream out;
  if (cmd.size() == 1) {
    out << who << " tried to roll an empty hand of dice.";
  } else {
    out << who << " rolled ";
    std::vector<int> die;
    die.reserve(cmd.size() - 1);
    for (size_t i = 1; i < cmd.size(); i++) {
      try {
        int faces = std::max(1, std::stoi(cmd[i]));
        die.push_back(faces);
        int val;
#ifndef WIN32
        val = (rand_r(&_rand_seed) % faces) + 1;
#else
        unsigned int v;
        rand_s(&v);
        val = (v % faces) + 1;
#endif
        out << val << " ";
      } catch (const std::exception &e) {
        out << "- ";
      }
    }
    out << "with dice ";
    for (size_t i = 0; i < die.size(); i++) {
      out << die[i];
      if (i + 1 < die.size()) {
        out << " ";
      }
    }
    out << ".";
  }
  return out.str();
}

std::string Simulation::cmdSetname(const std::string &who,
                                   const std::string &uid,
                                   const std::vector<std::string> &cmd) {
  std::stringstream s;
  for (size_t i = 1; i < cmd.size(); ++i) {
    s << cmd[i];
    if (i + 1 < cmd.size()) {
      s << " ";
    }
  }
  std::string newname = s.str();
  if (newname.size() > 0 && newname != "The Server") {
    Player *p = getPlayer(uid);
    if (p) {
      p->name = newname;
    }
    broadcastClients();
    return who + " is now called " + newname;
  } else {
    return "'" + newname + " is an invalid new name for " + who;
  }
}

std::string Simulation::cmdHelp(const std::string &who, const std::string &uid,
                                const std::vector<std::string> &cmd) {
  std::stringstream s;
  s << "Commands: <br/>";
  s << "/roll <die> ... - Roll dice with a public result<br/>";
  s << "/rollp <die> ... - Roll dice with a private result<br/>";
  s << "/setname <newname> - Change your username.<br/>";

  Player *p = getPlayer(uid);
  if (p != nullptr && p->permissions == Permissions::GAMEMASTER) {
    s << "<br/>GM Commands:<br/>";
    s << "/settiles <path> - Loads a tile map.<br/>";
    s << "/cleartiles <path> - Clears a tile map.<br/>";
    s << "/audio <play|stream|stopstreams> [src] - Control the audio "
         "server.<br/>";
  }

  return s.str();
}

std::string Simulation::cmdAudio(const std::string &who, const std::string &uid,
                                 const std::vector<std::string> &cmd) {
  using nlohmann::json;
  if (cmd.size() < 2) {
  }
  if (cmd[1] == "play") {
    if (cmd.size() != 3 && cmd.size() != 4) {
      return "Expected a command format of /audio play <src> [volume 0-1]";
    }
    const std::string &src = cmd[2];
    json packet;
    packet["type"] = "PlayAudio";
    json data;
    data["src"] = src;
    if (cmd.size() == 4) {
      data["volume"] = std::stof(cmd[3]);
    }
    packet["data"] = data;
    _web_socket_server->broadcast(packet.dump());
    return "Playing " + src;
  } else if (cmd[1] == "stream") {
    if (cmd.size() != 3 && cmd.size() != 4) {
      return "Expected a command format of /audio stream <src> [volume 0-1]";
    }
    const std::string &src = cmd[2];
    json packet;
    packet["type"] = "StreamAudio";
    json data;
    data["src"] = src;
    if (cmd.size() == 4) {
      data["volume"] = std::stof(cmd[3]);
    }
    packet["data"] = data;
    _web_socket_server->broadcast(packet.dump());
    return "Streaming " + src;
  } else if (cmd[1] == "stopstreams") {
    json packet;
    packet["type"] = "StopAudioStreams";
    json data;
    packet["data"] = data;
    _web_socket_server->broadcast(packet.dump());
    return "Stopped all audio streams";
  } else {
    return "Expected one of play|stream|stopstreams.";
  }
}

std::vector<std::string> Simulation::splitWs(const std::string &s) {
  std::vector<std::string> parts;
  size_t begin = 0, end = 0;
  // Skip any leading whitespace
  while (begin < s.size() && std::isspace(s[begin])) {
    begin++;
  }

  // split the string
  while (begin < s.size()) {
    // At this point s[begin] is not whitespace
    // Search for the next whitespace
    end = begin + 1;
    while (end < s.size() && !std::isspace(s[end])) {
      end++;
    }

    // s[begin:end] is now the longest whitespace free substring starting at
    // begin
    parts.emplace_back(s.substr(begin, end - begin));

    begin = end + 1;
    // skip any whitespace until the next none whitespace
    while (begin < s.size() && std::isspace(s[begin])) {
      begin++;
    }
  }

  return parts;
}

Player *Simulation::getPlayer(const std::string &uid) {
  for (Player &p : _players) {
    if (p.uid == uid) {
      return &p;
    }
  }
  return nullptr;
}

void Simulation::sendChatToAll(const std::string &msg) {
  using nlohmann::json;
  json resp_json;
  resp_json["type"] = "Chat";
  resp_json["data"] = {};
  resp_json["data"]["sender"] = "The Server";
  resp_json["data"]["message"] = msg;
  _web_socket_server->broadcast(resp_json.dump());
}
