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
#include "Packet.h"

#include "Player.h"
#include "Simulation.h"

Packet::Packet(nlohmann::json json, Simulation *simulation)
    : _json(json), _simulation(simulation) {}

const nlohmann::json &Packet::json() const { return _json; }

bool Packet::checkPermissions(Permissions min_perm) const {
  if (_json.contains("uid")) {
    std::string uid = json().at("uid");
    Player *p = _simulation->getPlayer(uid);
    if (p != nullptr) {
      return p->permissions == min_perm;
    }
  }
  return false;
}

WebSocketServer::Response Packet::makeMissingPermissionsResponse() const {
  nlohmann::json response;
  response["type"] = "Error";

  nlohmann::json resp_data;
  resp_data["msg"] = "Missing permissions.";
  resp_data["cause"] = json();

  response["data"] = resp_data;
  return {response.dump(), WebSocketServer::ResponseType::RETURN};
}
