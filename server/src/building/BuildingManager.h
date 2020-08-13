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

#include <nlohmann/json.hpp>

#include "Building.h"
#include "WebSocketServer.h"
#include "Packet.h"
#include "IdGenerator.h"

class BuildingManager {
 public:
  BuildingManager(IdGenerator *id_generator);

  WebSocketServer::Response onSetDoorOpen(const Packet &j);
  WebSocketServer::Response onCreateRoom(const Packet &j);
  WebSocketServer::Response onCreateWall(const Packet &j);
  WebSocketServer::Response onCreateDoor(const Packet &j);
  WebSocketServer::Response onCreateFurniture(const Packet &j);

  WebSocketServer::Response onModifyRoom(const Packet &j);
  WebSocketServer::Response onModifyWall(const Packet &j);
  WebSocketServer::Response onModifyDoor(const Packet &j);
  WebSocketServer::Response onModifyFurniture(const Packet &j);

  WebSocketServer::Response onDeleteRoom(const Packet &j);
  WebSocketServer::Response onDeleteWall(const Packet &j);
  WebSocketServer::Response onDeleteDoor(const Packet &j);
  WebSocketServer::Response onDeleteFurniture(const Packet &j);

  WebSocketServer::Response onClearBuilding(const Packet &j);
  WebSocketServer::Response onLoadBuilding(const Packet &j);

  nlohmann::json toJson() const;

  void registerPackets(
      std::unordered_map<
          std::string, std::function<WebSocketServer::Response(const Packet &)>>
          *packet_handlers);

 private:
  Building _building;
  IdGenerator *_id_generator;
};
