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
  WebSocketServer::Response onClearBuilding(const Packet &j);
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

  nlohmann::json toJson() const;

  void registerPackets(
      std::unordered_map<
          std::string, std::function<WebSocketServer::Response(const Packet &)>>
          packet_handlers);

 private:
  Building _building;
  IdGenerator *_id_generator;
};
