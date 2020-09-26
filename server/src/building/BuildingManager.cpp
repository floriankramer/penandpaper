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

#include "BuildingManager.h"

#include "Logger.h"

BuildingManager::BuildingManager(IdGenerator *id_generator)
    : _building(id_generator), _id_generator(id_generator) {}

nlohmann::json BuildingManager::toJson() const { return _building.toJson(); }

WebSocketServer::Response BuildingManager::onSetDoorOpen(
    const Packet &j, UserManager::UserPtr user) {
  if (!j.checkPermissions(Permissions::GAMEMASTER)) {
    return j.makeMissingPermissionsResponse();
  }
  std::shared_ptr<Door> d = _building.door(j.json().at("data").at("id"));
  if (d != nullptr) {
    d->setOpen(j.json().at("data").at("open").get<bool>());
    return {"", WebSocketServer::ResponseType::FORWARD};
  }
  return {"", WebSocketServer::ResponseType::SILENCE};
}

WebSocketServer::Response BuildingManager::onCreateRoom(
    const Packet &j, UserManager::UserPtr user) {
  using nlohmann::json;
  if (!j.checkPermissions(Permissions::GAMEMASTER)) {
    return j.makeMissingPermissionsResponse();
  }
  Vector2f pos = Vector2f::fromJson(j.json().at("data").at("position"));
  Vector2f size = Vector2f::fromJson(j.json().at("data").at("size"));
  std::shared_ptr<Room> r = _building.addRoom(pos, size);

  json response;
  response["type"] = "CreateRoom";
  response["data"] = r->toJson();

  return {response.dump(), WebSocketServer::ResponseType::BROADCAST};
}

WebSocketServer::Response BuildingManager::onCreateWall(
    const Packet &j, UserManager::UserPtr user) {
  using nlohmann::json;
  if (!j.checkPermissions(Permissions::GAMEMASTER)) {
    return j.makeMissingPermissionsResponse();
  }

  Vector2f start = Vector2f::fromJson(j.json().at("data").at("start"));
  Vector2f end = Vector2f::fromJson(j.json().at("data").at("end"));
  std::shared_ptr<Wall> w = _building.addWall(start, end);

  json response;
  response["type"] = "CreateWall";
  response["data"] = w->toJson();

  return {response.dump(), WebSocketServer::ResponseType::BROADCAST};
}

WebSocketServer::Response BuildingManager::onCreateDoor(
    const Packet &j, UserManager::UserPtr user) {
  using nlohmann::json;
  if (!j.checkPermissions(Permissions::GAMEMASTER)) {
    return j.makeMissingPermissionsResponse();
  }

  Vector2f position = Vector2f::fromJson(j.json().at("data").at("position"));
  float width = j.json().at("data").at("width").get<float>();
  float rotation = j.json().at("data").at("rotation").get<float>();
  std::shared_ptr<Door> d = _building.addDoor(position, width, rotation);

  json response;
  response["type"] = "CreateDoor";
  response["data"] = d->toJson();

  return {response.dump(), WebSocketServer::ResponseType::BROADCAST};
}

WebSocketServer::Response BuildingManager::onCreateFurniture(
    const Packet &j, UserManager::UserPtr user) {
  using nlohmann::json;
  if (!j.checkPermissions(Permissions::GAMEMASTER)) {
    return j.makeMissingPermissionsResponse();
  }

  Vector2f position = Vector2f::fromJson(j.json().at("data").at("position"));
  Vector2f size = Vector2f::fromJson(j.json().at("data").at("size"));
  float rotation = j.json().at("data").at("rotation").get<float>();
  std::shared_ptr<Furniture> f =
      _building.addFurniture(position, size, rotation);

  json response;
  response["type"] = "CreateFurniture";
  response["data"] = f->toJson();

  return {response.dump(), WebSocketServer::ResponseType::BROADCAST};
}

WebSocketServer::Response BuildingManager::onModifyRoom(
    const Packet &j, UserManager::UserPtr user) {
  using nlohmann::json;
  if (!j.checkPermissions(Permissions::GAMEMASTER)) {
    return j.makeMissingPermissionsResponse();
  }
  uint64_t id = j.json().at("data").at("id").get<uint64_t>();
  Vector2f pos = Vector2f::fromJson(j.json().at("data").at("position"));
  Vector2f size = Vector2f::fromJson(j.json().at("data").at("size"));
  bool is_visible = j.json().at("data").at("is_visible").get<bool>();

  std::shared_ptr<Room> r = _building.room(id);
  if (r) {
    r->position() = pos;
    r->size() = size;
    r->isVisible() = is_visible;

    json response;
    response["type"] = "ModifyRoom";
    response["data"] = r->toJson();

    return {response.dump(), WebSocketServer::ResponseType::BROADCAST};
  }
  return {"", WebSocketServer::ResponseType::SILENCE};
}

WebSocketServer::Response BuildingManager::onModifyWall(
    const Packet &j, UserManager::UserPtr user) {
  using nlohmann::json;
  if (!j.checkPermissions(Permissions::GAMEMASTER)) {
    return j.makeMissingPermissionsResponse();
  }

  uint64_t id = j.json().at("data").at("id").get<uint64_t>();
  Vector2f start = Vector2f::fromJson(j.json().at("data").at("start"));
  Vector2f end = Vector2f::fromJson(j.json().at("data").at("end"));
  bool is_visible = j.json().at("data").at("is_visible").get<bool>();

  std::shared_ptr<Wall> w = _building.wall(id);
  if (w) {
    w->start() = start;
    w->end() = end;
    w->isVisible() = is_visible;

    json response;
    response["type"] = "ModifyWall";
    response["data"] = w->toJson();

    return {response.dump(), WebSocketServer::ResponseType::BROADCAST};
  }
  return {"", WebSocketServer::ResponseType::SILENCE};
}

WebSocketServer::Response BuildingManager::onModifyDoor(
    const Packet &j, UserManager::UserPtr user) {
  using nlohmann::json;
  if (!j.checkPermissions(Permissions::GAMEMASTER)) {
    return j.makeMissingPermissionsResponse();
  }

  uint64_t id = j.json().at("data").at("id").get<uint64_t>();
  Vector2f position = Vector2f::fromJson(j.json().at("data").at("position"));
  float width = j.json().at("data").at("width").get<float>();
  float rotation = j.json().at("data").at("rotation").get<float>();
  bool is_open = j.json().at("data").at("is_open").get<bool>();
  bool is_visible = j.json().at("data").at("is_visible").get<bool>();

  std::shared_ptr<Door> d = _building.door(id);
  if (d) {
    d->position() = position;
    d->width() = width;
    d->rotation() = rotation;
    d->isOpen() = is_open;
    d->isVisible() = is_visible;

    json response;
    response["type"] = "ModifyDoor";
    response["data"] = d->toJson();

    return {response.dump(), WebSocketServer::ResponseType::BROADCAST};
  } else {
    LOG_WARN << "Tried to modify a nonexistant door" << LOG_END;
  }
  return {"", WebSocketServer::ResponseType::SILENCE};
}

WebSocketServer::Response BuildingManager::onModifyFurniture(
    const Packet &j, UserManager::UserPtr user) {
  using nlohmann::json;
  if (!j.checkPermissions(Permissions::GAMEMASTER)) {
    return j.makeMissingPermissionsResponse();
  }

  uint64_t id = j.json().at("data").at("id").get<uint64_t>();
  Vector2f position = Vector2f::fromJson(j.json().at("data").at("position"));
  Vector2f size = Vector2f::fromJson(j.json().at("data").at("size"));
  float rotation = j.json().at("data").at("rotation").get<float>();
  bool is_visible = j.json().at("data").at("is_visible").get<bool>();

  std::shared_ptr<Furniture> f = _building.furniture(id);
  if (f) {
    f->position() = position;
    f->size() = size;
    f->rotation() = rotation;
    f->isVisible() = is_visible;

    json response;
    response["type"] = "ModifyFurniture";
    response["data"] = f->toJson();

    return {response.dump(), WebSocketServer::ResponseType::BROADCAST};
  }
  return {"", WebSocketServer::ResponseType::SILENCE};
}

WebSocketServer::Response BuildingManager::onDeleteRoom(
    const Packet &j, UserManager::UserPtr user) {
  using nlohmann::json;
  if (!j.checkPermissions(Permissions::GAMEMASTER)) {
    return j.makeMissingPermissionsResponse();
  }
  _building.deleteRoom(j.json().at("data").at("id").get<uint64_t>());
  return {"", WebSocketServer::ResponseType::FORWARD};
}

WebSocketServer::Response BuildingManager::onDeleteWall(
    const Packet &j, UserManager::UserPtr user) {
  using nlohmann::json;
  if (!j.checkPermissions(Permissions::GAMEMASTER)) {
    return j.makeMissingPermissionsResponse();
  }
  _building.deleteWall(j.json().at("data").at("id").get<uint64_t>());
  return {"", WebSocketServer::ResponseType::FORWARD};
}

WebSocketServer::Response BuildingManager::onDeleteDoor(
    const Packet &j, UserManager::UserPtr user) {
  using nlohmann::json;
  if (!j.checkPermissions(Permissions::GAMEMASTER)) {
    return j.makeMissingPermissionsResponse();
  }
  _building.deleteDoor(j.json().at("data").at("id").get<uint64_t>());
  return {"", WebSocketServer::ResponseType::FORWARD};
}

WebSocketServer::Response BuildingManager::onDeleteFurniture(
    const Packet &j, UserManager::UserPtr user) {
  using nlohmann::json;
  if (!j.checkPermissions(Permissions::GAMEMASTER)) {
    return j.makeMissingPermissionsResponse();
  }
  _building.deleteFurniture(j.json().at("data").at("id").get<uint64_t>());
  return {"", WebSocketServer::ResponseType::FORWARD};
}

WebSocketServer::Response BuildingManager::onClearBuilding(
    const Packet &j, UserManager::UserPtr user) {
  if (!j.checkPermissions(Permissions::GAMEMASTER)) {
    return j.makeMissingPermissionsResponse();
  }
  _building = Building(_id_generator);
  return {"", WebSocketServer::ResponseType::FORWARD};
}

WebSocketServer::Response BuildingManager::onLoadBuilding(
    const Packet &j, UserManager::UserPtr user) {
  using nlohmann::json;
  if (!j.checkPermissions(Permissions::GAMEMASTER)) {
    return j.makeMissingPermissionsResponse();
  }
  _building = Building(_id_generator);
  _building.fromJson(j.json().at("data"));
  json r;
  r["type"] = "LoadBuilding";
  r["data"] = _building.toJson();
  return {r.dump(), WebSocketServer::ResponseType::BROADCAST};
}

void BuildingManager::registerPackets(
    std::unordered_map<std::string,
                       std::function<WebSocketServer::Response(
                           const Packet &, UserManager::UserPtr)>>
        *packet_handlers) {
  using std::placeholders::_1;
  using std::placeholders::_2;
  std::unordered_map<std::string, std::function<WebSocketServer::Response(
                                      const Packet &, UserManager::UserPtr)>>
      handlers = {
          {"SetDoorOpen",
           std::bind(&BuildingManager::onSetDoorOpen, this, _1, _2)},
          {"ClearBuilding",
           std::bind(&BuildingManager::onClearBuilding, this, _1, _2)},
          {"LoadBuilding",
           std::bind(&BuildingManager::onLoadBuilding, this, _1, _2)},
          {"CreateRoom",
           std::bind(&BuildingManager::onCreateRoom, this, _1, _2)},
          {"CreateWall",
           std::bind(&BuildingManager::onCreateWall, this, _1, _2)},
          {"CreateDoor",
           std::bind(&BuildingManager::onCreateDoor, this, _1, _2)},
          {"CreateFurniture",
           std::bind(&BuildingManager::onCreateFurniture, this, _1, _2)},
          {"ModifyRoom",
           std::bind(&BuildingManager::onModifyRoom, this, _1, _2)},
          {"ModifyWall",
           std::bind(&BuildingManager::onModifyWall, this, _1, _2)},
          {"ModifyDoor",
           std::bind(&BuildingManager::onModifyDoor, this, _1, _2)},
          {"ModifyFurniture",
           std::bind(&BuildingManager::onModifyFurniture, this, _1, _2)},
          {"DeleteRoom",
           std::bind(&BuildingManager::onDeleteRoom, this, _1, _2)},
          {"DeleteWall",
           std::bind(&BuildingManager::onDeleteWall, this, _1, _2)},
          {"DeleteDoor",
           std::bind(&BuildingManager::onDeleteDoor, this, _1, _2)},
          {"DeleteFurniture",
           std::bind(&BuildingManager::onDeleteFurniture, this, _1, _2)}};
  packet_handlers->insert(handlers.begin(), handlers.end());
}
