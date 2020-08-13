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

#include "Building.h"

#include "Util.h"

Building::Building(IdGenerator *id_generator)
    : _id(id_generator->next()), _id_generator(id_generator) {}

nlohmann::json Building::toJson() const {
  using nlohmann::json;
  json j;
  j["id"] = _id;
  j["rooms"] = util::map<std::shared_ptr<Room>, json>(
      _rooms,
      [](const std::shared_ptr<Room> &r) -> json { return r->toJson(); });

  j["walls"] = util::map<std::shared_ptr<Wall>, json>(
      _walls,
      [](const std::shared_ptr<Wall> &w) -> json { return w->toJson(); });

  j["doors"] = util::map<std::shared_ptr<Door>, json>(
      _doors,
      [](const std::shared_ptr<Door> &d) -> json { return d->toJson(); });

  j["furniture"] = util::map<std::shared_ptr<Furniture>, json>(
      _furniture,
      [](const std::shared_ptr<Furniture> &d) -> json { return d->toJson(); });

  return j;
}

void Building::fromJson(const nlohmann::json &j) {
  using nlohmann::json;
  for (const json &room : j.at("rooms")) {
    _rooms.emplace_back(
        std::make_shared<Room>(Room::fromJson(room, _id_generator->next())));
  }
  for (const json &room : j.at("walls")) {
    _walls.emplace_back(
        std::make_shared<Wall>(Wall::fromJson(room, _id_generator->next())));
  }
  for (const json &room : j.at("doors")) {
    _doors.emplace_back(
        std::make_shared<Door>(Door::fromJson(room, _id_generator->next())));
  }
  for (const json &room : j.at("furniture")) {
    _furniture.emplace_back(std::make_shared<Furniture>(
        Furniture::fromJson(room, _id_generator->next())));
  }
}

std::shared_ptr<Room> Building::addRoom(const Vector2f &pos,
                                        const Vector2f &size) {
  _rooms.emplace_back(std::make_shared<Room>(_id_generator->next(), pos, size));
  return _rooms.back();
}

std::shared_ptr<Wall> Building::addWall(const Vector2f &start,
                                        const Vector2f &end) {
  _walls.emplace_back(
      std::make_shared<Wall>(_id_generator->next(), start, end));
  return _walls.back();
}

std::shared_ptr<Door> Building::addDoor(const Vector2f &position, float width,
                                        float rotation) {
  _doors.emplace_back(
      std::make_shared<Door>(_id_generator->next(), position, width, rotation));
  return _doors.back();
}

std::shared_ptr<Furniture> Building::addFurniture(const Vector2f &position,
                                                  const Vector2f &size,
                                                  float rotation) {
  _furniture.emplace_back(std::make_shared<Furniture>(
      _id_generator->next(), position, size, rotation));
  return _furniture.back();
}

std::shared_ptr<Room> Building::room(uint64_t id) {
  for (std::shared_ptr<Room> &r : _rooms) {
    if (r->id() == id) {
      return r;
    }
  }
  return nullptr;
}

std::shared_ptr<Wall> Building::wall(uint64_t id) {
  for (std::shared_ptr<Wall> &r : _walls) {
    if (r->id() == id) {
      return r;
    }
  }
  return nullptr;
}

std::shared_ptr<Door> Building::door(uint64_t id) {
  for (std::shared_ptr<Door> &r : _doors) {
    if (r->id() == id) {
      return r;
    }
  }
  return nullptr;
}

std::shared_ptr<Furniture> Building::furniture(uint64_t id) {
  for (std::shared_ptr<Furniture> &r : _furniture) {
    if (r->id() == id) {
      return r;
    }
  }
  return nullptr;
}

bool Building::deleteRoom(uint64_t id) {
  for (size_t i = 0; i < _rooms.size(); ++i) {
    if (_rooms[i]->id() == id) {
      std::iter_swap(_rooms.begin() + i, _rooms.end() - 1);
      _rooms.erase(_rooms.end() - 1);
      return true;
    }
  }
  return false;
}

bool Building::deleteWall(uint64_t id) {
  for (size_t i = 0; i < _walls.size(); ++i) {
    if (_walls[i]->id() == id) {
      std::iter_swap(_walls.begin() + i, _walls.end() - 1);
      _walls.erase(_walls.end() - 1);
      return true;
    }
  }
  return false;
}

bool Building::deleteDoor(uint64_t id) {
  for (size_t i = 0; i < _doors.size(); ++i) {
    if (_doors[i]->id() == id) {
      std::iter_swap(_doors.begin() + i, _doors.end() - 1);
      _doors.erase(_doors.end() - 1);
      return true;
    }
  }
  return false;
}

bool Building::deleteFurniture(uint64_t id) {
  for (size_t i = 0; i < _furniture.size(); ++i) {
    if (_furniture[i]->id() == id) {
      std::iter_swap(_furniture.begin() + i, _furniture.end() - 1);
      _furniture.erase(_furniture.end() - 1);
      return true;
    }
  }
  return false;
}
