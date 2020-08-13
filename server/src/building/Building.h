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

#include <cstdint>
#include <memory>
#include <nlohmann/json.hpp>
#include <vector>

#include "Door.h"
#include "IdGenerator.h"
#include "Room.h"
#include "Wall.h"

class Building {
 public:
  Building(IdGenerator *id_generator);

  nlohmann::json toJson() const;
  void fromJson(const nlohmann::json &j);

  std::shared_ptr<Room> addRoom(const Vector2f &pos, const Vector2f &size);
  std::shared_ptr<Wall> addWall(const Vector2f &start, const Vector2f &end);
  std::shared_ptr<Door> addDoor(const Vector2f &position, float width, float rotation);
  std::shared_ptr<Furniture> addFurniture(const Vector2f &position, const Vector2f &size,
                          float rotation);

  std::shared_ptr<Room> room(uint64_t id);
  std::shared_ptr<Wall> wall(uint64_t id);
  std::shared_ptr<Door> door(uint64_t id);
  std::shared_ptr<Furniture> furniture(uint64_t id);

  bool deleteRoom(uint64_t id);
  bool deleteWall(uint64_t id);
  bool deleteDoor(uint64_t id);
  bool deleteFurniture(uint64_t id);

 private:
  uint64_t _id;

  std::vector<std::shared_ptr<Room>> _rooms;
  std::vector<std::shared_ptr<Wall>> _walls;
  std::vector<std::shared_ptr<Door>> _doors;
  std::vector<std::shared_ptr<Furniture>> _furniture;

  IdGenerator *_id_generator;
};
