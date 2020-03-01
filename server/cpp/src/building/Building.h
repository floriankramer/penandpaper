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
