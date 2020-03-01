#pragma once

#include <nlohmann/json.hpp>

#include "WebSocketServer.h"
#include "Permissions.h"

class Simulation;

class Packet {
public:

  Packet(nlohmann::json json, Simulation *simulation);

  const nlohmann::json &json() const;


  WebSocketServer::Response makeMissingPermissionsResponse() const;
  bool checkPermissions(Permissions min_perm) const;

private:
  nlohmann::json _json;
  Simulation *_simulation;
};
