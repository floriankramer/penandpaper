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
