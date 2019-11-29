#include <functional>

#include "HttpServer.h"
#include "Simulation.h"
#include "WebSocketServer.h"

int main(int argc, char **argv) {
  Simulation sim;
  WebSocketServer wss(
      std::bind(&Simulation::onMessage, &sim, std::placeholders::_1),
      std::bind(&Simulation::onNewClient, &sim));
  HttpServer server;
  return 0;
}
