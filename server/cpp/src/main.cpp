#include <functional>

#include "HttpServer.h"
#include "Simulation.h"
#include "WebSocketServer.h"

int main(int argc, char **argv) {
  bool do_keycheck = !(argc == 2 && strcmp(argv[1], "--no-key") == 0);

  Simulation sim;
  WebSocketServer wss(
      std::bind(&Simulation::onMessage, &sim, std::placeholders::_1),
      std::bind(&Simulation::onNewClient, &sim));
  HttpServer server(do_keycheck);
  return 0;
}
