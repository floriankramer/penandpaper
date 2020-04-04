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

#include <functional>

#include "Database.h"
#include "HttpServer.h"
#include "Simulation.h"
#include "WebSocketServer.h"
#include "Wiki.h"

int main(int argc, char **argv) {
  bool do_keycheck = !(argc == 2 && strcmp(argv[1], "--no-key") == 0);

  Database db("./database.sqlite3");
  std::shared_ptr<Wiki> wiki = std::make_shared<Wiki>(&db);
  Simulation sim;
  WebSocketServer wss(
      std::bind(&Simulation::onMessage, &sim, std::placeholders::_1),
      std::bind(&Simulation::onNewClient, &sim));
  sim.setWebSocketServer(&wss);

  HttpServer server(do_keycheck);
  server.registerRequestHandler("/wiki/.*", HttpServer::RequestType::GET, wiki);
  server.registerRequestHandler("/wiki/.*", HttpServer::RequestType::POST, wiki);
  server.run();
  return 0;
}
