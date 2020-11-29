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

#ifndef WIN32
#include <getopt.h>
#endif

#include <functional>
#include <memory>

#include "Database.h"
#include "HttpServer.h"
#include "Logger.h"
#include "PluginManager.h"
#include "Simulation.h"
#include "UserManager.h"
#include "WebSocketServer.h"
#include "Wiki.h"

struct Settings {
  std::string base_dir = ".";
};

Settings parseSettings(int argc, char **argv) {
  Settings s;
#ifndef WIN32
  struct option long_options[] = {{"data-dir", required_argument, 0, 'd'},
                                  {0, 0, 0, 0}};
  int option_index = 0;
  bool failed = false;
  while (true) {
    int c = getopt_long(argc, argv, "d:", long_options, &option_index);
    if (c < 0) {
      break;
    }
    switch (c) {
      case 0:
        // Flag option, nothing to do
        break;
      case 'd':
        s.base_dir = optarg;
      case '?':
        failed = true;
        break;
      default:
        LOG_ERROR << "Unexpected argument: " << char(c) << LOG_END;
        failed = true;
        break;
    }
  }
  if (failed) {
    exit(1);
  }
#else
  s.do_keycheck = false;
#endif
  return s;
}

int main(int argc, char **argv) {
  Settings settings = parseSettings(argc, argv);

  std::shared_ptr<PluginManager> plugins = std::make_shared<PluginManager>();

  Database db("./database.sqlite3");
  std::shared_ptr<UserManager> user_manager =
      std::make_shared<UserManager>(&db);

  std::shared_ptr<Wiki> wiki = std::make_shared<Wiki>(&db);
  Simulation sim;
  std::shared_ptr<WebSocketServer> wss = std::make_shared<WebSocketServer>(
      user_manager,
      std::bind(&Simulation::onMessage, &sim, std::placeholders::_1,
                std::placeholders::_2),
      std::bind(&Simulation::onNewClient, &sim, std::placeholders::_1),
      settings.base_dir);
  sim.setWebSocketServer(wss);
  sim.setPluginManager(plugins.get());

  HttpServer server(user_manager, settings.base_dir);
  server.registerRequestHandler("/wiki/.*", HttpServer::RequestType::GET, wiki);
  server.registerRequestHandler("/wiki/.*", HttpServer::RequestType::POST,
                                wiki);
  server.registerRequestHandler("/plugin/.*", HttpServer::RequestType::GET,
                                plugins);
  server.setWSSHandler(wss);
  server.run();
  return 0;
}
