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
