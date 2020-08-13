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

#ifndef MINIANT_LOGGER_H_
#define MINIANT_LOGGER_H_

#include <iostream>
#include <string>

#define LL_TRACE 0
#define LL_DEBUG 1
#define LL_INFO 2
#define LL_WARN 3
#define LL_ERROR 4

#ifndef LOGLEVEL
#define LOGLEVEL LL_DEBUG
#endif  // LOGLEVEL

const std::string LOG_STRING[5] = {
    "\x1b[34m[TRACE] ", "\x1b[36m[DEBUG] ", "[INFO ] ",
    "\x1b[33m[WARN ] ", "\x1b[31m[ERROR] ",
};

#define LOG(level)        \
  if (level < LOGLEVEL) { \
  } else                  \
    std::cout << LOG_STRING[level]
#define LOG_END "\x1b[0m" << std::endl

#define LOG_TRACE LOG(LL_TRACE)
#define LOG_DEBUG LOG(LL_DEBUG)
#define LOG_INFO LOG(LL_INFO)
#define LOG_WARN LOG(LL_WARN)
#define LOG_ERROR LOG(LL_ERROR)

#endif  // MINIANT_LOGGER_H_
