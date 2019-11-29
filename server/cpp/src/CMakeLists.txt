cmake_minimum_required(VERSION 3.7)
project(penandpaper)


#==============================================================================
# threading
#==============================================================================
find_package(Threads REQUIRED)


#==============================================================================
# ssl
#==============================================================================
find_package(OpenSSL REQUIRED)

#============================================================================== 
# httplib
#============================================================================== 
include_directories(SYSTEM "${CMAKE_CURRENT_LIST_DIR}/../depend/cpp-httplib/")

#============================================================================== 
# websocket-hpp 
#============================================================================== 
include_directories(SYSTEM "${CMAKE_CURRENT_LIST_DIR}/../depend/websocketpp/")

#==============================================================================
# nlohman json
#==============================================================================
include_directories(SYSTEM "${CMAKE_CURRENT_LIST_DIR}/../depend/json/include/")


#============================================================================== 
# penandpaper
#============================================================================== 
add_executable(penandpaper-server main.cpp
  HttpServer.cpp HttpServer.h
  WebSocketServer.cpp WebSocketServer.h
  Simulation.cpp Simulation.h
  Logger.h
  Token.cpp Token.h
  Doodad.cpp Doodad.h
  Serializeable.h)

target_link_libraries(penandpaper-server OpenSSL::SSL ${CMAKE_THREAD_LIBS_INIT})

