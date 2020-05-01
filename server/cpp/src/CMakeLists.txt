cmake_minimum_required(VERSION 3.7)
project(penandpaper)


set(BUILD_ATLAS_MODULE OFF CACHE BOOL "Build the atlas module")
set(BUILD_FONTRENDERER ON CACHE BOOL "Build the font renderer used to pregenerate sdf font.")

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


if (BUILD_ATLAS_MODULE)
  #==============================================================================
  # simple2d
  #==============================================================================
  # This is used for fast 2d vector graphic rendering for the maps
  include_directories(SYSTEM "${CMAKE_CURRENT_LIST_DIR}/../depend/simple2d/include/")
endif ()

#============================================================================== 
# penandpaper
#============================================================================== 

if (BUILD_ATLAS_MODULE)
  add_subdirectory(atlas)
endif ()

add_subdirectory(geometry)
add_subdirectory(building)

if (BUILD_FONTRENDERER)
  add_subdirectory(fontrenderer)
endif ()

add_executable(penandpaper-server main.cpp
  HttpServer.cpp HttpServer.h
  WebSocketServer.cpp WebSocketServer.h
  Simulation.cpp Simulation.h
  Logger.h
  Token.cpp Token.h
  Doodad.cpp Doodad.h
  Serializeable.h
  Util.cpp Util.h
  Packet.cpp Packet.h
  Permissions.h
  Player.h
  IdGenerator.cpp IdGenerator.h
  Wiki.cpp Wiki.h
  Database.cpp Database.h
  Markdown.cpp Markdown.h
  Authenticator.cpp Authenticator.h
  Random.cpp Random.h
  QGramIndex.cpp QGramIndex.h
  BTree.cpp BTree.h)


add_executable(base64 base64_main.cpp
  Util.cpp Util.h)

add_executable(markdown markdown_main.cpp
  Markdown.cpp Markdown.h)

target_link_libraries(penandpaper-server building geometry sqlite3 OpenSSL::SSL ${CMAKE_THREAD_LIBS_INIT})

install(TARGETS penandpaper-server DESTINATION bin/)
