cmake_minimum_required(VERSION 3.7)
project(penandpaper)

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(BUILD_ATLAS_MODULE OFF CACHE BOOL "Build the atlas module")
set(BUILD_FONTRENDERER ON CACHE BOOL "Build the font renderer used to pregenerate sdf font.")

set(SQLITE_SRC_FILES "")

#==============================================================================
# threading
#==============================================================================
find_package(Threads REQUIRED)


#==============================================================================
# ssl
#==============================================================================
find_package(OpenSSL REQUIRED)
include_directories(${OPENSSL_INCLUDE_DIR})

#============================================================================== 
# httplib
#============================================================================== 
include_directories(SYSTEM "${CMAKE_CURRENT_LIST_DIR}/../depend/cpp-httplib/")

#============================================================================== 
# websocket-hpp 
#============================================================================== 
include_directories(SYSTEM "${CMAKE_CURRENT_LIST_DIR}/../depend/websocketpp/")
if (WIN32)
 add_definitions(-D_WEBSOCKETPP_CPP11_STL_)
endif (WIN32)

#==============================================================================
# nlohman json
#==============================================================================
include_directories(SYSTEM "${CMAKE_CURRENT_LIST_DIR}/../depend/json/include/")


#==============================================================================
# sqlite3
#==============================================================================
if (WIN32)
  include_directories(SYSTEM "${CMAKE_CURRENT_LIST_DIR}/../depend/sqlite3/")
  set(SQLITE_SRC_FILES "${CMAKE_CURRENT_LIST_DIR}/../depend/sqlite3/sqlite3.c")
endif ()

#==============================================================================
# asio
#==============================================================================
if (WIN32)
  include_directories(SYSTEM "${CMAKE_CURRENT_LIST_DIR}/../depend/asio-1.16.1/include")
endif ()

#==============================================================================
# boost
#==============================================================================
if (WIN32)
  include_directories(SYSTEM "${CMAKE_CURRENT_LIST_DIR}/../depend/boost_1_73_0")
endif ()

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
  MarkdownNode.cpp MarkdownNode.h
  Markdown.cpp Markdown.h
  Authenticator.cpp Authenticator.h
  Random.cpp Random.h
  QGramIndex.cpp QGramIndex.h
  BTree.cpp BTree.h
  Os.cpp Os.h
  Plugin.cpp Plugin.h
  PluginManager.cpp PluginManager.h
  File.cpp File.h
  LuaScript.cpp LuaScript.h
  ${SQLITE_SRC_FILES})


add_executable(base64 base64_main.cpp
  Util.cpp Util.h)

add_executable(markdown markdown_main.cpp
  Markdown.cpp Markdown.h
  MarkdownNode.cpp MarkdownNode.h)

target_link_libraries(penandpaper-server building geometry lua OpenSSL::SSL ${CMAKE_THREAD_LIBS_INIT})
if (NOT WIN32) 
    target_link_libraries(penandpaper-server sqlite3)
else()
    target_link_libraries(penandpaper-server bcrypt)
endif ()

install(TARGETS penandpaper-server DESTINATION bin/)
