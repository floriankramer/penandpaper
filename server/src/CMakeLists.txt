
add_library(penandpaper-utils STATIC
  utils/File.cpp utils/File.h)

if (BUILD_ATLAS_MODULE)
  add_subdirectory(atlas)
endif ()

add_subdirectory(geometry)
add_subdirectory(building)

if (BUILD_FONTRENDERER)
  add_subdirectory(fontrenderer)
endif ()

add_library(penandpaper-core STATIC
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
  UserManager.cpp UserManager.h
  Random.cpp Random.h
  QGramIndex.cpp QGramIndex.h
  BTree.cpp BTree.h
  Os.cpp Os.h
  Plugin.cpp Plugin.h
  PluginManager.cpp PluginManager.h
  LuaScript.cpp LuaScript.h
  ${SQLITE_SRC_FILES})

target_link_libraries(penandpaper-core
  building
  geometry
  penandpaper-utils
  sodium
  uWebSockets
  ${LUA_LIBRARIES}
  OpenSSL::SSL
  ${CMAKE_THREAD_LIBS_INIT})

add_executable(penandpaper-server main.cpp)

add_executable(base64 base64_main.cpp
  Util.cpp Util.h)

add_executable(markdown markdown_main.cpp
  Markdown.cpp Markdown.h
  MarkdownNode.cpp MarkdownNode.h)

target_link_libraries(penandpaper-server
  penandpaper-core)

if (NOT WIN32)
    target_link_libraries(penandpaper-server sqlite3)
else()
    target_link_libraries(penandpaper-server bcrypt)
endif ()

install(TARGETS penandpaper-server DESTINATION bin/)
