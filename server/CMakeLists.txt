cmake_minimum_required(VERSION 3.7)
project(penandpaper)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(BUILD_ATLAS_MODULE OFF CACHE BOOL "Build the atlas module")
set(BUILD_FONTRENDERER ON CACHE BOOL "Build the font renderer used to pregenerate sdf font.")

set(SQLITE_SRC_FILES "")

if (${CMAKE_GENERATOR} STREQUAL "Ninja")
    if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
       add_compile_options (-fdiagnostics-color=always)
    elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
       add_compile_options (-fcolor-diagnostics)
    endif ()
endif ()

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
# nlohman json
#==============================================================================
include_directories(SYSTEM "${CMAKE_CURRENT_LIST_DIR}/depend/json/include/")


#==============================================================================
# sqlite3
#==============================================================================
if (WIN32)
  include_directories(SYSTEM "${CMAKE_CURRENT_LIST_DIR}/depend/sqlite3/")
  set(SQLITE_SRC_FILES "${CMAKE_CURRENT_LIST_DIR}/depend/sqlite3/sqlite3.c")
endif ()

#==============================================================================
# lua
#==============================================================================
find_package(Lua REQUIRED)
include_directories(SYSTEM ${LUA_INCLUDE_DIR})

#==============================================================================
# uWebSockets 
#==============================================================================
add_subdirectory("depend/uWebSockets")


#==============================================================================
# penandpaper
#==============================================================================
add_subdirectory("src")
