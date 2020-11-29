add_library(building
  Building.cpp Building.h
  Door.cpp Door.h
  Furniture.cpp Furniture.h
  Room.cpp Room.h
  Wall.cpp Wall.h
  BuildingManager.cpp BuildingManager.h
  Visible.cpp Visible.h)

target_link_libraries(building geometry
  uWebSockets
  )
target_include_directories(building PUBLIC "${CMAKE_CURRENT_LIST_DIR}/..")
