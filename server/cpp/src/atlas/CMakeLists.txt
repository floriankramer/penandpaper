# Dependencies

# QT
find_package(Qt5 REQUIRED COMPONENTS Core Widgets)

set(CMAKE_AUTOMOC ON)
# set(CMAKE_AUTORCC ON)
# set(CMAKE_AUTOUIC ON)
set(CMAKE_INCLUDE_CURRENT_DIR ON)

add_library(atlas_core
  core/Map.cpp core/Map.h
  core/OpenSimplexNoise.cpp core/OpenSimplexNoise.h)

add_library(atlas_rendering
  rendering/TileRenderer.cpp rendering/TileRenderer.h
  rendering/Image.cpp rendering/Image.h)
target_link_directories(atlas_rendering PUBLIC core)
target_link_libraries(atlas_rendering atlas_core png)


add_executable(atlas_gui
  gui/main.cpp
  gui/MainWindow.cpp gui/MainWindow.h
  gui/Conversions.cpp gui/Conversions.h)
target_link_directories(atlas_gui PUBLIC core PUBLIC rendering)

target_link_libraries(atlas_gui Qt5::Core Qt5::Widgets atlas_rendering atlas_core)

# target_link_libraries(atlas png)
# target_link_libraries(atlas "${CMAKE_CURRENT_LIST_DIR}/../../depend/simple2d/build/libsimple2d.a")
