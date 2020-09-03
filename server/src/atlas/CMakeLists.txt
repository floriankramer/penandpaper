# Dependencies

# QT
find_package(Qt5 REQUIRED COMPONENTS Core Widgets)

# pthread
find_package(Threads REQUIRED)

set(CMAKE_AUTOMOC ON)
# set(CMAKE_AUTORCC ON)
# set(CMAKE_AUTOUIC ON)
set(CMAKE_INCLUDE_CURRENT_DIR ON)

add_library(atlas_core
  core/Material.h
  core/Semaphore.h
  core/Map.cpp core/Map.h
  core/OpenSimplexNoise.cpp core/OpenSimplexNoise.h
  core/ObjectIndex.cpp core/ObjectIndex.h
  core/ObjectDistribution.cpp core/ObjectDistribution.h
  core/EventBus.cpp core/EventBus.h
  core/Image.cpp core/Image.h
  core/GridIndex.cpp core/GridIndex.h)
target_link_libraries(atlas_core penandpaper-utils Threads::Threads)

add_library(atlas_rendering
  rendering/TileRenderer.cpp rendering/TileRenderer.h)
target_link_directories(atlas_rendering PUBLIC core)
target_link_libraries(atlas_rendering penandpaper-utils atlas_core png)


add_executable(atlas_gui
  gui/main.cpp
  gui/MainWindow.cpp gui/MainWindow.h
  gui/Conversions.cpp gui/Conversions.h
  gui/MapWidget.cpp gui/MapWidget.h
  gui/BrushPanel.cpp gui/BrushPanel.h
  gui/ObjectPanel.cpp gui/ObjectPanel.h
  gui/Camera.cpp gui/Camera.h)
target_link_directories(atlas_gui PUBLIC core PUBLIC rendering)

target_link_libraries(atlas_gui Qt5::Core Qt5::Widgets penandpaper-utils atlas_rendering atlas_core)

# target_link_libraries(atlas png)
# target_link_libraries(atlas "${CMAKE_CURRENT_LIST_DIR}/../../depend/simple2d/build/libsimple2d.a")
