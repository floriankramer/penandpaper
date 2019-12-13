add_library(atlas
  Atlas.cpp Atlas.h
  BiomeMap.cpp BiomeMap.h
  Image.cpp Image.h
  Biome.h
  Point.h
  Triangle.cpp Triangle.h)

target_link_libraries(atlas png)

add_executable(atlas_debug atlas_debug_main.cpp)
target_link_libraries(atlas_debug atlas)
