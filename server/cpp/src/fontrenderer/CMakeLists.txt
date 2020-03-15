find_package(Freetype REQUIRED)

add_executable(fontrenderer
  FontRenderer.cpp FontRenderer.h)

target_include_directories(fontrenderer PRIVATE /usr/include/msdfgen)
target_include_directories(fontrenderer PRIVATE ${FREETYPE_INCLUDE_DIRS})
target_link_libraries(fontrenderer msdfgen png ${FREETYPE_LIBRARIES})
