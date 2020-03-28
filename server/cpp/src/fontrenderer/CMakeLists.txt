find_package(Freetype REQUIRED)

add_subdirectory("${CMAKE_CURRENT_LIST_DIR}/../../depend/msdfgen" "msdfgen")

add_executable(fontrenderer
  FontRenderer.cpp FontRenderer.h)

target_include_directories(fontrenderer PRIVATE ${CMAKE_CURRENT_LIST_DIR}/../../depend/msdfgen/)
target_include_directories(fontrenderer PRIVATE ${FREETYPE_INCLUDE_DIRS})
target_link_libraries(fontrenderer lib_msdfgen png ${FREETYPE_LIBRARIES})
