cmake_minimum_required(VERSION 3.16)
project(gigi)

set(CMAKE_BUILD_TYPE Debug)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# set the output directory for built objects
# make sure the dynamic library goes into the build directory
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/$<CONFIGURATION>")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/$<CONFIGURATION>")

add_subdirectory(vendor/sdl)
add_subdirectory(vendor/libuv)

file(GLOB_RECURSE bsrepl_srcs CONFIGURE_DEPENDS "src/bootstrap/*.c")
add_executable(bsrepl ${bsrepl_srcs})
target_compile_features(bsrepl PRIVATE c_std_23)

add_executable(gigi src/main.c)
target_compile_features(gigi PRIVATE c_std_23)
target_compile_definitions(gigi PUBLIC RUNTIME_PATH="./runtime/")
target_link_libraries(gigi PRIVATE SDL3::SDL3)
target_link_libraries(gigi PRIVATE uv)

add_custom_command(
  TARGET ${CMAKE_PROJECT_NAME} POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_SOURCE_DIR}/runtime ${CMAKE_SOURCE_DIR}/out/runtime
  COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE_DIR:gigi>/gigi ${CMAKE_SOURCE_DIR}/out/gigi
  COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE_DIR:bsrepl>/bsrepl ${CMAKE_SOURCE_DIR}/out/bsrepl
)
