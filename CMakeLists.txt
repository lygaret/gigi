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

# Declaring our executable
add_executable(${PROJECT_NAME} src/main.c)

target_compile_definitions(${PROJECT_NAME} PUBLIC RUNTIME_PATH="./runtime/")
target_link_libraries(${PROJECT_NAME} PRIVATE SDL3::SDL3)
target_link_libraries(${PROJECT_NAME} PRIVATE uv)

add_custom_command(
  TARGET ${CMAKE_PROJECT_NAME} POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E copy_directory ${CMAKE_SOURCE_DIR}/runtime ${CMAKE_SOURCE_DIR}/out/runtime
  COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE_DIR:${CMAKE_PROJECT_NAME}>/${CMAKE_PROJECT_NAME} ${CMAKE_SOURCE_DIR}/out/${CMAKE_PROJECT_NAME}
)  
