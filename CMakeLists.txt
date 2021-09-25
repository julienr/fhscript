cmake_minimum_required(VERSION 3.14)
project(fhscript)

set(CMAKE_CXX_STANDARD 17)

set(BUILD_GMOCK OFF)
add_subdirectory("${PROJECT_SOURCE_DIR}/third_party/googletest" "third_party/googletest")

enable_testing()

add_executable(
  main
  src/main.cc
)

add_executable(
  tests
  src/fhscript_test.cc
)
target_link_libraries(
  tests
  gtest_main
)

include(GoogleTest)
gtest_discover_tests(tests)

# Copy the examples directory so tests can be run from build
add_custom_command(TARGET tests PRE_BUILD
                   COMMAND ${CMAKE_COMMAND} -E copy_directory
                       ${CMAKE_SOURCE_DIR}/examples/ $<TARGET_FILE_DIR:tests>/examples/)
