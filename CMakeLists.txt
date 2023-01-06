cmake_minimum_required(VERSION 3.0.0)
project(magnum_carl VERSION 0.1.0)

# Enable exporting commands for use with vscode
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_EXPORT_COMPILE_COMMANDS true)
set(CMAKE_CXX_FLAGS "-pthread -Ofast -flto -ffast-math -Wall -Wextra -Wno-unused-parameter -pedantic")
set(CMAKE_EXE_LINKER_FLAGS "-Ofast -flto -ffast-math")

add_executable(magnum_carl main.cpp)
