cmake_minimum_required(VERSION 3.0.0)
project(magnum_carl VERSION 0.1.0)

# Enable exporting commands for use with vscode
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_FLAGS "-pthread -Ofast -flto -ffast-math -Wall -Wextra -Wno-unused-parameter -pedantic")
set(CMAKE_EXE_LINKER_FLAGS "-Ofast -flto -ffast-math")

include_directories(magnum_carl PUBLIC . tests)

add_executable(magnum_carl bitboard.cpp board.cpp hash.cpp magic.cpp transposition_table.cpp main.cpp)
set_target_properties(magnum_carl PROPERTIES COMPILE_FLAGS "-DNDEBUG -DLOG_DIR=\\\"${CMAKE_SOURCE_DIR}/debug_logs/\\\" -DTABLEBASE_DIR=\\\"${CMAKE_SOURCE_DIR}/tablebases/\\\"")
