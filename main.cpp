
#include "bitboard.hpp"
#include "board.hpp"
#include "perft.hpp"
#include "random.hpp"
#include "tests/test_perft.hpp"

#include <chrono>
#include <iostream>
#include <thread>

inline void init_all() {
  init_bitboards();
  init_slider_attack_tables();
  init_hash_keys();
}

inline void print_debug_info() {
  std::cout << "----- Start Debug Info -----" << std::endl;
  std::cout << "sizeof(Board)      = " << sizeof(Board) << std::endl;
  std::cout << "sizeof(castle_t)   = " << sizeof(castle_t) << std::endl;
  std::cout << "sizeof(hash_t)     = " << sizeof(hash_t) << std::endl;
  std::cout << "sizeof(Move)       = " << sizeof(Move) << std::endl;
  std::cout << "sizeof(piece_t)    = " << sizeof(piece_t) << std::endl;
  std::cout << "sizeof(square_t)   = " << sizeof(square_t) << std::endl;
  std::cout << "sizeof(bitboard_t) = " << sizeof(bitboard_t) << std::endl;
  std::cout << std::endl;

  run_perft_tests("tests/short_perft.txt");

  Board board;
  std::cout << board << std::endl;

  while (board.check_result() == InProgress) {
    const auto moves = board.legal_moves_slow();
    const auto move_idx = random_uint64() % moves.size();
    const auto move = moves[move_idx];
    board.make_move(move);

    std::cout << board << std::endl;
    std::this_thread::sleep_for(std::chrono::milliseconds(200));
  }
}

int main(int argc, char *argv[]) {
  init_all();
  print_debug_info();
}
