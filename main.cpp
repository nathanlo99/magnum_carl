
#include "bitboard.hpp"
#include "board.hpp"
#include "perft.hpp"
#include "random.hpp"
#include "search.hpp"
#include "tests/test_perft.hpp"
#include "transposition_table.hpp"

#include <chrono>
#include <iostream>
#include <thread>

inline void init_all() {
  init_bitboards();
  init_slider_attack_tables();
  init_hash_keys();
  tt_clear();
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

  Board board;
  std::cout << board << std::endl;
  while (board.check_result() == InProgress) {
    const Move best_move = alpha_beta_search(board, 100, 1000);
    board.make_move(best_move);
    std::cout << board << std::endl;
    std::cout << best_move << std::endl;
  }
}

int main(int argc, char *argv[]) {
  init_all();
  print_debug_info();
}
