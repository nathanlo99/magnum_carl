
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

  // run_perft_tests("tests/short_perft.txt");

  // Board board(
  //     "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0
  //     1");
  Board board;
  for (int depth = 1; depth < 10; ++depth) {
    const int result =
        alpha_beta_evaluate(board, 0, depth, -MateScore, MateScore);
    std::cout << "Evaluation to depth " << depth << " was " << result
              << std::endl;
  }
}

int main(int argc, char *argv[]) {
  init_all();
  print_debug_info();
}
