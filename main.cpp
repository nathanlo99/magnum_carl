
#include "bitboard.hpp"
#include "board.hpp"

#include <iostream>

inline void init_all() {
  init_bitboards();
  init_slider_attack_tables();
  init_hash_keys();
}

int main(int argc, char *argv[]) {
  init_all();
  Board board(Board::start_fen);
  std::cout << board << std::endl;
}
