
#include "bitboard.hpp"
#include "board.hpp"
#include "perft.hpp"
#include "tests/test_perft.hpp"

#include <iostream>

inline void init_all() {
  init_bitboards();
  init_slider_attack_tables();
  init_hash_keys();
}

int main(int argc, char *argv[]) {
  init_all();

  run_perft_tests("tests/short_perft.txt");
}
