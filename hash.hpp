
#pragma once

#include "castle_perms.hpp"
#include "piece.hpp"
#include "random.hpp"
#include "square.hpp"

#include <array>
#include <cstdint>
#include <iomanip>

using hash_t = uint64_t;

extern std::array<std::array<hash_t, 64>, 12> piece_keys;
extern std::array<hash_t, 64> en_passant_keys;
extern std::array<hash_t, 16> castle_keys;
extern hash_t side_key;

inline void init_hash_keys() {
  for (piece_t piece = 0; piece < InvalidPiece; ++piece)
    for (square_t sq = 0; sq < 64; ++sq)
      piece_keys[piece][sq] = random_uint64();

  for (square_t sq = 0; sq < 64; ++sq)
    en_passant_keys[sq] = random_uint64();

  for (castle_t perm = 0; perm < 16; ++perm)
    castle_keys[perm] = random_uint64();

  side_key = random_uint64();
}
