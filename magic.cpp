
#include "magic.hpp"

std::array<bitboard_t, 64> bishop_masks;
std::array<bitboard_t, 64> rook_masks;
std::array<std::array<bitboard_t, 512>, 64> bishop_attacks;
std::array<std::array<bitboard_t, 4096>, 64> rook_attacks;
