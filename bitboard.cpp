
#include "bitboard.hpp"
#include <array>

std::array<std::array<bitboard_t, 64>, 2> pawn_attack_bitboards;
std::array<bitboard_t, 64> knight_attack_bitboards;
std::array<bitboard_t, 64> king_attack_bitboards;
