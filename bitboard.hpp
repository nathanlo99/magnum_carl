
#pragma once

#include <array>
#include <cassert>
#include <cstdint>
#include <iomanip>
#include <ios>
#include <iostream>
#include <utility>

#include "square.hpp"
#include "util.hpp"

using bitboard_t = uint64_t;

// Modifying or reading bitboards at given indices
constexpr inline bitboard_t square_to_bitboard(const size_t sq) {
  return static_cast<bitboard_t>(1) << sq;
}
constexpr inline bool get_bit(const bitboard_t board, const size_t sq) {
  return board & square_to_bitboard(sq);
}
constexpr inline void set_bit(bitboard_t &board, const size_t sq) {
  board |= square_to_bitboard(sq);
}
constexpr inline void unset_bit(bitboard_t &board, const size_t sq) {
  board &= ~square_to_bitboard(sq);
}
constexpr inline void toggle_bit(bitboard_t &board, const size_t sq) {
  board ^= square_to_bitboard(sq);
}

// Built-in functions for counting / finding bits
constexpr inline auto bit_count(const bitboard_t board) {
  return __builtin_popcountll(board);
}
constexpr inline square_t lsb(const bitboard_t board) {
  assert(board != 0);
  return __builtin_ctzll(board);
}
constexpr inline square_t msb(const bitboard_t board) {
  assert(board != 0);
  return 63 ^ __builtin_clzll(board);
}
constexpr inline square_t pop_bit(bitboard_t &board) {
  const square_t sq = lsb(board);
  board &= board - 1;
  return sq;
}

inline void print_bitboard(const bitboard_t board) {
  std::cout << " " << std::internal // fill between the prefix and the number
            << std::setfill('0') << std::hex << std::setw(16) << board
            << std::dec << std::endl;
  for (int row = 7; row >= 0; --row) {
    std::cout << (row + 1) << " ";
    for (int col = 0; col < 8; ++col) {
      const int idx = 8 * row + col;
      const char display_char = get_bit(board, idx) ? '#' : '.';
      std::cout << display_char << ' ';
    }
    std::cout << std::hex << std::setw(1) << ((board >> (8 * row)) & 0xF)
              << ((board >> (8 * row + 4)) & 0xF) << std::dec << std::endl;
  }
  std::cout << "  a b c d e f g h" << '\n' << std::endl;
  std::cout << "Count: " << bit_count(board)
            << ", lsb: " << (board == 0 ? "-" : square_to_string(lsb(board)))
            << ", msb: " << (board == 0 ? "-" : square_to_string(msb(board)))
            << std::endl;
  std::cout << "----------------------------" << std::endl;
}

constexpr std::array<bitboard_t, 64> file_mask = []() {
  std::array<bitboard_t, 64> result = {};
  constexpr bitboard_t a_file = 0x0101'0101'0101'0101;
  for (square_t sq = 0; sq < 64; ++sq) {
    result[sq] = (a_file << square_file(sq));
  }
  return result;
}();

constexpr std::array<bitboard_t, 64> rank_mask = []() {
  std::array<bitboard_t, 64> result = {};
  constexpr bitboard_t rank_1 = 0x00FF;
  for (square_t sq = 0; sq < 64; ++sq) {
    result[sq] = (rank_1 << (8 * square_rank(sq)));
  }
  return result;
}();

constexpr std::array<bitboard_t, 64> isolated_pawn_mask = []() {
  std::array<bitboard_t, 64> result = {};
  for (square_t sq = 0; sq < 64; ++sq) {
    result[sq] = 0ULL;
    if (square_file(sq) != 0)
      result[sq] |= file_mask[sq - 1];
    if (square_file(sq) != 7)
      result[sq] |= file_mask[sq + 1];
  }
  return result;
}();

constexpr std::array<std::array<bitboard_t, 64>, 2> passed_pawn_mask = []() {
  std::array<std::array<bitboard_t, 64>, 2> result = {};
  for (square_t sq = 0; sq < 64; ++sq) {
    const int this_rank = square_rank(sq);
    bitboard_t ranks_above = 0ULL, ranks_below = 0ULL;
    const bitboard_t adjacent_files = isolated_pawn_mask[sq] | file_mask[sq];
    for (int rank = 0; rank < 8; ++rank) {
      if (rank > this_rank)
        ranks_above |= rank_mask[8 * rank];
      if (rank < this_rank)
        ranks_below |= rank_mask[8 * rank];
    }
    result[White][sq] = ranks_above & adjacent_files;
    result[Black][sq] = ranks_below & adjacent_files;
  }
  return result;
}();

// Move generation constants and functions
constexpr bitboard_t not_a_file = 0xfefe'fefe'fefe'fefe;
constexpr bitboard_t not_ab_file = 0xfcfc'fcfc'fcfc'fcfc;
constexpr bitboard_t not_h_file = 0x7f7f'7f7f'7f7f'7f7f;
constexpr bitboard_t not_gh_file = 0x3f3f'3f3f'3f3f'3f3f;
constexpr bitboard_t on_second_rank = 0x0000'0000'0000'FF00;
constexpr bitboard_t on_seventh_rank = 0x00FF'0000'0000'0000;
constexpr bitboard_t white_squares = 0x55AA'55AA'55AA'55AA;
constexpr bitboard_t black_squares = 0xAA55'AA55'AA55'AA55;

extern std::array<std::array<bitboard_t, 64>, 2> pawn_attack_bitboards;
extern std::array<bitboard_t, 64> knight_attack_bitboards;
extern std::array<bitboard_t, 64> king_attack_bitboards;

// bishop_num_relevant_bits[sq] = bit_count(get_bishop_attack_mask(sq));
constexpr std::array<size_t, 64> bishop_num_relevant_bits = {
    6, 5, 5, 5, 5, 5, 5, 6, //
    5, 5, 5, 5, 5, 5, 5, 5, //
    5, 5, 7, 7, 7, 7, 5, 5, //
    5, 5, 7, 9, 9, 7, 5, 5, //
    5, 5, 7, 9, 9, 7, 5, 5, //
    5, 5, 7, 7, 7, 7, 5, 5, //
    5, 5, 5, 5, 5, 5, 5, 5, //
    6, 5, 5, 5, 5, 5, 5, 6, //
};

// rook_num_relevant_bits[sq] = bit_count(get_rook_attack_mask(sq));
constexpr std::array<size_t, 64> rook_num_relevant_bits = {
    12, 11, 11, 11, 11, 11, 11, 12, //
    11, 10, 10, 10, 10, 10, 10, 11, //
    11, 10, 10, 10, 10, 10, 10, 11, //
    11, 10, 10, 10, 10, 10, 10, 11, //
    11, 10, 10, 10, 10, 10, 10, 11, //
    11, 10, 10, 10, 10, 10, 10, 11, //
    11, 10, 10, 10, 10, 10, 10, 11, //
    12, 11, 11, 11, 11, 11, 11, 12, //
};

constexpr inline bitboard_t get_pawn_attack_bitboard(const int side,
                                                     const square_t sq) {
  bitboard_t result = 0;
  const bitboard_t pawn_bitboard = square_to_bitboard(sq);
  if (side == White) {
    result |= (pawn_bitboard << 7) & not_h_file; // Left
    result |= (pawn_bitboard << 9) & not_a_file; // Right
  } else {
    result |= (pawn_bitboard >> 9) & not_h_file; // Left
    result |= (pawn_bitboard >> 7) & not_a_file; // Right
  }
  return result;
}

constexpr inline bitboard_t get_knight_attack_bitboard(const square_t sq) {
  bitboard_t result = 0;
  const bitboard_t knight_bitboard = square_to_bitboard(sq);

  result |= (knight_bitboard << 6) & not_gh_file;
  result |= (knight_bitboard << 10) & not_ab_file;
  result |= (knight_bitboard << 15) & not_h_file;
  result |= (knight_bitboard << 17) & not_a_file;

  result |= (knight_bitboard >> 6) & not_ab_file;
  result |= (knight_bitboard >> 10) & not_gh_file;
  result |= (knight_bitboard >> 15) & not_a_file;
  result |= (knight_bitboard >> 17) & not_h_file;

  return result;
}

constexpr inline bitboard_t get_king_attack_bitboard(const square_t sq) {
  bitboard_t result = 0;
  const bitboard_t king_bitboard = square_to_bitboard(sq);

  result |= (king_bitboard << 7) & not_h_file;
  result |= (king_bitboard << 8);
  result |= (king_bitboard << 9) & not_a_file;
  result |= (king_bitboard << 1) & not_a_file;

  result |= (king_bitboard >> 7) & not_a_file;
  result |= (king_bitboard >> 8);
  result |= (king_bitboard >> 9) & not_h_file;
  result |= (king_bitboard >> 1) & not_h_file;

  return result;
}

constexpr inline bitboard_t get_bishop_attack_mask(const square_t sq) {
  bitboard_t result = 0;
  const std::array<std::pair<int, int>, 4> diagonal_offsets = {
      std::make_pair(1, 1), std::make_pair(-1, -1), std::make_pair(1, -1),
      std::make_pair(-1, 1)};
  for (const auto &[dx, dy] : diagonal_offsets) {
    int row = sq / 8, col = sq % 8;
    while (true) {
      row += dy;
      col += dx;
      if (row < 1 || row > 6 || col < 1 || col > 6)
        break;
      set_bit(result, 8 * row + col);
    }
  }
  return result;
}

constexpr inline bitboard_t get_rook_attack_mask(const square_t sq) {
  bitboard_t result = 0;
  int row = sq / 8, col = sq % 8;
  for (int nc = col + 1; nc <= 6; ++nc)
    set_bit(result, 8 * row + nc);
  for (int nc = col - 1; nc >= 1; --nc)
    set_bit(result, 8 * row + nc);
  for (int nr = row + 1; nr <= 6; ++nr)
    set_bit(result, 8 * nr + col);
  for (int nr = row - 1; nr >= 1; --nr)
    set_bit(result, 8 * nr + col);
  return result;
}

constexpr inline bitboard_t generate_bishop_attacks(const square_t sq,
                                                    const bitboard_t occ) {
  bitboard_t result = 0;
  constexpr std::array<std::pair<int, int>, 4> diagonal_offsets = {
      std::make_pair(1, 1), std::make_pair(-1, -1), std::make_pair(1, -1),
      std::make_pair(-1, 1)};
  for (const auto &[dx, dy] : diagonal_offsets) {
    int row = sq / 8, col = sq % 8;
    while (true) {
      row += dy;
      col += dx;
      if (row < 0 || row > 7 || col < 0 || col > 7)
        break;
      set_bit(result, 8 * row + col);
      if (get_bit(occ, 8 * row + col))
        break;
    }
  }
  return result;
}

constexpr inline bitboard_t generate_rook_attacks(const square_t sq,
                                                  const bitboard_t occ) {
  bitboard_t result = 0;
  int row = sq / 8, col = sq % 8;
  for (int nc = col + 1; nc <= 7; ++nc) {
    set_bit(result, 8 * row + nc);
    if (get_bit(occ, 8 * row + nc))
      break;
  }
  for (int nc = col - 1; nc >= 0; --nc) {
    set_bit(result, 8 * row + nc);
    if (get_bit(occ, 8 * row + nc))
      break;
  }
  for (int nr = row + 1; nr <= 7; ++nr) {
    set_bit(result, 8 * nr + col);
    if (get_bit(occ, 8 * nr + col))
      break;
  }
  for (int nr = row - 1; nr >= 0; --nr) {
    set_bit(result, 8 * nr + col);
    if (get_bit(occ, 8 * nr + col))
      break;
  }
  return result;
}

constexpr inline bitboard_t
set_occupancy(const int index, const int bits_in_mask, bitboard_t attack_mask) {
  bitboard_t occ = 0ULL;

  for (int count = 0; count < bits_in_mask; ++count) {
    const square_t sq = lsb(attack_mask);
    unset_bit(attack_mask, sq);
    if (get_bit(index, count))
      set_bit(occ, sq);
  }
  return occ;
}

// Initialization functions
constexpr inline void init_piece_attack_bitboards() {
  for (square_t sq = 0; sq < 64; ++sq) {
    pawn_attack_bitboards[White][sq] = get_pawn_attack_bitboard(White, sq);
    pawn_attack_bitboards[Black][sq] = get_pawn_attack_bitboard(Black, sq);
    knight_attack_bitboards[sq] = get_knight_attack_bitboard(sq);
    king_attack_bitboards[sq] = get_king_attack_bitboard(sq);
  }
}

constexpr inline void init_bitboards() { init_piece_attack_bitboards(); }
