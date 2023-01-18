
#pragma once

#include <array>
#include <cstdint>
#include <string>

using square_t = uint8_t;

// Enumerate the board squares for programatic use
enum : square_t {
  A1 = 0,
  B1,
  C1,
  D1,
  E1,
  F1,
  G1,
  H1,
  A2,
  B2,
  C2,
  D2,
  E2,
  F2,
  G2,
  H2,
  A3,
  B3,
  C3,
  D3,
  E3,
  F3,
  G3,
  H3,
  A4,
  B4,
  C4,
  D4,
  E4,
  F4,
  G4,
  H4,
  A5,
  B5,
  C5,
  D5,
  E5,
  F5,
  G5,
  H5,
  A6,
  B6,
  C6,
  D6,
  E6,
  F6,
  G6,
  H6,
  A7,
  B7,
  C7,
  D7,
  E7,
  F7,
  G7,
  H7,
  A8,
  B8,
  C8,
  D8,
  E8,
  F8,
  G8,
  H8,
  InvalidSquare = 255,
};

constexpr std::array<int, 64> square_colour_table = {
    1, 0, 1, 0, 1, 0, 1, 0, //
    0, 1, 0, 1, 0, 1, 0, 1, //
    1, 0, 1, 0, 1, 0, 1, 0, //
    0, 1, 0, 1, 0, 1, 0, 1, //
    1, 0, 1, 0, 1, 0, 1, 0, //
    0, 1, 0, 1, 0, 1, 0, 1, //
    1, 0, 1, 0, 1, 0, 1, 0, //
    0, 1, 0, 1, 0, 1, 0, 1, //
};

constexpr inline square_t string_to_square(const std::string_view &s) {
  if (s.size() == 1)
    return InvalidSquare;
  const auto row = s[0] - 'a', col = s[1] - '1';
  return row * 8 + col;
}

constexpr inline int square_file(const square_t sq) { return sq % 8; }
constexpr inline int square_rank(const square_t sq) { return sq / 8; }
constexpr inline char file_to_char(const int file) { return 'a' + file; }
constexpr inline char rank_to_char(const int rank) { return '1' + rank; }

inline std::string square_to_string(const square_t sq) {
  if (sq == InvalidSquare)
    return "-";
  const uint8_t row = sq / 8, col = sq % 8;
  return {static_cast<char>(col + 'a'), static_cast<char>(row + '1')};
}

constexpr inline bool square_on_first_rank(const square_t sq) {
  return 0 <= sq && sq <= 7;
}

constexpr inline bool square_on_second_rank(const square_t sq) {
  return 8 <= sq && sq <= 15;
}

constexpr inline bool square_on_seventh_rank(const square_t sq) {
  return 48 <= sq && sq <= 55;
}

constexpr inline bool square_on_eighth_rank(const square_t sq) {
  return 56 <= sq && sq <= 63;
}

constexpr inline int square_colour(const square_t sq) {
  return square_colour_table[sq];
}
