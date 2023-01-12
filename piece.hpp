
#pragma once

#include <array>
#include <cstdint>
#include <string>

#include "util.hpp"

enum Piece {
  WhitePawn = 0,
  WhiteKnight,
  WhiteBishop,
  WhiteRook,
  WhiteQueen,
  WhiteKing,
  BlackPawn = 6,
  BlackKnight,
  BlackBishop,
  BlackRook,
  BlackQueen,
  BlackKing,
  InvalidPiece = 12,
};

constexpr std::array<int, 12> normalized_piece_value_table = {0, 1, 2, 3, 4, 5,
                                                              0, 1, 2, 3, 4, 5};
constexpr std::array<int, 6> piece_value_table = {100, 320, 330, 500, 900, 0};

constexpr std::array<std::array<int, 64>, 6> half_piece_square_table = {
    // WhitePawn
    std::array<int, 64>{
        0,  0,  0,   0,   0,   0,   0,  0,  //
        50, 50, 50,  50,  50,  50,  50, 50, //
        10, 10, 20,  30,  30,  20,  10, 10, //
        5,  5,  10,  25,  25,  10,  5,  5,  //
        0,  0,  0,   20,  20,  0,   0,  0,  //
        5,  -5, -10, 0,   0,   -10, -5, 5,  //
        5,  10, 10,  -20, -20, 10,  10, 5,  //
        0,  0,  0,   0,   0,   0,   0,  0,  //

    },
    // WhiteKnight
    std::array<int, 64>{
        -50, -40, -30, -30, -30, -30, -40, -50, //
        -40, -20, 0,   0,   0,   0,   -20, -40, //
        -30, 0,   10,  15,  15,  10,  0,   -30, //
        -30, 5,   15,  20,  20,  15,  5,   -30, //
        -30, 0,   15,  20,  20,  15,  0,   -30, //
        -30, 5,   10,  15,  15,  10,  5,   -30, //
        -40, -20, 0,   5,   5,   0,   -20, -40, //
        -50, -40, -30, -30, -30, -30, -40, -50, //
    },
    // WhiteBishop
    std::array<int, 64>{
        -20, -10, -10, -10, -10, -10, -10, -20, //
        -10, 0,   0,   0,   0,   0,   0,   -10, //
        -10, 0,   5,   10,  10,  5,   0,   -10, //
        -10, 5,   5,   10,  10,  5,   5,   -10, //
        -10, 0,   10,  10,  10,  10,  0,   -10, //
        -10, 10,  10,  10,  10,  10,  10,  -10, //
        -10, 5,   0,   0,   0,   0,   5,   -10, //
        -20, -10, -10, -10, -10, -10, -10, -20, //
    },
    // WhiteRook
    std::array<int, 64>{
        0,  0,  0,  0,  0,  0,  0,  0,  //
        5,  10, 10, 10, 10, 10, 10, 5,  //
        -5, 0,  0,  0,  0,  0,  0,  -5, //
        -5, 0,  0,  0,  0,  0,  0,  -5, //
        -5, 0,  0,  0,  0,  0,  0,  -5, //
        -5, 0,  0,  0,  0,  0,  0,  -5, //
        -5, 0,  0,  0,  0,  0,  0,  -5, //
        0,  0,  0,  5,  5,  0,  0,  0,  //
    },
    // WhiteQueen
    std::array<int, 64>{
        -20, -10, -10, -5, -5, -10, -10, -20, //
        -10, 0,   0,   0,  0,  0,   0,   -10, //
        -10, 0,   5,   5,  5,  5,   0,   -10, //
        -5,  0,   5,   5,  5,  5,   0,   -5,  //
        0,   0,   5,   5,  5,  5,   0,   -5,  //
        -10, 5,   5,   5,  5,  5,   0,   -10, //
        -10, 0,   5,   0,  0,  0,   0,   -10, //
        -20, -10, -10, -5, -5, -10, -10, -20, //
    },
    // WhiteKing
    std::array<int, 64>{
        -30, -40, -40, -50, -50, -40, -40, -30, //
        -30, -40, -40, -50, -50, -40, -40, -30, //
        -30, -40, -40, -50, -50, -40, -40, -30, //
        -30, -40, -40, -50, -50, -40, -40, -30, //
        -20, -30, -30, -40, -40, -30, -30, -20, //
        -10, -20, -20, -20, -20, -20, -20, -10, //
        20,  20,  0,   0,   0,   0,   20,  20,  //
        20,  30,  10,  0,   0,   10,  30,  20,  //
    },
};

using piece_t = uint8_t;

constexpr std::array<std::array<int, 64>, 12> piece_square_table = []() {
  std::array<std::array<int, 64>, 12> result;
  for (piece_t piece = WhitePawn; piece <= WhiteKing; ++piece) {
    for (size_t sq = 0; sq < 64; ++sq) { // square_t not yet defined
      result[piece][sq] =
          piece_value_table[piece] + half_piece_square_table[piece][sq ^ 56];
      result[piece + 6][sq ^ 56] = -result[piece][sq];
    }
  }
  return result;
}();

constexpr const char *encoding = "PNBRQKpnbrqk ";
constexpr inline piece_t char_to_piece(const char c) {
  for (piece_t p = 0; p < InvalidPiece; ++p) {
    if (encoding[p] == c)
      return p;
  }
  return InvalidPiece;
}

constexpr inline char piece_to_char(const piece_t p) { return encoding[p]; }
constexpr inline bool piece_side(const piece_t p) { return p >= BlackPawn; }
constexpr inline bool piece_is_pawn(const piece_t p) {
  return p == WhitePawn || p == BlackPawn;
}

template <int side>
constexpr inline piece_t sided_piece(const piece_t white_piece) {
  static_assert(side == White || side == Black);
  assert(piece_side(white_piece) == White);
  return white_piece + side * 6;
}

constexpr inline piece_t sided_piece(const int side,
                                     const piece_t white_piece) {
  assert(side == White || side == Black);
  assert(piece_side(white_piece) == White);
  return white_piece + side * 6;
}
