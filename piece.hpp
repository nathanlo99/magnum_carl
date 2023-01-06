
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

constexpr std::array<std::array<int, 64>, 6> default_piece_square_table = {
    // WhitePawn
    std::array<int, 64>{
        0,   0,   0,   0,   0,   0,   0,   0,   //
        100, 100, 100, 100, 100, 100, 100, 100, //
        100, 100, 100, 100, 100, 100, 100, 100, //
        100, 100, 100, 100, 100, 100, 100, 100, //
        105, 110, 120, 125, 125, 120, 110, 105, //
        150, 150, 150, 150, 150, 150, 150, 150, //
        250, 250, 250, 250, 250, 250, 250, 250, //
        0,   0,   0,   0,   0,   0,   0,   0    //
    },
    // WhiteKnight
    std::array<int, 64>{
        300, 300, 300, 300, 300, 300, 300, 300, //
        300, 300, 300, 300, 300, 300, 300, 300, //
        300, 300, 300, 300, 300, 300, 300, 300, //
        300, 300, 300, 300, 300, 300, 300, 300, //
        300, 300, 300, 300, 300, 300, 300, 300, //
        300, 300, 300, 300, 300, 300, 300, 300, //
        300, 300, 300, 300, 300, 300, 300, 300, //
        300, 300, 300, 300, 300, 300, 300, 300  //
    },
    // WhiteBishop
    std::array<int, 64>{
        350, 350, 350, 350, 350, 350, 350, 350, //
        350, 350, 350, 350, 350, 350, 350, 350, //
        350, 350, 350, 350, 350, 350, 350, 350, //
        350, 350, 350, 350, 350, 350, 350, 350, //
        350, 350, 350, 350, 350, 350, 350, 350, //
        350, 350, 350, 350, 350, 350, 350, 350, //
        350, 350, 350, 350, 350, 350, 350, 350, //
        350, 350, 350, 350, 350, 350, 350, 350  //
    },
    // WhiteRook
    std::array<int, 64>{
        500, 500, 500, 500, 500, 500, 500, 500, //
        500, 500, 500, 500, 500, 500, 500, 500, //
        500, 500, 500, 500, 500, 500, 500, 500, //
        500, 500, 500, 500, 500, 500, 500, 500, //
        500, 500, 500, 500, 500, 500, 500, 500, //
        500, 500, 500, 500, 500, 500, 500, 500, //
        500, 500, 500, 500, 500, 500, 500, 500, //
        500, 500, 500, 500, 500, 500, 500, 500  //
    },
    // WhiteQueen
    std::array<int, 64>{
        900, 900, 900, 900, 900, 900, 900, 900, //
        900, 900, 900, 900, 900, 900, 900, 900, //
        900, 900, 900, 900, 900, 900, 900, 900, //
        900, 900, 900, 900, 900, 900, 900, 900, //
        900, 900, 900, 900, 900, 900, 900, 900, //
        900, 900, 900, 900, 900, 900, 900, 900, //
        900, 900, 900, 900, 900, 900, 900, 900, //
        900, 900, 900, 900, 900, 900, 900, 900  //
    },
    // WhiteKing
    std::array<int, 64>{
        0, 0, 0, 0, 0, 0, 0, 0, //
        0, 0, 0, 0, 0, 0, 0, 0, //
        0, 0, 0, 0, 0, 0, 0, 0, //
        0, 0, 0, 0, 0, 0, 0, 0, //
        0, 0, 0, 0, 0, 0, 0, 0, //
        0, 0, 0, 0, 0, 0, 0, 0, //
        0, 0, 0, 0, 0, 0, 0, 0, //
        0, 0, 0, 0, 0, 0, 0, 0  //
    },
};

using piece_t = uint8_t;

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
