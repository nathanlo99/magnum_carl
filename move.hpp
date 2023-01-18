
#pragma once

#include <array>
#include <cstdint>
#include <iostream>
#include <iterator>
#include <sstream>
#include <utility>
#include <vector>

#include "piece.hpp"
#include "square.hpp"

enum MoveType : uint8_t {
  Quiet = 0,
  DoublePawn = 1,
  LongCastle = 2,
  ShortCastle = 3,
  Capture = 4,
  EnPassant = 5,

  PromotionKnight = 8,
  PromotionBishop = 9,
  PromotionRook = 10,
  PromotionQueen = 11,
  CapturePromotionKnight = 12,
  CapturePromotionBishop = 13,
  CapturePromotionRook = 14,
  CapturePromotionQueen = 15,
};

constexpr inline std::string_view move_type_to_string(const MoveType type) {
  constexpr std::array<std::string_view, 16> lookup = {"quiet",
                                                       "double_pawn",
                                                       "long_castle",
                                                       "short_castle",
                                                       "capture",
                                                       "enpassant",
                                                       "invalid",
                                                       "invalid",
                                                       "promote_knight",
                                                       "promote_bishop",
                                                       "promote_rook",
                                                       "promote_queen",
                                                       "capture_promote_knight",
                                                       "capture_promote_bishop",
                                                       "capture_promote_rook",
                                                       "capture_promote_queen"};
  return lookup[type];
}

struct Move {
  MoveType type;
  square_t source, target;
  piece_t moved_piece = InvalidPiece;

  constexpr explicit Move()
      : type(Quiet), source(InvalidSquare), target(InvalidSquare),
        moved_piece(InvalidPiece) {}
  constexpr explicit Move(const MoveType type, const square_t source,
                          const square_t target, const piece_t moved_piece)
      : type(type), source(source), target(target), moved_piece(moved_piece) {}

  constexpr bool operator==(const Move &other) const {
    // NOTE: We don't compare the moved_piece, since equality is implied by the
    // source squares being equal
    return type == other.type && source == other.source &&
           target == other.target;
  }

  constexpr bool operator!=(const Move &other) const {
    return !(*this == other);
  }

  constexpr inline bool is_valid() const { return moved_piece != InvalidPiece; }
  constexpr inline bool is_invalid() const {
    return moved_piece == InvalidPiece;
  }

  constexpr inline bool is_capture_promotion() const {
    return type == CapturePromotionKnight || type == CapturePromotionBishop ||
           type == CapturePromotionRook || type == CapturePromotionQueen;
  }
  constexpr inline bool is_some_promotion() const {
    return is_capture_promotion() || type == PromotionKnight ||
           type == PromotionBishop || type == PromotionRook ||
           type == PromotionQueen;
  }
  constexpr inline bool is_capture() const {
    return type == Capture || type == EnPassant || is_capture_promotion();
  }
  constexpr inline piece_t promotion_piece() const {
    assert(is_some_promotion());
    const int side = piece_side(moved_piece);
    if (type == PromotionKnight || type == CapturePromotionKnight)
      return sided_piece(side, WhiteKnight);
    if (type == PromotionBishop || type == CapturePromotionBishop)
      return sided_piece(side, WhiteBishop);
    if (type == PromotionRook || type == CapturePromotionRook)
      return sided_piece(side, WhiteRook);
    if (type == PromotionQueen || type == CapturePromotionQueen)
      return sided_piece(side, WhiteQueen);
    __builtin_unreachable();
  }

  inline std::string to_string() const {
    if (type == LongCastle)
      return "O-O-O";
    if (type == ShortCastle)
      return "O-O";
    std::stringstream ss;
    ss << square_to_string(source);
    if (is_capture())
      ss << "x";
    ss << square_to_string(target);
    if (is_some_promotion())
      ss << piece_to_char(sided_piece<Black>(promotion_piece() % 6));
    return ss.str();
  }

  inline std::string to_algebraic() const {
    if (type == LongCastle)
      return "O-O-O";
    if (type == ShortCastle)
      return "O-O";
    std::stringstream ss;
    ss << square_to_string(source) << square_to_string(target);
    if (is_some_promotion())
      ss << piece_to_char(sided_piece<Black>(promotion_piece() % 6));
    return ss.str();
  }

  inline std::string to_uci() const {
    std::stringstream ss;
    ss << square_to_string(source) << square_to_string(target);
    if (is_some_promotion())
      ss << piece_to_char(sided_piece<Black>(promotion_piece() % 6));
    return ss.str();
  }

  inline std::string to_long_string() const {
    std::stringstream ss;
    ss << "{";
    ss << "moved: " << piece_to_char(moved_piece) << ", ";
    ss << "source: " << square_to_string(source) << ", ";
    ss << "target: " << square_to_string(target) << ", ";
    ss << "type: " << move_type_to_string(type);
    ss << "}";
    return ss.str();
  }

  friend inline std::ostream &operator<<(std::ostream &os, const Move move) {
    return os << move.to_string();
  }
};
