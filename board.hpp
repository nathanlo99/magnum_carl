
#pragma once

#include <iostream>
#include <string>
#include <vector>

#include "bitboard.hpp"
#include "castle_perms.hpp"
#include "hash.hpp"
#include "magic.hpp"
#include "move.hpp"
#include "piece.hpp"
#include "square.hpp"
#include "util.hpp"

enum GameResult {
  // Decisive results
  WhiteCheckmate,
  BlackCheckmate,

  // Draws
  Stalemate,
  DrawByRepetition,
  DrawByFiftyMove,
  DrawByInsufficientMaterial,

  // No result yet, game is still going
  InProgress,
};

constexpr inline std::string_view result_to_string(const GameResult result) {
  switch (result) {
  case WhiteCheckmate:
    return "White wins by checkmate";
  case BlackCheckmate:
    return "Black wins by checkmate";
  case Stalemate:
    return "Draw by stalemate";
  case DrawByRepetition:
    return "Draw by repetition";
  case DrawByFiftyMove:
    return "Draw by fifty-move rule";
  case DrawByInsufficientMaterial:
    return "Draw by insufficient material";
  case InProgress:
    return "Game in progress";
  }
  return "Unknown result";
}

constexpr inline double result_to_double(const GameResult result) {
  switch (result) {
  case WhiteCheckmate:
    return 1.0;
  case BlackCheckmate:
    return -1.0;
  case Stalemate:
  case DrawByRepetition:
  case DrawByFiftyMove:
  case DrawByInsufficientMaterial:
    return 0.0;
  default:
    return -1000.0;
  }
}

struct History {
  // Set the default values to clearly invalid states for debugging purposes
  hash_t hash = 0ULL;
  square_t en_passant = InvalidSquare;
  piece_t captured_piece = InvalidPiece;
  uint8_t fifty_move = 127;
  castle_t castle_perms = 37;

  constexpr History() = default;
  constexpr History(const hash_t hash, const square_t en_passant,
                    const piece_t captured_piece, const uint8_t fifty_move,
                    const castle_t castle_perms)
      : hash(hash), en_passant(en_passant), captured_piece(captured_piece),
        fifty_move(fifty_move), castle_perms(castle_perms) {}
};

struct Board {
  static constexpr const char *start_fen =
      "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

  // Not a legal chess position, but a helpful starting point for
  // programmatically setting up positions
  static constexpr const char *white_empty_fen = "8/8/8/8/8/8/8/8 w - - 0 1";
  static constexpr const char *black_empty_fen = "8/8/8/8/8/8/8/8 b - - 0 1";

  static constexpr const size_t max_moves_in_game = 512;
  static constexpr const size_t max_moves_in_position = 256;

public:
  // Standard data
  bool m_side_to_move = White;
  uint8_t m_fifty_move = 0;
  uint16_t m_ply = 0;
  square_t m_en_passant_sq = InvalidSquare;
  castle_t m_castle_perms = 15;
  int m_material_evaluation = 0;

  std::array<bitboard_t, 12> m_bitboards = {0};
  std::array<bitboard_t, 3> m_occupancies = {0};

  hash_t m_hash = 0ULL;

  std::array<History, max_moves_in_game> m_history;

public:
  // Methods
  explicit Board(const std::string_view &fen = Board::start_fen);

  int get_piece_value(const piece_t piece, const square_t square) const;
  int static_evaluate() const;

  template <bool update_hash>
  void move_piece(const piece_t piece, const square_t source,
                  const square_t target);
  template <bool update_hash>
  void place_piece(const piece_t piece, const square_t square);
  template <bool update_hash>
  void remove_piece(const piece_t piece, const square_t sq);
  template <bool update_hash> piece_t remove_piece_slow(const square_t sq);
  piece_t piece_at_slow(const square_t sq) const;
  void set_en_passant_sq(const square_t sq);

  template <int side> bool is_square_attacked(const square_t sq) const;
  template <int side> bitboard_t get_attacked_squares_slow() const;
  bool current_player_in_check() const;

  template <int this_side> Move *pseudo_captures(Move *list) const;
  template <int this_side> Move *pseudo_quiet_moves(Move *list) const;
  template <int this_side> Move *pseudo_promotions(Move *list) const;
  template <int this_side> Move *pseudo_moves(Move *list) const;
  template <int this_side>
  Move *pseudo_captures_and_promotions(Move *list) const;

  // Generates the legal moves
  // NOTE: These functions are logically const, since they leave the board state
  // the same as before the function is called, but because we test moves by
  // playing them then undoing them, this is not physically const.
  //
  // Marking them non-const is a compromise as the alternative is to make every
  // other member variable mutable
  Move *legal_moves(Move *list);
  Move *legal_captures(Move *list);

  hash_t compute_hash_slow() const;
  void check_invariants(const std::string &message = "") const;

  bool make_move(const Move move);
  void unmake_move(const Move move);

  int repeat_count() const;
  bool is_drawn_by_repetition() const;
  bool is_drawn_by_fifty_move() const;
  bool is_drawn_by_insufficient_material() const;
  std::pair<bool, int> compute_simple_evaluation() const;
  GameResult check_result();

  std::ostream &print(std::ostream &os) const;
  std::string to_fen() const;
  std::string to_lichess_link() const;

private:
  template <piece_t piece>
  Move *generate_captures(Move *list, const bitboard_t other_occupancies,
                          const bitboard_t both_occupancies) const;
  template <piece_t piece>
  Move *generate_quiet_moves(Move *list, const bitboard_t empty_squares,
                             const bitboard_t both_occupancies) const;
};

inline std::ostream &operator<<(std::ostream &os, const Board &board) {
  return board.print(os);
}

// ==================== TEMPLATED FUNCTION IMPLEMENTATIONS ====================

// Returns true if the given square is attacked by any piece on the given side
template <int side> bool Board::is_square_attacked(const square_t sq) const {
  static_assert(side == White || side == Black);
  constexpr int other_side = ::other_side(side);
  constexpr piece_t my_pawn = sided_piece<side>(WhitePawn);
  constexpr piece_t my_knight = sided_piece<side>(WhiteKnight);
  constexpr piece_t my_bishop = sided_piece<side>(WhiteBishop);
  constexpr piece_t my_rook = sided_piece<side>(WhiteRook);
  constexpr piece_t my_queen = sided_piece<side>(WhiteQueen);
  constexpr piece_t my_king = sided_piece<side>(WhiteKing);
  if (get_rook_attacks(sq, m_occupancies[Both]) &
      (m_bitboards[my_rook] | m_bitboards[my_queen]))
    return true;
  if (get_bishop_attacks(sq, m_occupancies[Both]) &
      (m_bitboards[my_bishop] | m_bitboards[my_queen]))
    return true;
  if (knight_attack_bitboards[sq] & m_bitboards[my_knight])
    return true;
  if (pawn_attack_bitboards[other_side][sq] & m_bitboards[my_pawn])
    return true;
  if (king_attack_bitboards[sq] & m_bitboards[my_king])
    return true;
  return false;
}

// Returns a bitboard, where the bit corresponding to a square is set if and
// only if the square is attacked by the given side
template <int side> bitboard_t Board::get_attacked_squares_slow() const {
  bitboard_t attacked_mask = 0;
  for (square_t sq = 0; sq < 64; ++sq) {
    if (is_square_attacked<side>(sq))
      set_bit(attacked_mask, sq);
  }
  return attacked_mask;
}

template <piece_t piece>
Move *Board::generate_captures(Move *list, const bitboard_t other_occupancies,
                               const bitboard_t both_occupancies) const {
  bitboard_t piece_bitboard = m_bitboards[piece];
  while (piece_bitboard) {
    const square_t source = pop_bit(piece_bitboard);
    bitboard_t attack_bitboard =
        get_attacks<piece>(source, both_occupancies) & other_occupancies;
    while (attack_bitboard) {
      const square_t target = pop_bit(attack_bitboard);
      *list++ = Move(Capture, source, target, piece);
    }
  }
  return list;
}

template <piece_t piece>
Move *Board::generate_quiet_moves(Move *list, const bitboard_t empty_squares,
                                  const bitboard_t both_occupancies) const {
  bitboard_t piece_bitboard = m_bitboards[piece];
  while (piece_bitboard) {
    const square_t source = pop_bit(piece_bitboard);
    bitboard_t attack_bitboard =
        get_attacks<piece>(source, both_occupancies) & empty_squares;
    while (attack_bitboard) {
      const square_t target = pop_bit(attack_bitboard);
      *list++ = Move(Quiet, source, target, piece);
    }
  }
  return list;
}

// Returns the pseudo-legal capture moves, which are moves the pieces could
// legally make, but could possibly put the king in check
template <int this_side> Move *Board::pseudo_captures(Move *list) const {
  constexpr int other_side = ::other_side(this_side);

  if (this_side == White) {
    // Generate white pawn captures
    bitboard_t white_pawns = m_bitboards[WhitePawn];

    // Handle en-passant while we have the full set of white pawns
    if (m_en_passant_sq != InvalidSquare) {
      bitboard_t source_bitboard =
          white_pawns & pawn_attack_bitboards[other_side][m_en_passant_sq];
      while (source_bitboard) {
        *list++ = Move(EnPassant, pop_bit(source_bitboard), m_en_passant_sq,
                       WhitePawn);
      }
    }

    while (white_pawns) {
      const square_t source = pop_bit(white_pawns);
      bitboard_t attacked =
          pawn_attack_bitboards[White][source] & m_occupancies[Black];
      while (attacked) {
        const square_t target = pop_bit(attacked);
        if (square_on_eighth_rank(target)) {
          *list++ = Move(CapturePromotionQueen, source, target, WhitePawn);
          *list++ = Move(CapturePromotionRook, source, target, WhitePawn);
          *list++ = Move(CapturePromotionBishop, source, target, WhitePawn);
          *list++ = Move(CapturePromotionKnight, source, target, WhitePawn);
        } else {
          *list++ = Move(Capture, source, target, WhitePawn);
        }
      }
    }
  } else {
    // Generate black pawn captures
    bitboard_t black_pawns = m_bitboards[BlackPawn];

    // Handle en-passant while we have the full set of black pawns
    if (m_en_passant_sq != InvalidSquare) {
      bitboard_t source_bitboard =
          black_pawns & pawn_attack_bitboards[other_side][m_en_passant_sq];
      while (source_bitboard) {
        *list++ = Move(EnPassant, pop_bit(source_bitboard), m_en_passant_sq,
                       BlackPawn);
      }
    }

    while (black_pawns) {
      const square_t source = pop_bit(black_pawns);
      bitboard_t attacked =
          pawn_attack_bitboards[Black][source] & m_occupancies[White];
      while (attacked) {
        const square_t target = pop_bit(attacked);
        // If the target is on the 1st rank, then generate 4 capture
        // promotions
        if (square_on_first_rank(target)) {
          *list++ = Move(CapturePromotionQueen, source, target, BlackPawn);
          *list++ = Move(CapturePromotionRook, source, target, BlackPawn);
          *list++ = Move(CapturePromotionBishop, source, target, BlackPawn);
          *list++ = Move(CapturePromotionKnight, source, target, BlackPawn);
        } else {
          *list++ = Move(Capture, source, target, BlackPawn);
        }
      }
    }
  }

  const bitboard_t other_occ = m_occupancies[other_side],
                   both_occ = m_occupancies[Both];

  // NOTE: Generate captures in this order to simulate MVV-LVA move ordering
  if (this_side == White) {
    list = generate_captures<WhiteKnight>(list, other_occ, both_occ);
    list = generate_captures<WhiteBishop>(list, other_occ, both_occ);
    list = generate_captures<WhiteRook>(list, other_occ, both_occ);
    list = generate_captures<WhiteQueen>(list, other_occ, both_occ);
    list = generate_captures<WhiteKing>(list, other_occ, both_occ);
  } else {
    list = generate_captures<BlackKnight>(list, other_occ, both_occ);
    list = generate_captures<BlackBishop>(list, other_occ, both_occ);
    list = generate_captures<BlackRook>(list, other_occ, both_occ);
    list = generate_captures<BlackQueen>(list, other_occ, both_occ);
    list = generate_captures<BlackKing>(list, other_occ, both_occ);
  }
  return list;
}

// Generate the pseudo-legal non-capture moves
template <int this_side> Move *Board::pseudo_quiet_moves(Move *list) const {
  constexpr int other_side = ::other_side(this_side);
  const bitboard_t both_occ = m_occupancies[Both], empty_squares = ~both_occ;

  if (this_side == White) {
    // Castling
    if (m_castle_perms & (WhiteShortCastle | WhiteLongCastle) &&
        !is_square_attacked<other_side>(E1)) {
      constexpr bitboard_t short_castle_mask =
          square_to_bitboard(F1) | square_to_bitboard(G1);
      constexpr bitboard_t long_castle_mask = square_to_bitboard(D1) |
                                              square_to_bitboard(C1) |
                                              square_to_bitboard(B1);
      if (m_castle_perms & WhiteShortCastle &&
          !(both_occ & short_castle_mask) &&
          !is_square_attacked<other_side>(F1)) {
        *list++ = Move(ShortCastle, E1, G1, WhiteKing);
      }
      if (m_castle_perms & WhiteLongCastle && !(both_occ & long_castle_mask) &&
          !is_square_attacked<other_side>(D1)) {
        *list++ = Move(LongCastle, E1, C1, WhiteKing);
      }
    }

    // White pawn quiet moves: single and double pawn moves, promotions
    bitboard_t white_pawns = m_bitboards[WhitePawn] & (empty_squares >> 8);
    bitboard_t white_double_pawns =
        white_pawns & on_second_rank & (empty_squares >> 16);
    bitboard_t white_non_promote = white_pawns & ~on_seventh_rank;
    bitboard_t white_promote = white_pawns & on_seventh_rank;
    while (white_non_promote) {
      const square_t source = pop_bit(white_non_promote);
      const square_t target = source + 8;
      *list++ = Move(Quiet, source, target, WhitePawn);
    }
    while (white_promote) {
      const square_t source = pop_bit(white_promote);
      const square_t target = source + 8;
      *list++ = Move(PromotionQueen, source, target, WhitePawn);
      *list++ = Move(PromotionRook, source, target, WhitePawn);
      *list++ = Move(PromotionBishop, source, target, WhitePawn);
      *list++ = Move(PromotionKnight, source, target, WhitePawn);
    }
    while (white_double_pawns) {
      const square_t source = pop_bit(white_double_pawns),
                     second_target = source + 16;
      *list++ = Move(DoublePawn, source, second_target, WhitePawn);
    }
  } else {
    // Castling
    if (m_castle_perms & (BlackShortCastle | BlackLongCastle) &&
        !is_square_attacked<other_side>(E8)) {
      constexpr bitboard_t short_castle_mask =
          square_to_bitboard(F8) | square_to_bitboard(G8);
      constexpr bitboard_t long_castle_mask = square_to_bitboard(D8) |
                                              square_to_bitboard(C8) |
                                              square_to_bitboard(B8);
      if (m_castle_perms & BlackShortCastle &&
          !(both_occ & short_castle_mask) &&
          !is_square_attacked<other_side>(F8)) {
        *list++ = Move(ShortCastle, E8, G8, BlackKing);
      }
      if (m_castle_perms & BlackLongCastle && !(both_occ & long_castle_mask) &&
          !is_square_attacked<other_side>(D8)) {
        *list++ = Move(LongCastle, E8, C8, BlackKing);
      }
    }

    // Black pawn quiet moves: single and double pawn moves, promotions
    bitboard_t black_pawns = m_bitboards[BlackPawn] & (empty_squares << 8);
    bitboard_t black_double_pawns =
        black_pawns & on_seventh_rank & (empty_squares << 16);
    bitboard_t black_non_promote = black_pawns & ~on_second_rank;
    bitboard_t black_promote = black_pawns & on_second_rank;
    while (black_non_promote) {
      const square_t source = pop_bit(black_non_promote);
      const square_t target = source - 8;
      *list++ = Move(Quiet, source, target, BlackPawn);
    }

    while (black_promote) {
      const square_t source = pop_bit(black_promote);
      const square_t target = source - 8;
      *list++ = Move(PromotionQueen, source, target, BlackPawn);
      *list++ = Move(PromotionRook, source, target, BlackPawn);
      *list++ = Move(PromotionBishop, source, target, BlackPawn);
      *list++ = Move(PromotionKnight, source, target, BlackPawn);
    }

    while (black_double_pawns) {
      const square_t source = pop_bit(black_double_pawns);
      *list++ = Move(DoublePawn, source, source - 16, BlackPawn);
    }
  }

  if constexpr (this_side == White) {
    list = generate_quiet_moves<WhiteKing>(list, empty_squares, both_occ);
    list = generate_quiet_moves<WhiteKnight>(list, empty_squares, both_occ);
    list = generate_quiet_moves<WhiteBishop>(list, empty_squares, both_occ);
    list = generate_quiet_moves<WhiteRook>(list, empty_squares, both_occ);
    list = generate_quiet_moves<WhiteQueen>(list, empty_squares, both_occ);
  } else {
    list = generate_quiet_moves<BlackKing>(list, empty_squares, both_occ);
    list = generate_quiet_moves<BlackKnight>(list, empty_squares, both_occ);
    list = generate_quiet_moves<BlackBishop>(list, empty_squares, both_occ);
    list = generate_quiet_moves<BlackRook>(list, empty_squares, both_occ);
    list = generate_quiet_moves<BlackQueen>(list, empty_squares, both_occ);
  }
  return list;
}

template <int this_side> Move *Board::pseudo_promotions(Move *list) const {
  const bitboard_t both_occ = m_occupancies[Both], empty_squares = ~both_occ;

  if (this_side == White) {
    // White pawn quiet moves: single and double pawn moves, promotions
    const bitboard_t white_pawns =
        m_bitboards[WhitePawn] & (empty_squares >> 8);
    bitboard_t white_promote = white_pawns & on_seventh_rank;
    while (white_promote) {
      const square_t source = pop_bit(white_promote);
      const square_t target = source + 8;
      *list++ = Move(PromotionQueen, source, target, WhitePawn);
      *list++ = Move(PromotionRook, source, target, WhitePawn);
      *list++ = Move(PromotionBishop, source, target, WhitePawn);
      *list++ = Move(PromotionKnight, source, target, WhitePawn);
    }
  } else {
    // Black pawn promotions
    const bitboard_t black_pawns =
        m_bitboards[BlackPawn] & (empty_squares << 8);
    bitboard_t black_promote = black_pawns & on_second_rank;

    while (black_promote) {
      const square_t source = pop_bit(black_promote);
      const square_t target = source - 8;
      *list++ = Move(PromotionQueen, source, target, BlackPawn);
      *list++ = Move(PromotionRook, source, target, BlackPawn);
      *list++ = Move(PromotionBishop, source, target, BlackPawn);
      *list++ = Move(PromotionKnight, source, target, BlackPawn);
    }
  }
  return list;
}

// Generate all pseudo-legal captures and queen promotions
template <int this_side>
Move *Board::pseudo_captures_and_promotions(Move *list) const {
  list = pseudo_captures<this_side>(list);
  list = pseudo_promotions<this_side>(list);
  return list;
}

// Generate all the pseudo-legal chess moves
template <int this_side> Move *Board::pseudo_moves(Move *list) const {
  list = pseudo_captures<this_side>(list);
  list = pseudo_quiet_moves<this_side>(list);
  return list;
}

// Move the supplied piece from the given source square to the given target
// square, maintaining the occupancy invariants along the way
template <bool update_hash>
void Board::move_piece(const piece_t piece, const square_t source,
                       const square_t target) {
  const int side = piece_side(piece);
  assert(get_bit(m_bitboards[piece], source));
  assert(get_bit(m_occupancies[side], source));
  assert(get_bit(m_occupancies[Both], source));
  assert(!get_bit(m_bitboards[piece], target));
  assert(!get_bit(m_occupancies[side], target));
  assert(!get_bit(m_occupancies[Both], target));
  const bitboard_t toggle =
      square_to_bitboard(source) | square_to_bitboard(target);
  m_bitboards[piece] ^= toggle;
  m_occupancies[side] ^= toggle;
  m_occupancies[Both] ^= toggle;
  m_material_evaluation -= get_piece_value(piece, source);
  m_material_evaluation += get_piece_value(piece, target);
  if constexpr (update_hash)
    m_hash ^= piece_keys[piece][source] ^ piece_keys[piece][target];
}

// Places a piece onto the given square, maintaining the occupancy invariants
// along the way
template <bool update_hash>
void Board::place_piece(const piece_t piece, const square_t sq) {
  const int side = piece_side(piece);
  assert(!get_bit(m_bitboards[piece], sq));
  assert(!get_bit(m_occupancies[side], sq));
  assert(!get_bit(m_occupancies[Both], sq));
  set_bit(m_bitboards[piece], sq);
  set_bit(m_occupancies[side], sq);
  set_bit(m_occupancies[Both], sq);
  m_material_evaluation += get_piece_value(piece, sq);
  if constexpr (update_hash)
    m_hash ^= piece_keys[piece][sq];
}

// Remove an unknown piece from a given square, looping over all the bitboards
// NOTE: This is generally slow, if the piece is known, use the version below
template <bool update_hash>
piece_t Board::remove_piece_slow(const square_t sq) {
  assert(get_bit(m_occupancies[Both], sq));
  for (piece_t piece = 0; piece < InvalidPiece; ++piece) {
    if (get_bit(m_bitboards[piece], sq)) {
      remove_piece<update_hash>(piece, sq);
      return piece;
    }
  }
  __builtin_unreachable();
}

// Remove a known piece from a given square, maintaining the occupancy
// invariants along the way
template <bool update_hash>
void Board::remove_piece(const piece_t piece, const square_t sq) {
  const int side = piece_side(piece);
  assert(get_bit(m_bitboards[piece], sq));
  assert(get_bit(m_occupancies[side], sq));
  assert(get_bit(m_occupancies[Both], sq));
  unset_bit(m_bitboards[piece], sq);
  unset_bit(m_occupancies[side], sq);
  unset_bit(m_occupancies[Both], sq);
  m_material_evaluation -= get_piece_value(piece, sq);
  if constexpr (update_hash)
    m_hash ^= piece_keys[piece][sq];
}
