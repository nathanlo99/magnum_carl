
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

  bool is_square_attacked(const int side, const square_t sq) const;
  bitboard_t get_attacked_squares_slow(const int side) const;
  bool current_player_in_check() const;

  Move *pseudo_captures(Move *list) const;
  Move *pseudo_quiet_moves(Move *list) const;
  Move *pseudo_promotions(Move *list) const;
  Move *pseudo_moves(Move *list) const;
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
  std::vector<Move> legal_moves_slow();

  hash_t compute_hash_slow() const;
  void check_invariants(const std::string &message = "") const;

  bool make_move(const Move move);
  void unmake_move(const Move move);
  bool has_legal_capture();

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
