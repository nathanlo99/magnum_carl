

#include "board.hpp"

#include <array>
#include <cassert>
#include <iostream>
#include <iterator>
#include <string_view>

#include "bitboard.hpp"
#include "castle_perms.hpp"
#include "hash.hpp"
#include "magic.hpp"
#include "move.hpp"
#include "piece.hpp"
#include "square.hpp"
#include "util.hpp"

// Given a string encoded in FEN notation, construct a Board
Board::Board(const std::string_view &fen) {
  size_t fen_idx = 0, fen_size = fen.size();

  // Parse the first part of the FEN, encoding the pieces on the board
  int row = 7, col = 0;

  for (fen_idx = 0; fen_idx < fen_size; ++fen_idx) {
    const char cur_char = fen[fen_idx];
    if (cur_char == ' ') {
      assert(col == 8 && row == 0);
      break;
    } else if (cur_char == '/') {
      assert(col == 8 && row > 0);
      row--;
      col = 0;
      continue;
    } else if ('1' <= cur_char && cur_char <= '8') {
      const int pawn_count = cur_char - '0';
      col += pawn_count;
      assert(col <= 8);
    } else {
      const piece_t piece = char_to_piece(cur_char);
      place_piece<true>(piece, 8 * row + col);
      col++;
    }
  }
  fen_idx++; // Skip the space

  // Parse the side to move: either w or b
  assert(fen_idx < fen_size && (fen[fen_idx] == 'w' || fen[fen_idx] == 'b'));
  m_side_to_move = fen[fen_idx] == 'w' ? White : Black;
  m_hash ^= m_side_to_move == White ? 0 : side_key;
  fen_idx += 2; // Advance past the side and space

  // Parse the castle permissions, either some subset of KQkq or a single dash
  assert(fen_idx < fen_size);
  m_castle_perms = 0;
  for (; fen_idx < fen_size; ++fen_idx) {
    const char cur_char = fen[fen_idx];
    if (cur_char == ' ')
      break;
    else if (cur_char == '-')
      continue;
    else if (cur_char == 'K')
      m_castle_perms |= WhiteShortCastle;
    else if (cur_char == 'Q')
      m_castle_perms |= WhiteLongCastle;
    else if (cur_char == 'k')
      m_castle_perms |= BlackShortCastle;
    else if (cur_char == 'q')
      m_castle_perms |= BlackLongCastle;
    else
      assert(false && "Invalid castle perm specification");
  }
  m_hash ^= castle_keys[m_castle_perms];
  fen_idx++;

  // Parse the en-passant square, which is either - or supplied in algebraic
  // notation
  if (fen[fen_idx] == '-') {
    m_en_passant_sq = InvalidSquare;
    fen_idx += 2;
  } else {
    const int col = fen[fen_idx] - 'a', row = fen[fen_idx + 1] - '1';
    assert(0 <= col && col < 8);
    assert(0 <= row && row < 8);
    m_en_passant_sq = 8 * row + col;
    fen_idx += 3;
    m_hash ^= en_passant_keys[m_en_passant_sq];
  }

  // Parse the number of plies since the last pawn move or capture
  m_fifty_move = 0;
  for (; fen_idx < fen_size; ++fen_idx) {
    if (fen[fen_idx] == ' ')
      break;
    m_fifty_move = 10 * m_fifty_move + (fen[fen_idx] - '0');
  }
  fen_idx++;

  // Parse the 1-indexed move counter, which needs to be adjusted to be a
  // 0-indexed number of plies since the beginning of the game
  m_ply = 0;
  for (; fen_idx < fen_size && std::isdigit(fen[fen_idx]); ++fen_idx) {
    m_ply = 10 * m_ply + (fen[fen_idx] - '0');
  }
  m_ply = 2 * (m_ply - 1) + (m_side_to_move == White ? 0 : 1);
}

std::string Board::to_fen() const {
  std::stringstream ss;

  std::array<piece_t, 64> pieces;
  for (square_t sq = 0; sq < 64; ++sq) {
    pieces[sq] = InvalidPiece;
    for (piece_t piece = 0; piece < InvalidPiece; ++piece) {
      if (get_bit(m_bitboards[piece], sq)) {
        pieces[sq] = piece;
        break;
      }
    }
  }

  int num_empty = 0;
  for (int row = 7; row >= 0; --row) {
    for (int col = 0; col < 8; ++col) {
      const square_t sq = 8 * row + col;
      const piece_t piece = pieces[sq];
      if (piece == InvalidPiece) {
        num_empty++;
      } else {
        if (num_empty > 0) {
          ss << static_cast<char>('0' + num_empty);
          num_empty = 0;
        }
        ss << piece_to_char(piece);
      }
    }
    if (num_empty > 0) {
      ss << static_cast<char>('0' + num_empty);
      num_empty = 0;
    }
    if (row != 0)
      ss << "/";
  }

  ss << ' ' << (m_side_to_move == White ? 'w' : 'b');
  ss << ' ' << castle_perms_to_string(m_castle_perms);
  ss << ' ' << square_to_string(m_en_passant_sq);
  ss << ' ' << static_cast<int>(m_fifty_move);
  ss << ' ' << static_cast<int>(m_ply / 2 + 1);

  return ss.str();
}

std::string Board::to_lichess_link() const {
  std::string result = "https://lichess.org/analysis/standard/" + to_fen();
  std::replace(result.begin(), result.end(), ' ', '_');
  return result;
}

// Display the board in a human-readable format
std::ostream &Board::print(std::ostream &os) const {
  const char *line_sep = "   +---+---+---+---+---+---+---+---+";
  os << "\n" << line_sep << "\n";
  for (int row = 7; row >= 0; --row) {
    os << " " << (row + 1) << " |";
    for (int col = 0; col < 8; ++col) {
      os << " " << piece_to_char(piece_at_slow(8 * row + col)) << " |";
    }
    os << "\n" << line_sep << "\n";
  }
  os << "     a   b   c   d   e   f   g   h  \n\n";
  os << "Side     :  " << (m_side_to_move == White ? "White" : "Black") << "\n";
  os << "Castle   :  " << castle_perms_to_string(m_castle_perms) << "\n";
  os << "Enpas    :  " << square_to_string(m_en_passant_sq) << "\n";
  os << "50-move  :  " << static_cast<int>(m_fifty_move) << "\n";
  os << "Repeat   :  " << repeat_count() << "\n";
  os << "Ply      :  " << static_cast<int>(m_ply) << "\n";
  const int static_evaluation = static_evaluate(),
            white_static_evaluation = m_side_to_move == White
                                          ? static_evaluation
                                          : -static_evaluation;
  os << "Static   :  " << white_static_evaluation << "\n";
  os << "Hash     :  " << std::hex << std::setw(16) << std::setfill('0')
     << m_hash << std::dec << std::setfill(' ') << "\n";
  os << "FEN      :  " << to_fen() << "\n";
  os << "Lichess  :  " << to_lichess_link() << "\n";
  return os << std::flush;
}

inline int Board::get_piece_value(const piece_t piece,
                                  const square_t square) const {
  return piece_square_table[piece][square];
}

int Board::static_evaluate() const {
  int evaluation = 0;
  for (piece_t piece = WhitePawn; piece <= BlackKing; ++piece) {
    bitboard_t bitboard = m_bitboards[piece];
    while (bitboard) {
      const square_t sq = pop_bit(bitboard);
      evaluation += get_piece_value(piece, sq);
    }
  }
  return m_side_to_move == White ? evaluation : -evaluation;
}

void Board::set_en_passant_sq(const square_t sq) {
  if (m_en_passant_sq != InvalidSquare)
    m_hash ^= en_passant_keys[m_en_passant_sq];
  m_en_passant_sq = sq;
  if (m_en_passant_sq != InvalidSquare)
    m_hash ^= en_passant_keys[m_en_passant_sq];
}

// Queries the piece at a given square, which is possibly empty
// NOTE: This is slow since it has to loop over every bitboard
piece_t Board::piece_at_slow(const square_t sq) const {
  if (!get_bit(m_occupancies[Both], sq))
    return InvalidPiece;
  for (piece_t piece = 0; piece < InvalidPiece; ++piece) {
    if (get_bit(m_bitboards[piece], sq))
      return piece;
  }
  __builtin_unreachable();
}

bool Board::current_player_in_check() const {
  if (m_side_to_move == White) {
    const square_t king_square = lsb(m_bitboards[WhiteKing]);
    return is_square_attacked(Black, king_square);
  } else {
    const square_t king_square = lsb(m_bitboards[BlackKing]);
    return is_square_attacked(White, king_square);
  }
}

// Makes the supplied pseudo-legal move and returns true iff the move does not
// place the king in check
bool Board::make_move(const Move move) {
  const int old_side = m_side_to_move, new_side = other_side(old_side);
  // NOTE: The captured piece will be filled in later by each capture move
  m_history[m_ply] = {m_hash, m_en_passant_sq, InvalidPiece, m_fifty_move,
                      m_castle_perms};
  piece_t &captured_piece = m_history[m_ply].captured_piece;
  m_ply++;

  set_en_passant_sq(InvalidSquare);

  if (move.type == Quiet) {
    move_piece<true>(move.moved_piece, move.source, move.target);
  } else if (move.type == DoublePawn) {
    move_piece<true>(move.moved_piece, move.source, move.target);
    set_en_passant_sq(old_side == White ? move.target - 8 : move.target + 8);
  } else if (move.type == LongCastle) {
    if (old_side == White) {
      move_piece<true>(WhiteKing, E1, C1);
      move_piece<true>(WhiteRook, A1, D1);
    } else {
      move_piece<true>(BlackKing, E8, C8);
      move_piece<true>(BlackRook, A8, D8);
    }
  } else if (move.type == ShortCastle) {
    if (old_side == White) {
      move_piece<true>(WhiteKing, E1, G1);
      move_piece<true>(WhiteRook, H1, F1);
    } else {
      move_piece<true>(BlackKing, E8, G8);
      move_piece<true>(BlackRook, H8, F8);
    }
  } else if (move.type == Capture) {
    captured_piece = remove_piece_slow<true>(move.target);
    move_piece<true>(move.moved_piece, move.source, move.target);
  } else if (move.type == EnPassant) {
    captured_piece = sided_piece(new_side, WhitePawn);
    const square_t remove_square =
        old_side == White ? move.target - 8 : move.target + 8;
    move_piece<true>(move.moved_piece, move.source, move.target);
    remove_piece<true>(captured_piece, remove_square);
  } else if (move.is_some_promotion()) {
    const piece_t promoted_piece = move.promotion_piece();
    if (move.is_capture_promotion())
      captured_piece = remove_piece_slow<true>(move.target);
    remove_piece<true>(move.moved_piece, move.source);
    place_piece<true>(promoted_piece, move.target);
  }

  // Update counters
  m_side_to_move = new_side;
  m_hash ^= side_key;

  m_fifty_move++;
  if (piece_is_pawn(move.moved_piece) || move.is_capture())
    m_fifty_move = 0;

  m_hash ^= castle_keys[m_castle_perms];
  m_castle_perms &=
      castle_perm_update[move.source] & castle_perm_update[move.target];
  m_hash ^= castle_keys[m_castle_perms];

  const square_t king_square =
      lsb(m_bitboards[sided_piece(old_side, WhiteKing)]);

  return !is_square_attacked(new_side, king_square);
}

// Given the pseudo-legal move that was just played, unmake the move
void Board::unmake_move(const Move move) {
  assert(m_ply > 0);

  const int new_side = m_side_to_move, old_side = other_side(new_side);
  const History &history = m_history[m_ply - 1];
  const piece_t captured_piece = history.captured_piece;
  m_ply--;

  if (move.type == Quiet || move.type == DoublePawn) {
    move_piece<false>(move.moved_piece, move.target, move.source);
  } else if (move.type == LongCastle) {
    if (old_side == White) {
      move_piece<false>(WhiteKing, C1, E1);
      move_piece<false>(WhiteRook, D1, A1);
    } else {
      move_piece<false>(BlackKing, C8, E8);
      move_piece<false>(BlackRook, D8, A8);
    }
  } else if (move.type == ShortCastle) {
    if (old_side == White) {
      move_piece<false>(WhiteKing, G1, E1);
      move_piece<false>(WhiteRook, F1, H1);
    } else {
      move_piece<false>(BlackKing, G8, E8);
      move_piece<false>(BlackRook, F8, H8);
    }
  } else if (move.type == Capture) {
    move_piece<false>(move.moved_piece, move.target, move.source);
    place_piece<false>(captured_piece, move.target);
  } else if (move.type == EnPassant) {
    const square_t remove_square =
        old_side == White ? move.target - 8 : move.target + 8;
    move_piece<false>(move.moved_piece, move.target, move.source);
    place_piece<false>(captured_piece, remove_square);
  } else if (move.is_some_promotion()) {
    remove_piece<false>(move.promotion_piece(), move.target);
    place_piece<false>(move.moved_piece, move.source);
    if (move.is_capture_promotion())
      place_piece<false>(captured_piece, move.target);
  }

  m_side_to_move = old_side;
  m_fifty_move = history.fifty_move;
  m_castle_perms = history.castle_perms;
  m_en_passant_sq = history.en_passant;
  m_hash = history.hash;
}

hash_t Board::compute_hash_slow() const {
  hash_t result = 0;
  for (piece_t piece = 0; piece < InvalidPiece; ++piece) {
    bitboard_t piece_bitboard = m_bitboards[piece];
    while (piece_bitboard) {
      const square_t sq = pop_bit(piece_bitboard);
      result ^= piece_keys[piece][sq];
    }
  }

  if (m_en_passant_sq != InvalidSquare)
    result ^= en_passant_keys[m_en_passant_sq];

  result ^= castle_keys[m_castle_perms];

  if (m_side_to_move == Black)
    result ^= side_key;

  return result;
}

// In debug mode, check all the invariants we can think of on the board
void Board::check_invariants(const std::string &description) const {
#ifndef NDEBUG
  assert(m_side_to_move == White || m_side_to_move == Black);
  assert(m_ply % 2 == m_side_to_move);
  if (m_fifty_move > 128) {
    std::cout << "Invariant broken: m_fifty_move = "
              << static_cast<int>(m_fifty_move) << std::endl;
    assert(false);
  }
  if (m_en_passant_sq != InvalidSquare) {
    if (m_side_to_move == White)
      assert(A6 <= m_en_passant_sq && m_en_passant_sq <= H6);
    else if (m_side_to_move == Black)
      assert(A3 <= m_en_passant_sq && m_en_passant_sq <= H3);
  }
  assert(m_castle_perms <= 15);

  expect_equal(m_occupancies[White] | m_occupancies[Black],
               m_occupancies[Both]);
  expect_equal(m_occupancies[White] & m_occupancies[Black], 0ULL);

  bitboard_t white_pieces = 0ULL;
  for (piece_t piece = WhitePawn; piece <= WhiteKing; ++piece) {
    white_pieces |= m_bitboards[piece];
  }
  expect_equal(white_pieces, m_occupancies[White]);

  bitboard_t black_pieces = 0ULL;
  for (piece_t piece = BlackPawn; piece <= BlackKing; ++piece) {
    black_pieces |= m_bitboards[piece];
  }
  expect_equal(black_pieces, m_occupancies[Black]);

  for (square_t sq = 0; sq < 64; ++sq) {
    if (!get_bit(m_occupancies[Both], sq))
      continue;
    piece_t found_piece = InvalidPiece;
    for (piece_t piece = 0; piece < InvalidPiece; ++piece) {
      if (get_bit(m_bitboards[piece], sq)) {
        if (found_piece != InvalidPiece) {
          std::cerr << "Invariants broken " << description << std::endl;
          std::cerr << "Found both " << piece_to_char(found_piece) << " and "
                    << piece_to_char(piece) << " at square "
                    << square_to_string(sq) << std::endl;
        }
        expect_equal(found_piece, InvalidPiece);
        found_piece = piece;
      }
    }
    if (found_piece == InvalidPiece) {
      std::cerr << "Invariants broken " << description << std::endl;
      std::cout << "Occupancies implied there would be a piece at "
                << square_to_string(sq) << " but there wasn't..." << std::endl;
    }
    assert(found_piece != InvalidPiece);
  }

  expect_equal(m_hash, compute_hash_slow());

#endif
}

// Generates all the legal chess moves in the given position, by generating the
// pseudo-legal moves, then making each to check which of them do not leave the
// king in check
Move *Board::legal_moves(Move *list) {
  std::array<Move, Board::max_moves_in_position> moves;
  Move *start_ptr = &moves[0], *end_ptr = nullptr;

  end_ptr = pseudo_moves(start_ptr);
  for (Move *move_ptr = start_ptr; move_ptr < end_ptr; ++move_ptr) {
    const Move &move = *move_ptr;
    if (make_move(move))
      *list++ = move;
    unmake_move(move);
  }
  return list;
}

std::vector<Move> Board::legal_moves_slow() {
  std::vector<Move> result;
  std::array<Move, Board::max_moves_in_position> moves;
  Move *start_ptr = &moves[0], *end_ptr = pseudo_moves(start_ptr);
  for (Move *move_ptr = start_ptr; move_ptr < end_ptr; ++move_ptr) {
    const Move &move = *move_ptr;
    if (make_move(move))
      result.push_back(move);
    unmake_move(move);
  }
  return result;
}

bool Board::has_legal_capture() {
  std::array<Move, Board::max_moves_in_position> pseudo_moves;
  Move *start_ptr = &pseudo_moves[0], *end_ptr = pseudo_captures(start_ptr);
  for (Move *move_ptr = start_ptr; move_ptr < end_ptr; ++move_ptr) {
    const Move move = *move_ptr;
    const bool is_legal_move = make_move(move);
    if (is_legal_move) {
      unmake_move(move);
      return true;
    }
    unmake_move(move);
  }
  return false;
}

bool Board::is_drawn_by_fifty_move() const {
  return m_ply >= Board::max_moves_in_game || m_fifty_move >= 100;
}

int Board::repeat_count() const {
  int repeat_count = 0;
  const size_t start = std::max(0, static_cast<int>(m_ply) - m_fifty_move);
  for (size_t entry = start; entry < m_ply; ++entry) {
    if (m_history[entry].hash == m_hash)
      repeat_count++;
  }
  return repeat_count;
}

bool Board::is_drawn_by_repetition() const {
  // Count to two repeats, since the last occurrence is the current position
  return repeat_count() >= 2;
}

// Returns true if the material on the board cannot theoretically reach a
// decisive result, even with sub-optimal play
bool Board::is_drawn_by_insufficient_material() const {
  // Wouldn't want to misevaluate a checkmate as a draw, so we postpone draw
  // evaluations by at least a ply if the current player is in check
  if (current_player_in_check())
    return false;

  // If there are more than 4 pieces (including kings) left on the board, this
  // doesn't match any of the draw patterns
  const size_t num_pieces = bit_count(m_occupancies[Both]);
  if (num_pieces > 4)
    return false;

  // If either side has a pawn, queen, or rook, the game is never a forced draw
  const bitboard_t pawns_queens_and_rooks =
      m_bitboards[WhitePawn] | m_bitboards[BlackPawn] |
      m_bitboards[WhiteQueen] | m_bitboards[BlackQueen] |
      m_bitboards[WhiteRook] | m_bitboards[BlackRook];
  if (pawns_queens_and_rooks != 0ULL)
    return false;

  // We're guaranteed that the remaining pieces are kings, bishops and knights
  // So if we have 2 or 3 pieces, this is either KvK, KBvK or KNvK
  if (num_pieces <= 3)
    return true;

  // An endgame with exactly two bishops is a forced draw iff the bishops are
  // the same colour
  const bitboard_t white_bishops = m_bitboards[WhiteBishop];
  const bitboard_t black_bishops = m_bitboards[BlackBishop];
  bitboard_t all_bishops = white_bishops | black_bishops;
  if (bit_count(all_bishops) == 2) {
    const square_t first_bishop = pop_bit(all_bishops);
    const square_t second_bishop = pop_bit(all_bishops);
    if (square_colour(first_bishop) == square_colour(second_bishop))
      return true;
  }

  // NOTE: We don't consider KBvKN a forced draw, since in the position
  // FEN: 'k1B5/8/K1n5/8/8/8/8/8 w - - 0 1'
  // The following sequence of moves leads to checkmate:
  // 1. Kb6 Nb8   2. Bb7#

  return false;
}

GameResult Board::check_result() {
  if (is_drawn_by_fifty_move())
    return DrawByFiftyMove;
  if (is_drawn_by_insufficient_material())
    return DrawByInsufficientMaterial;
  if (is_drawn_by_repetition())
    return DrawByRepetition;

  std::array<Move, Board::max_moves_in_position> moves;
  Move *start_ptr = &moves[0], *end_ptr = legal_moves(start_ptr);
  const bool has_no_legal_moves = end_ptr == start_ptr;

  if (m_side_to_move == White) {
    const square_t king_square = lsb(m_bitboards[WhiteKing]);
    const bool in_check = is_square_attacked(Black, king_square);
    if (has_no_legal_moves)
      return in_check ? BlackCheckmate : Stalemate;
  } else {
    const square_t king_square = lsb(m_bitboards[BlackKing]);
    const bool in_check = is_square_attacked(White, king_square);
    if (has_no_legal_moves)
      return in_check ? WhiteCheckmate : Stalemate;
  }

  return InProgress;
}

// Returns true if the given square is attacked by any piece on the given side
bool Board::is_square_attacked(const int side, const square_t sq) const {
  const int other_side = ::other_side(side);
  const piece_t my_pawn = sided_piece(side, WhitePawn);
  const piece_t my_knight = sided_piece(side, WhiteKnight);
  const piece_t my_bishop = sided_piece(side, WhiteBishop);
  const piece_t my_rook = sided_piece(side, WhiteRook);
  const piece_t my_queen = sided_piece(side, WhiteQueen);
  const piece_t my_king = sided_piece(side, WhiteKing);
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
bitboard_t Board::get_attacked_squares_slow(const int side) const {
  bitboard_t attacked_mask = 0;
  for (square_t sq = 0; sq < 64; ++sq) {
    if (is_square_attacked(side, sq))
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
Move *Board::pseudo_captures(Move *list) const {
  int other_side = ::other_side(m_side_to_move);

  if (m_side_to_move == White) {
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
  if (m_side_to_move == White) {
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
Move *Board::pseudo_quiet_moves(Move *list) const {
  int other_side = ::other_side(m_side_to_move);
  const bitboard_t both_occ = m_occupancies[Both], empty_squares = ~both_occ;

  if (m_side_to_move == White) {
    // Castling
    if (m_castle_perms & (WhiteShortCastle | WhiteLongCastle) &&
        !is_square_attacked(other_side, E1)) {
      constexpr bitboard_t short_castle_mask =
          square_to_bitboard(F1) | square_to_bitboard(G1);
      constexpr bitboard_t long_castle_mask = square_to_bitboard(D1) |
                                              square_to_bitboard(C1) |
                                              square_to_bitboard(B1);
      if (m_castle_perms & WhiteShortCastle &&
          !(both_occ & short_castle_mask) &&
          !is_square_attacked(other_side, F1)) {
        *list++ = Move(ShortCastle, E1, G1, WhiteKing);
      }
      if (m_castle_perms & WhiteLongCastle && !(both_occ & long_castle_mask) &&
          !is_square_attacked(other_side, D1)) {
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
        !is_square_attacked(other_side, E8)) {
      constexpr bitboard_t short_castle_mask =
          square_to_bitboard(F8) | square_to_bitboard(G8);
      constexpr bitboard_t long_castle_mask = square_to_bitboard(D8) |
                                              square_to_bitboard(C8) |
                                              square_to_bitboard(B8);
      if (m_castle_perms & BlackShortCastle &&
          !(both_occ & short_castle_mask) &&
          !is_square_attacked(other_side, F8)) {
        *list++ = Move(ShortCastle, E8, G8, BlackKing);
      }
      if (m_castle_perms & BlackLongCastle && !(both_occ & long_castle_mask) &&
          !is_square_attacked(other_side, D8)) {
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

  if (m_side_to_move == White) {
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

Move *Board::pseudo_promotions(Move *list) const {
  const bitboard_t both_occ = m_occupancies[Both], empty_squares = ~both_occ;

  if (m_side_to_move == White) {
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
Move *Board::pseudo_captures_and_promotions(Move *list) const {
  list = pseudo_captures(list);
  list = pseudo_promotions(list);
  return list;
}

// Generate all the pseudo-legal chess moves
Move *Board::pseudo_moves(Move *list) const {
  list = pseudo_captures(list);
  list = pseudo_quiet_moves(list);
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
  if constexpr (update_hash)
    m_hash ^= piece_keys[piece][sq];
}
