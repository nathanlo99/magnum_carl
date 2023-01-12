
#pragma once

#include "board.hpp"
#include "transposition_table.hpp"

#include <algorithm>

inline int compute_move_score(Board &board, const Move tt_move, const Move move,
                              const bool move_resulted_in_check,
                              const bool move_resulted_in_captures) {
  int result = 0;

  if (move == tt_move) [[unlikely]]
    return 2000;

  if (move.is_capture()) {
    // The largest contribution from captures + promotions is
    //  40 if a queen is captured
    //  10 if captured by a pawn
    // 400 if promoting to a queen
    // ---
    // 450 in total
    if (move.type == EnPassant) {
      return 90;
    } else {
      const piece_t moved_piece = move.moved_piece,
                    captured_piece = board.piece_at_slow(move.target);
      result += 10 * normalized_piece_value_table[captured_piece];
      result += 10 - normalized_piece_value_table[moved_piece];
    }
  } else {
    if (move_resulted_in_check) {
      result += 500;
    }
    if (move_resulted_in_captures) {
      // Search moves which result in captures first, since these are simpler
      // and mostly interesting
      result += 100;
    } else if (piece_is_pawn(move.moved_piece)) {
      // Nudge quiet move searches in favour of changing the fifty move counter
      // NOTE: This has the intended side-effect of playing more "interesting"
      // moves if lots of quiet moves tie in evaluation
      result += 50;
    }
  }

  // Promotions, regardless of whether they are captures, are usually very
  // interesting

  // NOTE: This adds at most 400 points
  if (move.is_some_promotion()) {
    result += 100 * normalized_piece_value_table[move.promotion_piece()];
  }

  return result;
}

class MoveHeap {
  size_t num_moves = 0;
  std::array<std::pair<Move, int>, Board::max_moves_in_position> data;

  struct MoveHeapIterator {
    MoveHeap &m_heap;
    size_t m_start_idx;

    MoveHeapIterator(MoveHeap &heap, const size_t start_idx)
        : m_heap(heap), m_start_idx(start_idx) {}

    const Move &operator*() const {
      // Get the move with the highest score in data[start_idx..] and move it to
      // index start_idx
      const auto it = std::max_element(
          m_heap.data.begin() + m_start_idx,
          m_heap.data.begin() + m_heap.num_moves,
          [](const std::pair<Move, int> &a, const std::pair<Move, int> &b) {
            return a.second < b.second;
          });

      std::swap(*it, m_heap.data[m_start_idx]);
      return m_heap.data[m_start_idx].first;
    }

    MoveHeapIterator &operator++() {
      m_start_idx++;
      return *this;
    }

    bool operator!=(const MoveHeapIterator &other) const {
      return m_start_idx != other.m_start_idx;
    }
  };

public:
  MoveHeap(Board &board, const Move tt_move, Move *start_ptr, Move *end_ptr) {
    for (Move *move_ptr = start_ptr; move_ptr != end_ptr; ++move_ptr) {
      const Move &move = *move_ptr;
      const bool is_legal_move = board.make_move(move);
      if (!is_legal_move) {
        board.unmake_move(move);
        continue;
      }

      const bool move_resulted_in_captures = board.has_legal_capture();
      const bool move_resulted_in_check = board.current_player_in_check();

      board.unmake_move(move);

      data[num_moves++] = std::make_pair(
          move, compute_move_score(board, tt_move, move, move_resulted_in_check,
                                   move_resulted_in_captures));
    }
  }

  auto begin() { return MoveHeapIterator(*this, 0); }
  auto end() { return MoveHeapIterator(*this, num_moves); }
  auto empty() const { return num_moves == 0; }
};

inline int quiescence_evaluate(Board &board, int current_depth, int alpha,
                               int beta) {
  // This is the only drawing condition other than stalemate we have to check
  // here, the others are reset by a capture or queen promotion
  if (board.is_drawn_by_insufficient_material()) [[unlikely]]
    return DrawScore;

  // Generate pseudo-captures and filter them later
  std::array<Move, Board::max_moves_in_position> moves;

  Move *start_ptr = &moves[0],
       *end_ptr = board.pseudo_captures_and_promotions(start_ptr);

  MoveHeap heap(board, Move(), start_ptr, end_ptr);
  if (heap.empty()) {
    // If there are no legal captures, either return the terminal evaluation, or
    // the material evaluation.

    // No legal moves at all, return appropriate score
    // TODO: Replace this with legal_quiet_moves if more efficient?
    // NOTE: Since we already know that there are no legal captures
    end_ptr = board.legal_moves(start_ptr);
    if (start_ptr == end_ptr) [[unlikely]]
      return board.current_player_in_check() ? -MateScore + current_depth
                                             : DrawScore;

    // If there are no captures, but it isn't terminal, return the raw
    // material evaluation
    return board.static_evaluate();
  }

  // Search only captures and pawn promotions
  for (const auto &move : heap) {
    board.make_move(move);
    const int score =
        -quiescence_evaluate(board, current_depth + 1, -beta, -alpha);
    board.unmake_move(move);

    if (score >= beta)
      return beta;
    if (score > alpha)
      alpha = score;
  }
  return alpha;
}

// Fail-soft alpha-beta
inline int alpha_beta_evaluate(Board &board, int current_depth, int max_depth,
                               int alpha, int beta) {

  // This checks draws by repetition, fifty moves, and insufficient material, so
  // make sure to check this before quiescence_evaluate so we can skip checking
  // it in QS, since every capture and queen promotion will reset these counters
  const auto &[has_simple_eval, simple_eval] =
      board.compute_simple_evaluation();
  if (has_simple_eval) [[unlikely]] {
    log_print("alpha_beta_evaluate | done - simple_eval");
    return simple_eval;
  }

  // Extend the search if the current player is in check, to further explore
  // forcing situations
  const bool in_check = board.current_player_in_check();
  if (in_check)
    max_depth++;

  if (current_depth >= max_depth) [[unlikely]]
    return quiescence_evaluate(board, current_depth, -MateScore, MateScore);

  // Probe transposition table for the PV move
  const Move tt_move = tt_probe_move(board.m_hash);
  const int tt_score =
      tt_probe_score(board.m_hash, current_depth, max_depth, alpha, beta);

  // Only use transposition table alpha and beta cut-offs when we don't need to
  // preserve a PV
  if (current_depth >= 2 && tt_score != UnknownScore)
    return tt_score;

  std::array<Move, Board::max_moves_in_position> moves;
  Move *start_ptr = &moves[0], *end_ptr = board.pseudo_moves(start_ptr);

  MoveHeap heap(board, tt_move, start_ptr, end_ptr);
  // No legal moves: the game has ended in either a checkmate or stalemate
  if (heap.empty()) [[unlikely]] {
    log_print("alpha_beta_evaluate | done - terminal");
    return in_check ? -MateScore + current_depth : DrawScore;
  }

  bool have_improved_alpha = false;
  Move best_move;
  for (const auto &move : heap) {
    board.make_move(move);
    const int score = -alpha_beta_evaluate(board, current_depth + 1, max_depth,
                                           -beta, -alpha);
    board.unmake_move(move);

    if (score > alpha) {
      have_improved_alpha = true;
      alpha = score;
      best_move = move;

      if (score >= beta) {
        tt_write(board.m_hash, current_depth, max_depth, TTEntryBeta, score,
                 move);
        return beta;
      }
    }
  }

  tt_write(board.m_hash, current_depth, max_depth,
           have_improved_alpha ? TTEntryExact : TTEntryAlpha, alpha, best_move);

  return alpha;
}

inline Move alpha_beta_search(Board &board, int max_depth) {
  Move best_move;
  int alpha = -MateScore, beta = MateScore;
  for (int current_depth = 1; current_depth <= max_depth; ++current_depth) {
    const int score = alpha_beta_evaluate(board, 0, current_depth, alpha, beta);
    const Move current_best_move = tt_probe_move(board.m_hash);
    if (current_best_move.is_valid())
      best_move = current_best_move;
  }
  return best_move;
}
