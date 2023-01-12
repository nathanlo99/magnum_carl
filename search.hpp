
#pragma once

#include "board.hpp"

const int MateScore = 50'000;

int compute_move_score(Board &board, const Move tt_move, const Move move,
                       const bool move_resulted_in_check,
                       const bool move_resulted_in_captures) {
  int result = 0;

  if (move == tt_move)
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
      result += 10 * captured_piece;
      result += 10 - moved_piece;
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
    result += 100 * move.promotion_piece();
  }

  return result;
}

class MoveHeap {
  size_t num_moves = 0;
  std::array<std::pair<Move, int>, Board::max_moves_in_position> data;

public:
  MoveHeap(Board &board, const Move tt_move) {
    std::array<Move, Board::max_moves_in_position> moves;
    Move *start_ptr = &moves[0],
         *end_ptr = board.m_side_to_move == White
                        ? board.pseudo_moves<White>(start_ptr)
                        : board.pseudo_moves<Black>(start_ptr);

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
};

// Fail-soft alpha-beta
int alpha_beta_evaluate(Board &board, int current_depth, int max_depth,
                        int alpha, int beta) {
  int best_score = -MateScore;
  if (current_depth >= max_depth)
    return quiescence_evaluate(board, current_depth, -MateScore, MateScore);

  Move tt_move; // TODO: Query the tablebase
  MoveHeap heap(board, tt_move);
  for (const Move move : heap) {
    board.make_move(move);
    const int score = -alpha_beta_evaluate(board, current_depth + 1, max_depth,
                                           -beta, -alpha);
    board.unmake_move(move);

    if (score >= beta)
      return score; // Fail-soft beta cutoff

    if (score > best_score) {
      best_score = score;
      if (score > alpha)
        alpha = score;
    }
  }

  return best_score;
}
