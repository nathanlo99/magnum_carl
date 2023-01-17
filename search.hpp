
#pragma once

#include "board.hpp"
#include "piece.hpp"
#include "transposition_table.hpp"

#include <algorithm>
#include <unordered_set>

static std::atomic<bool> always_false = false;

struct search_info_t {
  long long start_time, stop_time;
  size_t num_nodes = 0;
  bool stopped = false;
  std::atomic<bool> &force_stop;

  search_info_t(const int search_ms = 100'000'000)
      : start_time(get_time_ms()), stop_time(start_time + search_ms),
        force_stop(always_false) {}
  search_info_t(std::atomic<bool> &force_stop)
      : start_time(get_time_ms()), stop_time(start_time + 100'000'000),
        force_stop(force_stop) {}

  inline void check_time() {
    if (!stopped && (num_nodes & 2047) == 0)
      stopped = get_time_ms() >= stop_time;
  }

  inline long long search_time() const { return get_time_ms() - start_time; }
  inline bool should_stop() const { return force_stop || stopped; }
};

inline std::string score_to_string(const int score) {
  // return "cp " + std::to_string(score);
  if (score > MateScore - MaxMateDepth) {
    const int distance_to_mate = MateScore - score;
    return "mate " + std::to_string(distance_to_mate / 2 + 1);
  } else if (score < -MateScore + MaxMateDepth) {
    const int distance_to_mate = MateScore + score;
    return "mate " + std::to_string(-distance_to_mate / 2);
  } else {
    return "cp " + std::to_string(score);
  }
}

inline void print_pv(std::ostream &out, Board &board) {
  std::vector<Move> pv_moves;
  std::unordered_set<hash_t> pv_hashes;
  while (true) {
    if (pv_hashes.count(board.m_hash) > 0)
      break;
    pv_hashes.insert(board.m_hash);

    const auto &[has_tt_entry, tt_entry] = tt_probe_entry(board.m_hash);
    if (!has_tt_entry || tt_entry.flag != TTEntryExact)
      break;

    out << tt_entry.move.to_uci() << " ";
    pv_moves.push_back(tt_entry.move);
    board.make_move(tt_entry.move);
  }
  for (auto it = pv_moves.rbegin(); it != pv_moves.rend(); ++it) {
    board.unmake_move(*it);
  }
  out << std::endl;
}

// Given a board, the current move stored in the transposition table, and some
// flags indicating interesting things about the move, compute its move-ordering
// score
inline int compute_move_score(Board &board, const Move tt_move, const Move move,
                              const bool move_resulted_in_check) {
  int result = 0;

  if (move == tt_move) [[unlikely]]
    return 10000;

  if (move_resulted_in_check)
    result += 500;

  if (move.is_some_promotion())
    result += 100 * normalized_piece_value_table[move.promotion_piece()];

  if (move.is_capture()) {
    result += 1000;
    // The largest contribution from captures + promotions is
    //  40 if a queen is captured
    //  10 if captured by a pawn
    // 400 if promoting to a queen
    // ---
    // 450 in total
    const piece_t moved_piece = move.moved_piece,
                  captured_piece = move.type == EnPassant
                                       ? WhitePawn
                                       : board.piece_at_slow(move.target);
    const int moved_piece_value = normalized_piece_value_table[moved_piece];
    const int captured_piece_value =
        normalized_piece_value_table[captured_piece];

    // NOTE: The colour of the captured pawn in an en-passant doesn't matter
    // as we ignore colour when we normalize
    result += 10 * captured_piece_value;
    result += 10 - moved_piece_value;
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

    inline Move operator*() const {
      // Get the move with the highest score in data[start_idx..] and move it to
      // index start_idx
      const auto start = m_heap.data.begin() + m_start_idx,
                 end = m_heap.data.begin() + m_heap.num_moves;
      for (auto it = start; it != end; ++it) {
        if (it->second > start->second)
          std::swap(*it, *start);
      }
      return start->first;
    }

    inline MoveHeapIterator &operator++() {
      m_start_idx++;
      return *this;
    }

    inline bool operator!=(const MoveHeapIterator &other) const {
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

      const bool move_resulted_in_check = board.current_player_in_check();

      // NOTE: compute_move_score relies on the board state before the move is
      // played, so we have to unmake the move before calling compute_move_score
      board.unmake_move(move);

      data[num_moves++] =
          std::make_pair(move, compute_move_score(board, tt_move, move,
                                                  move_resulted_in_check));
    }
  }

  inline auto begin() { return MoveHeapIterator(*this, 0); }
  inline auto end() { return MoveHeapIterator(*this, num_moves); }
  inline auto empty() const { return num_moves == 0; }
};

inline int quiescence_evaluate(Board &board, search_info_t &search_info,
                               int current_depth, int alpha, int beta) {
  // This is the only drawing condition other than stalemate we have to check
  // here, the others are reset by a capture or queen promotion
  if (board.is_drawn_by_insufficient_material()) [[unlikely]]
    return DrawScore;

  const int stand_pat_score = board.static_evaluate();
  if (stand_pat_score >= beta)
    return beta;
  if (stand_pat_score > alpha)
    alpha = stand_pat_score;

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
    return stand_pat_score;
  }

  // Search only captures and pawn promotions
  int best_score = -MateScore;
  for (const auto &move : heap) {
    board.make_move(move);
    const int score = -quiescence_evaluate(board, search_info,
                                           current_depth + 1, -beta, -alpha);
    board.unmake_move(move);

    if (score >= beta)
      return score;
    if (score > best_score) {
      best_score = score;
      if (score > alpha)
        alpha = score;
    }
  }
  return best_score;
}

// Fail-soft alpha-beta
inline int alpha_beta_evaluate(Board &board, search_info_t &search_info,
                               int current_depth, int max_depth, int alpha,
                               int beta) {

  // This checks draws by repetition, fifty moves, and insufficient material, so
  // make sure to check this before quiescence_evaluate so we can skip checking
  // it in QS, since every capture and queen promotion will reset these counters
  const auto &[has_simple_eval, simple_eval] =
      board.compute_simple_evaluation();
  if (has_simple_eval) [[unlikely]]
    return simple_eval;

  search_info.num_nodes++;

  search_info.check_time();
  if (current_depth >= 2 && search_info.should_stop()) [[unlikely]]
    return 0;

  // Extend the search if the current player is in check, to further explore
  // forcing situations
  const bool in_check = board.current_player_in_check();
  if (in_check)
    max_depth++;

  if (current_depth >= max_depth) [[unlikely]]
    return quiescence_evaluate(board, search_info, current_depth, alpha, beta);

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
  if (heap.empty()) [[unlikely]]
    return in_check ? -MateScore + current_depth : DrawScore;

  bool have_improved_alpha = false;
  int best_score = -MateScore;
  Move best_move;
  for (const auto &move : heap) {
    board.make_move(move);
    int score = 0;
    if (have_improved_alpha) {
      score = -alpha_beta_evaluate(board, search_info, current_depth + 1,
                                   max_depth, -(alpha + 1), -alpha);
      if (score > alpha && score < beta)
        score = -alpha_beta_evaluate(board, search_info, current_depth + 1,
                                     max_depth, -beta, -alpha);
    } else {
      score = -alpha_beta_evaluate(board, search_info, current_depth + 1,
                                   max_depth, -beta, -alpha);
    }
    board.unmake_move(move);

    if (score >= beta) {
      if (!search_info.should_stop())
        tt_write(board.m_hash, current_depth, max_depth, TTEntryBeta, score,
                 move);
      return score;
    }
    if (score > best_score) {
      best_score = score;
      best_move = move;
      if (score > alpha) {
        have_improved_alpha = true;
        alpha = score;
      }
    }
  }

  if (!search_info.should_stop())
    tt_write(board.m_hash, current_depth, max_depth,
             have_improved_alpha ? TTEntryExact : TTEntryAlpha, best_score,
             best_move);
  return best_score;
}

inline Move alpha_beta_search(Board &board, int max_depth,
                              int search_ms = 100'000'000) {
  log() << "Starting search to max depth " << max_depth << " for " << search_ms
        << " ms" << std::endl;
  search_info_t search_info(search_ms);
  Move best_move;

  int alpha = -MateScore, beta = MateScore;
  for (int depth = 1; depth <= max_depth; ++depth) {
    int score = 0, aspiration_window_size = 10;
    while (true) {
      score = alpha_beta_evaluate(board, search_info, 0, depth, alpha, beta);
      if (depth >= 2 && search_info.should_stop()) [[unlikely]]
        return best_move;

      if (score >= beta) {
        beta = std::min(MateScore, beta + aspiration_window_size);
        aspiration_window_size *= 4;
      } else if (score <= alpha) {
        alpha = std::max(-MateScore, alpha - aspiration_window_size);
        aspiration_window_size *= 4;
      } else {
        alpha = std::max(-MateScore, score - 10);
        beta = std::min(MateScore, score + 10);
        break;
      }
    }

    const Move current_best_move = tt_probe_move(board.m_hash);
    if (current_best_move.is_valid())
      best_move = current_best_move;

    // depth, time, nodes, score, nps, pv
    const auto nodes_per_second =
        search_info.num_nodes / search_info.search_time() * 1000;
    std::cout << "info time " << search_info.search_time() << " depth " << depth
              << " nodes " << search_info.num_nodes << " score "
              << score_to_string(score) << " nps " << nodes_per_second
              << " pv ";
    print_pv(std::cout, board);

    log() << "info time " << search_info.search_time() << " depth " << depth
          << " nodes " << search_info.num_nodes << " score "
          << score_to_string(score) << " nps " << nodes_per_second << " pv ";
    print_pv(log(), board);

    // Don't search deeper than the best mating sequence we've found so far
    // NOTE: If anything goes wrong with mating evaluations, remove this line
    // first before debugging
    if (score > MateScore - depth || score < -MateScore + depth)
      return best_move;

    if (depth >= 2 && search_info.should_stop()) [[unlikely]]
      return best_move;
  }

  return best_move;
}
