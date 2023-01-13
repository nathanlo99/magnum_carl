
#pragma once

#include "bitboard.hpp"
#include "board.hpp"
#include "logger.hpp"
#include "move.hpp"
#include "util.hpp"

#include <array>
#include <utility>

#include <unordered_map>

constexpr size_t max_tt_size = 0x4'000'000;

enum {
  TTEntryExact,
  TTEntryAlpha,
  TTEntryBeta,
  TTEntryInvalid,
};

struct tt_entry_t {
  hash_t hash = 0ULL;
  int remaining_depth = 0;
  int flag = TTEntryInvalid;
  Move move = Move();
  int score = UnknownScore;

  tt_entry_t() = default;
  tt_entry_t(const hash_t hash, const int remaining_depth, const Move move,
             const int flag, const int score)
      : hash(hash), remaining_depth(remaining_depth), flag(flag), move(move),
        score(score) {}

  std::string to_string() const { return move.to_string(); }

  std::string to_long_string() const {
    std::stringstream ss;
    ss << std::hex << std::setw(16) << std::setfill('0') << hash << std::dec
       << std::setfill(' ') << " : ";
    ss << "{ remaining_depth = " << remaining_depth << ", score ";
    ss << (flag == TTEntryExact   ? "=="
           : flag == TTEntryAlpha ? "<="
                                  : ">=")
       << " ";
    ss << score;
    ss << ", best_move = " << move.to_string() << " }";
    return ss.str();
  }
};

extern std::unordered_map<hash_t, tt_entry_t> transposition_table;

inline void tt_clear() {
  log() << "tt_clear()" << std::endl;
  transposition_table.clear();
}
inline tt_entry_t &tt_index(const hash_t hash) {
  return transposition_table[hash];
}

inline void tt_dump() {
  for (const auto &[hash, entry] : transposition_table) {
    if (entry.hash == 0ULL)
      continue;
    std::cout << entry.to_long_string() << std::endl;
  }
}

inline std::ostream &operator<<(std::ostream &os, const tt_entry_t &entry) {
  return os << entry.to_long_string();
}

constexpr inline int score_to_tt(const int eval_score,
                                 const int current_depth) {
  return (eval_score > MateScore - MaxMateDepth)    ? eval_score + current_depth
         : (eval_score < -MateScore + MaxMateDepth) ? eval_score - current_depth
                                                    : eval_score;
}

constexpr inline int score_from_tt(const int tt_score,
                                   const int current_depth) {
  return (tt_score > MateScore - MaxMateDepth)    ? tt_score - current_depth
         : (tt_score < -MateScore + MaxMateDepth) ? tt_score + current_depth
                                                  : tt_score;
}

inline Move tt_probe_move(const hash_t hash) {
  log_print("tt_probe(" << std::hex << std::setw(16) << std::setfill('0')
                        << hash << std::dec << std::setfill(' ') << ")");
  const tt_entry_t &entry = tt_index(hash);
  if (entry.hash != hash) [[unlikely]]
    return Move();
  return entry.move;
}

inline std::pair<bool, int> tt_probe_current_score(const hash_t hash) {
  const tt_entry_t &entry = tt_index(hash);
  if (entry.hash != hash || entry.flag != TTEntryExact)
    return {false, 69};
  return {true, entry.score};
}

inline int tt_probe_score(const hash_t hash, const int current_depth,
                          const int max_depth, const int alpha,
                          const int beta) {
  const tt_entry_t &entry = tt_index(hash);
  if (entry.hash != hash) [[unlikely]]
    return UnknownScore;
  const int remaining_depth = max_depth - current_depth;
  if (remaining_depth > entry.remaining_depth)
    return UnknownScore;

  const int eval_score = score_from_tt(entry.score, current_depth);
  if (entry.flag == TTEntryExact)
    return eval_score;
  if (entry.flag == TTEntryAlpha && eval_score <= alpha)
    return alpha;
  if (entry.flag == TTEntryBeta && eval_score >= beta)
    return beta;
  return UnknownScore;
}

inline std::pair<bool, tt_entry_t> tt_probe_entry(const hash_t hash) {
  const tt_entry_t &entry = tt_index(hash);
  return (entry.hash == hash) ? std::make_pair(true, entry)
                              : std::make_pair(false, tt_entry_t());
}

inline void tt_write(const hash_t hash, const int current_depth,
                     const int max_depth, const int flag, const int eval_score,
                     const Move best_move) {
  const int remaining_depth = max_depth - current_depth;
  log_print("tt_write(" << std::hex << std::setw(16) << std::setfill('0')
                        << hash << std::dec << std::setfill(' ') << ", "
                        << remaining_depth << ", " << best_move.to_string()
                        << ")");
  const int tt_score = score_to_tt(eval_score, current_depth);
  tt_entry_t &entry = tt_index(hash);
  entry = {hash, remaining_depth, best_move, flag, tt_score};
}
