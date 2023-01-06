
#pragma once

#include <cstdint>
#include <string_view>

#include "board.hpp"

inline uint64_t perft(Board &board, const size_t depth) {
  if (depth == 0)
    return 1;
  std::array<Move, Board::max_moves_in_position> moves;
  Move *start_ptr = &moves[0], *end_ptr = board.legal_moves(start_ptr);
  if (depth == 1)
    return end_ptr - start_ptr;

  uint64_t result = 0;
  for (Move *move_ptr = start_ptr; move_ptr < end_ptr; ++move_ptr) {
    const Move &move = *move_ptr;
    board.make_move(move);
    result += perft(board, depth - 1);
    board.unmake_move(move);
  }
  return result;
}

inline uint64_t perft_test(const std::string_view &fen, const size_t depth) {
  Board board(fen);
  return perft(board, depth);
}
