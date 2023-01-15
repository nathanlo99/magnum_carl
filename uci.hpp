
#pragma once

#include <iostream>

#include "board.hpp"

inline std::vector<std::string> split(const std::string &line) {
  std::vector<std::string> result;
  std::string buffer;
  for (char c : line) {
    if (c == ' ') {
      result.push_back(buffer);
      buffer.clear();
    } else {
      buffer.push_back(c);
    }
  }
  if (!buffer.empty())
    result.push_back(buffer);
  return result;
}

template <class InputIt> inline std::string join(InputIt start, InputIt end) {
  std::string result = *start++;
  while (start != end) {
    result += " " + *start++;
  }
  return result;
}

inline Move find_uci_move(const Board &board, const std::string &move_str) {
  std::array<Move, Board::max_moves_in_position> moves;
  Move *start_ptr = &moves[0], *end_ptr = board.pseudo_moves(start_ptr);
  for (Move *move_ptr = start_ptr; move_ptr != end_ptr; ++move_ptr) {
    const Move &move = *move_ptr;
    if (move.to_uci() == move_str)
      return move;
  }
  std::cerr << "Could not find move: '" << move_str
            << "' in following position:" << std::endl;
  std::cerr << board << std::endl;
  std::terminate();
}

inline void uci_loop(std::istream &in, std::ostream &out) {
  out << "id name magnum_carl" << std::endl;
  out << "id author Nathan Lo" << std::endl;
  out << "uciok" << std::endl;

  std::string line, last_position_line = "";
  Board board;
  bool is_new_game = true;
  while (std::getline(in, line)) {
    if (line == "isready") {
      out << "readyok" << std::endl;
    } else if (line == "ucinewgame") {
      is_new_game = true;
    } else if (line.starts_with("position")) {
      const std::vector<std::string> tokens = split(line);
      is_new_game |= !line.starts_with(last_position_line);
      if (is_new_game) {
        // Parse the entire thing
        const std::string fen =
            tokens[1] == "startpos"
                ? Board::start_fen
                : join(tokens.begin() + 2, tokens.begin() + 8);
        const size_t next_idx = tokens[1] == "startpos" ? 3 : 9;
        for (size_t idx = next_idx; idx < tokens.size(); ++idx) {
          board.make_move(find_uci_move(board, tokens[idx]));
        }
        is_new_game = false;
      } else {
        // Parse the last move and play it onto the cached position
        board.make_move(find_uci_move(board, tokens.back()));
      }
      last_position_line = line;
    } else if (line.starts_with("go")) {
      // TODO: Parse the go command and kick off the appropriate search command

    } else if (line == "stop") {
      // TODO: Interrupt pondering
    } else if (line == "quit")
      break;
  }
}
