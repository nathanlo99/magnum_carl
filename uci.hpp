
#pragma once

#include <iostream>
#include <limits>

#include "board.hpp"
#include "search.hpp"

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

struct UCI {
  struct GoCommand {
    bool ponder = false;
    int wtime_ms = std::numeric_limits<int>::max();
    int btime_ms = std::numeric_limits<int>::max();
    int winc_ms = 0;
    int binc_ms = 0;
    int search_ms = -1;
    int moves_to_go = 0;
    int max_depth = Board::max_moves_in_game;
    long long max_nodes = std::numeric_limits<long long>::max();

    friend std::ostream &operator<<(std::ostream &os,
                                    const GoCommand &command) {
      return os << "{ ponder = " << command.ponder
                << ", wtime_ms = " << command.wtime_ms
                << ", btime_ms = " << command.btime_ms
                << ", winc_ms = " << command.winc_ms
                << ", binc_ms = " << command.binc_ms
                << ", search_ms = " << command.search_ms
                << ", moves_to_go = " << command.moves_to_go
                << ", max_depth = " << command.max_depth
                << ", max_nodes = " << command.max_nodes << "}";
    }
  };

  Board m_board;
  bool m_is_new_game = true;
  std::string m_last_position_line = "";

  UCI() = default;

  void loop();

  void handle_position(const std::string &line);
  void handle_go(const std::string &line);
};

inline void UCI::handle_position(const std::string &line) {
  const std::vector<std::string> tokens = split(line);
  m_is_new_game |= !line.starts_with(m_last_position_line);
  if (m_is_new_game) {
    // Parse the entire thing
    const std::string fen = tokens[1] == "startpos"
                                ? Board::start_fen
                                : join(tokens.begin() + 2, tokens.begin() + 8);
    log() << "Parsing FEN: '" << fen << "'" << std::endl;
    m_board = Board(fen);
    const size_t next_idx = tokens[1] == "startpos" ? 3 : 9;
    for (size_t idx = next_idx; idx < tokens.size(); ++idx) {
      m_board.make_move(find_uci_move(m_board, tokens[idx]));
    }
    m_is_new_game = false;
  } else {
    // Parse the last two moves and play it onto the cached position
    const size_t num_tokens = tokens.size();
    m_board.make_move(find_uci_move(m_board, tokens[num_tokens - 2]));
    m_board.make_move(find_uci_move(m_board, tokens[num_tokens - 1]));
  }
  m_last_position_line = line;
}

inline void UCI::handle_go(const std::string &line) {
  const std::vector<std::string> tokens = split(line);
  assert(tokens[0] == "go");
  GoCommand command;
  for (size_t idx = 1; idx < tokens.size(); ++idx) {
    const std::string &token = tokens[idx];
    if (token == "wtime")
      command.wtime_ms = std::stoi(tokens[++idx]);
    else if (token == "btime")
      command.btime_ms = std::stoi(tokens[++idx]);
    else if (token == "winc")
      command.winc_ms = std::stoi(tokens[++idx]);
    else if (token == "binc")
      command.binc_ms = std::stoi(tokens[++idx]);
    else if (token == "movestogo")
      command.moves_to_go = std::stoi(tokens[++idx]);
    else if (token == "depth")
      command.max_depth = std::stoi(tokens[++idx]);
    else if (token == "nodes")
      command.max_nodes = std::stoi(tokens[++idx]);
    else if (token == "movetime")
      command.search_ms = std::stoi(tokens[++idx]);
    else if (token == "infinite")
      command.search_ms = std::numeric_limits<int>::max();
    else
      std::cerr << "WARN: Unknown go token: '" << token << "'" << std::endl;
  }

  if (command.search_ms == -1) {
    const int my_time =
        m_board.m_side_to_move == White ? command.wtime_ms : command.btime_ms;
    const int my_inc =
        m_board.m_side_to_move == White ? command.winc_ms : command.binc_ms;
    command.search_ms = (my_time - 1000) / 60.0 + my_inc * 0.9;
  }

  log() << "Received go command: " << command << std::endl;
  const Move best_move =
      alpha_beta_search(m_board, command.max_depth, command.search_ms);
  log() << "Sending best move: " << best_move.to_uci() << std::endl;
  std::cout << "bestmove " << best_move.to_uci() << std::endl;
}

inline void UCI::loop() {
  std::cout << "id name magnum_carl" << std::endl;
  std::cout << "id author Nathan Lo" << std::endl;
  std::cout << "uciok" << std::endl;

  std::string line;
  while (std::getline(std::cin, line)) {
    log() << "Received line '" << line << "'" << std::endl;
    if (line == "isready") {
      std::cout << "readyok" << std::endl;
    } else if (line == "ucinewgame") {
      m_is_new_game = true;
    } else if (line.starts_with("position")) {
      handle_position(line);
    } else if (line.starts_with("go")) {
      handle_go(line);
    } else if (line == "stop") {
      // TODO: Interrupt pondering
    } else if (line == "quit")
      break;
  }
  log() << "Terminating session" << std::endl;
}
