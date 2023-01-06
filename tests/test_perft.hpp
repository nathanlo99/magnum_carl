
#pragma once

#include "perft.hpp"

#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

struct PerftTest {
  std::string fen;
  std::vector<uint64_t> expected;
};

inline std::vector<PerftTest> parse_perft_file(const std::string &filename) {
  std::vector<PerftTest> result;
  std::ifstream ifs(filename);
  std::string line;

  while (std::getline(ifs, line)) {
    PerftTest next_test;
    std::string token;
    std::istringstream iss{line};
    for (int idx = 0; std::getline(iss, token, ','); ++idx) {
      if (idx == 0)
        next_test.fen = token;
      else
        next_test.expected.push_back(std::stoull(token));
    }
    result.push_back(next_test);
  }

  return result;
}

inline void run_perft_tests(const std::string &filename, size_t start_idx = 0,
                            size_t max_depth = 12) {
  const auto &perft_tests = parse_perft_file(filename);
  for (size_t idx = start_idx; idx < perft_tests.size(); ++idx) {
    const PerftTest &test = perft_tests[idx];
    std::cout << "(" << (idx + 1) << "/" << perft_tests.size() << ")"
              << " FEN: '" << test.fen << "'" << std::endl;

    for (size_t i = 0; i < test.expected.size(); ++i) {
      const size_t depth = i + 1;
      const uint64_t expected = test.expected[i];
      if (depth > max_depth)
        continue;
      std::cout << "Starting test to depth " << depth << " [" << std::setw(11)
                << std::setfill(' ') << expected << "]   " << std::flush;

      const auto start_ns = get_time_ns();
      const uint64_t perft_result = perft_test(test.fen, depth);
      const auto end_ns = get_time_ns();
      if (perft_result != expected) {
        std::cout << "Perft test failed!" << std::endl;
        std::cout << "FEN:      '" << test.fen << "'" << std::endl;
        std::cout << "Depth:    " << depth << std::endl;
        std::cout << "Expected: " << expected << std::endl;
        std::cout << "Actual:   " << perft_result << std::endl;
        exit(1);
      }

      const int positions_per_second = (expected * 1e9) / (end_ns - start_ns);
      std::cout << "[" << std::setw(9) << std::setfill(' ')
                << positions_per_second << " pps"
                << "]" << std::endl;
    }
    std::cout << std::endl;
  }
}
