
#pragma once

#include <algorithm>
#include <cassert>
#include <chrono>
#include <cstring>
#include <iostream>

// Any number bigger than 2 * 16 * 900 = 28K, the maximum material evaluation
constexpr int MateScore = 50000;
constexpr int MaxMateDepth = 1000; // Maximum number of plies to mate
constexpr int DrawScore = -25;     // Negative for contempt
constexpr int UnknownScore = 69420;

#define __SHORT_FILE__ strrchr("/" __FILE__, '/') + 1

#ifdef LOG
#define log_print(expr)                                                        \
  std::cout << "LOG | " << __SHORT_FILE__ << ":" << __LINE__ << " ["           \
            << __func__ << "]: " << expr << std::endl
#else
#define log_print(expr)
#endif

#ifdef NDEBUG
#define debug_only(expr)
#define expect_equal(result, expected) __builtin_assume(result == expected)
#else
#define debug_only(expr) expr
#define expect_equal(result, expected)                                         \
  _expect_equal(result, expected, __SHORT_FILE__, __LINE__, __func__)

template <typename T, typename U>
constexpr void _expect_equal(const T &result, const U &expected,
                             const char *short_file, const int line,
                             const char *func) {
  if (result != expected) {
    std::cerr << short_file << ": " << line << " [" << func << "]\n";
    std::cerr << "expect_equal failed:\n";
    std::cerr << "  expected: " << expected << "\n";
    std::cerr << "  got     : " << result << "\n";
    exit(1);
  }
}
#endif

enum {
  White = 0,
  Black = 1,
  Both = 2,
};

constexpr inline int other_side(const int side) {
  assert(side == White || side == Black);
  return side ^ 1;
}

inline long long get_time_ns() {
  return std::chrono::duration_cast<std::chrono::nanoseconds>(
             std::chrono::system_clock::now().time_since_epoch())
      .count();
}

inline long long get_time_ms() {
  return std::chrono::duration_cast<std::chrono::milliseconds>(
             std::chrono::system_clock::now().time_since_epoch())
      .count();
}
