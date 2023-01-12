
#pragma once

#include <fstream>
#include <iostream>

constexpr const char *debug_log_filename = LOG_DIR "magnum_carl_debug.log";
constexpr const char *log_filename = LOG_DIR "magnum_carl.log";

enum LogFile { Default, Debug, Stderr };

inline std::ostream &log(const LogFile log = Stderr) {
  static std::ofstream default_log(log_filename, std::ios::app);
  static std::ofstream debug_log(debug_log_filename, std::ios::app);
  switch (log) {
  case Default:
    return default_log;
  case Debug:
    return debug_log;
  case Stderr:
    return std::cerr;
  }
  __builtin_unreachable();
}
