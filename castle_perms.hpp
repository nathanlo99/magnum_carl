
#pragma once

#include <array>
#include <cassert>
#include <cstdint>
#include <string>

using castle_t = uint8_t;

constexpr std::array<uint8_t, 64> castle_perm_update = {
    14, 15, 15, 15, 12, 15, 15, 13, //
    15, 15, 15, 15, 15, 15, 15, 15, //
    15, 15, 15, 15, 15, 15, 15, 15, //
    15, 15, 15, 15, 15, 15, 15, 15, //
    15, 15, 15, 15, 15, 15, 15, 15, //
    15, 15, 15, 15, 15, 15, 15, 15, //
    15, 15, 15, 15, 15, 15, 15, 15, //
    11, 15, 15, 15, 03, 15, 15, 07, //
};

enum : castle_t {
  WhiteLongCastle = 1,
  WhiteShortCastle = 2,
  BlackLongCastle = 4,
  BlackShortCastle = 8,
};

inline std::string castle_perms_to_string(const castle_t perms) {
  assert(perms <= 15);
  if (perms == 0)
    return "-";
  std::string result = "";
  if (perms & WhiteShortCastle)
    result.push_back('K');
  if (perms & WhiteLongCastle)
    result.push_back('Q');
  if (perms & BlackShortCastle)
    result.push_back('k');
  if (perms & BlackShortCastle)
    result.push_back('q');
  return result;
}
