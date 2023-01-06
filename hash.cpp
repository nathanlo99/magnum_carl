
#include "hash.hpp"

std::array<std::array<hash_t, 64>, 12> piece_keys;
std::array<hash_t, 64> en_passant_keys;
std::array<hash_t, 16> castle_keys;
hash_t side_key;
