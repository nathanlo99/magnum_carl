
#pragma once

#include "bitboard.hpp"
#include "piece.hpp"

// Generated through gen_magic_numbers() in gen_magic.hpp
constexpr std::array<bitboard_t, 64> bishop_magics = {
    0x0020011001004080ULL, // a1 (6 bits)
    0x0004042802002000ULL, // b1 (6 bits)
    0x0004010202000000ULL, // c1 (4 bits)
    0x0004040080000000ULL, // d1 (3 bits)
    0x0001104000000000ULL, // e1 (3 bits)
    0x0000821040000000ULL, // f1 (4 bits)
    0x0000821010040000ULL, // g1 (5 bits)
    0x0000410090012000ULL, // h1 (6 bits)
    0x0000080801080210ULL, // a2 (6 bits)
    0x0000101002004840ULL, // b2 (6 bits)
    0x0000040102020000ULL, // c2 (4 bits)
    0x0000040400800000ULL, // d2 (3 bits)
    0x0000011040000000ULL, // e2 (3 bits)
    0x0000008210400000ULL, // f2 (4 bits)
    0x0000004208044000ULL, // g2 (5 bits)
    0x0000002404020800ULL, // h2 (5 bits)
    0x0010800820080080ULL, // a3 (6 bits)
    0x0011000802008400ULL, // b3 (6 bits)
    0x0002001000220020ULL, // c3 (5 bits)
    0x0000800802004000ULL, // d3 (4 bits)
    0x0000800400a00000ULL, // e3 (4 bits)
    0x0002000040500400ULL, // f3 (5 bits)
    0x0000800208040200ULL, // g3 (5 bits)
    0x0000800040480800ULL, // h3 (5 bits)
    0x0004400004080800ULL, // a4 (5 bits)
    0x0002200010010200ULL, // b4 (5 bits)
    0x0001100001004200ULL, // c4 (5 bits)
    0x0010040000401020ULL, // d4 (5 bits)
    0x0000840000802000ULL, // e4 (4 bits)
    0x000040800100a000ULL, // f4 (5 bits)
    0x0001020000421000ULL, // g4 (5 bits)
    0x0000404000840400ULL, // h4 (5 bits)
    0x0008084000080200ULL, // a5 (5 bits)
    0x0001082000020400ULL, // b5 (5 bits)
    0x0000440200100020ULL, // c5 (5 bits)
    0x0000020080080080ULL, // d5 (4 bits)
    0x0008020400001100ULL, // e5 (5 bits)
    0x0002004200010080ULL, // f5 (5 bits)
    0x0008008100008800ULL, // g5 (5 bits)
    0x0002008100002400ULL, // h5 (5 bits)
    0x0000842008002000ULL, // a6 (5 bits)
    0x0001080104001000ULL, // b6 (5 bits)
    0x0000082088001000ULL, // c6 (5 bits)
    0x0000002011000800ULL, // d6 (4 bits)
    0x0000080100400400ULL, // e6 (4 bits)
    0x0040008080800100ULL, // f6 (5 bits)
    0x0010010104000108ULL, // g6 (6 bits)
    0x0008008080840200ULL, // h6 (6 bits)
    0x0001010802400000ULL, // a7 (5 bits)
    0x0001010082200000ULL, // b7 (5 bits)
    0x0000002084100000ULL, // c7 (4 bits)
    0x0000000020880000ULL, // d7 (3 bits)
    0x0000001002020000ULL, // e7 (3 bits)
    0x0000040408020000ULL, // f7 (4 bits)
    0x0010050808004000ULL, // g7 (6 bits)
    0x0010040084004400ULL, // h7 (6 bits)
    0x0000104808041000ULL, // a8 (6 bits)
    0x0000002402021000ULL, // b8 (5 bits)
    0x0000000020841000ULL, // c8 (4 bits)
    0x0000000000208800ULL, // d8 (3 bits)
    0x0000000010020200ULL, // e8 (3 bits)
    0x0000000404080200ULL, // f8 (4 bits)
    0x0000100401080220ULL, // g8 (6 bits)
    0x0010020081020200ULL, // h8 (6 bits)
};

constexpr std::array<bitboard_t, 64> rook_magics = {
    0x0080002080400010ULL, // a1 (5 bits)
    0x0040001000200040ULL, // b1 (4 bits)
    0x0080100020008008ULL, // c1 (5 bits)
    0x0080100008008004ULL, // d1 (5 bits)
    0x0080080080040002ULL, // e1 (5 bits)
    0x0100010002080400ULL, // f1 (5 bits)
    0x0080008001000200ULL, // g1 (4 bits)
    0x0100020080402100ULL, // h1 (6 bits)
    0x0000800020400080ULL, // a2 (4 bits)
    0x0000400020005000ULL, // b2 (4 bits)
    0x0000801000200080ULL, // c2 (4 bits)
    0x0000800800100080ULL, // d2 (4 bits)
    0x0000800400080080ULL, // e2 (4 bits)
    0x0000800200040080ULL, // f2 (4 bits)
    0x0000800100020080ULL, // g2 (4 bits)
    0x0000800040800100ULL, // h2 (4 bits)
    0x0000208000400080ULL, // a3 (4 bits)
    0x0000404000201000ULL, // b3 (4 bits)
    0x0000808010002000ULL, // c3 (4 bits)
    0x0000808008001000ULL, // d3 (4 bits)
    0x0000808004000800ULL, // e3 (4 bits)
    0x0000808002000400ULL, // f3 (4 bits)
    0x0000010100020004ULL, // g3 (4 bits)
    0x0000020000804401ULL, // h3 (5 bits)
    0x0000208080004000ULL, // a4 (4 bits)
    0x0000200040005000ULL, // b4 (4 bits)
    0x0000100080200080ULL, // c4 (4 bits)
    0x0000080080100080ULL, // d4 (4 bits)
    0x0000040080080080ULL, // e4 (4 bits)
    0x0000020080040080ULL, // f4 (4 bits)
    0x0000010080800200ULL, // g4 (4 bits)
    0x0000800080004100ULL, // h4 (4 bits)
    0x0000204000800080ULL, // a5 (4 bits)
    0x0000200040401000ULL, // b5 (4 bits)
    0x0000100080802000ULL, // c5 (4 bits)
    0x0000080080801000ULL, // d5 (4 bits)
    0x0000040080800800ULL, // e5 (4 bits)
    0x0000020080800400ULL, // f5 (4 bits)
    0x0000020001010004ULL, // g5 (4 bits)
    0x0000800040800100ULL, // h5 (4 bits)
    0x0000204000808000ULL, // a6 (4 bits)
    0x0000200040008080ULL, // b6 (4 bits)
    0x0000100020008080ULL, // c6 (4 bits)
    0x0000080010008080ULL, // d6 (4 bits)
    0x0000040008008080ULL, // e6 (4 bits)
    0x0000020004008080ULL, // f6 (4 bits)
    0x0000010002008080ULL, // g6 (4 bits)
    0x0000008100420004ULL, // h6 (5 bits)
    0x0000204000800080ULL, // a7 (4 bits)
    0x0000200040008080ULL, // b7 (4 bits)
    0x0000100020008080ULL, // c7 (4 bits)
    0x0000080010008080ULL, // d7 (4 bits)
    0x0000040008008080ULL, // e7 (4 bits)
    0x0000020004008080ULL, // f7 (4 bits)
    0x0000800100020080ULL, // g7 (4 bits)
    0x0000800041000080ULL, // h7 (4 bits)
    0x0000208001004011ULL, // a8 (6 bits)
    0x0001001080204001ULL, // b8 (6 bits)
    0x0000090040102001ULL, // c8 (6 bits)
    0x0000200500100009ULL, // d8 (6 bits)
    0x0002000420100802ULL, // e8 (6 bits)
    0x0001000804000201ULL, // f8 (5 bits)
    0x0001000082000441ULL, // g8 (6 bits)
    0x0000008021040042ULL, // h8 (6 bits)
};

extern std::array<bitboard_t, 64> bishop_masks;
extern std::array<bitboard_t, 64> rook_masks;
extern std::array<std::array<bitboard_t, 512>, 64> bishop_attacks;
extern std::array<std::array<bitboard_t, 4096>, 64> rook_attacks;

constexpr inline void init_slider_attack_tables() {
  for (square_t sq = 0; sq < 64; ++sq) {
    bishop_masks[sq] = get_bishop_attack_mask(sq);
    rook_masks[sq] = get_rook_attack_mask(sq);

    { // Bishops
      const bitboard_t attack_mask = bishop_masks[sq];
      const size_t relevant_bits = bishop_num_relevant_bits[sq];
      __builtin_assume(relevant_bits > 0);
      const int max_entries = 1 << relevant_bits;
      assert(max_entries <= 512);

      for (int index = 0; index < max_entries; ++index) {
        const bitboard_t occupancy =
            set_occupancy(index, relevant_bits, attack_mask);
        const int magic_index =
            (occupancy * bishop_magics[sq]) >> (64 - relevant_bits);
        const bitboard_t actual_attacks =
            generate_bishop_attacks(sq, occupancy);
        assert(bishop_attacks[sq][magic_index] == 0 ||
               bishop_attacks[sq][magic_index] == actual_attacks);
        bishop_attacks[sq][magic_index] = actual_attacks;
      }
    }

    { // Rooks
      const bitboard_t attack_mask = rook_masks[sq];
      const size_t relevant_bits = rook_num_relevant_bits[sq];
      __builtin_assume(relevant_bits > 0);
      const int max_entries = 1 << relevant_bits;
      assert(max_entries <= 4096);

      for (int index = 0; index < max_entries; ++index) {
        const bitboard_t occupancy =
            set_occupancy(index, relevant_bits, attack_mask);
        const int magic_index =
            (occupancy * rook_magics[sq]) >> (64 - relevant_bits);
        const bitboard_t actual_attacks = generate_rook_attacks(sq, occupancy);
        assert(rook_attacks[sq][magic_index] == 0 ||
               rook_attacks[sq][magic_index] == actual_attacks);
        rook_attacks[sq][magic_index] = actual_attacks;
      }
    }
  }
}

constexpr inline bitboard_t get_bishop_attacks(const square_t sq,
                                               bitboard_t occ) {
  occ &= bishop_masks[sq];
  occ *= bishop_magics[sq];
  occ >>= 64 - bishop_num_relevant_bits[sq];
  return bishop_attacks[sq][occ];
}

constexpr inline bitboard_t get_rook_attacks(const square_t sq,
                                             bitboard_t occ) {
  occ &= rook_masks[sq];
  occ *= rook_magics[sq];
  occ >>= 64 - rook_num_relevant_bits[sq];
  return rook_attacks[sq][occ];
}

constexpr inline bitboard_t get_queen_attacks(const square_t sq,
                                              const bitboard_t occ) {
  return get_bishop_attacks(sq, occ) | get_rook_attacks(sq, occ);
}

template <piece_t piece>
constexpr inline bitboard_t get_attacks(const square_t sq,
                                        [[maybe_unused]] const bitboard_t occ) {
  if constexpr (piece == WhitePawn)
    return pawn_attack_bitboards[White][sq];
  if constexpr (piece == BlackPawn)
    return pawn_attack_bitboards[Black][sq];
  if constexpr (piece == WhiteKnight || piece == BlackKnight)
    return knight_attack_bitboards[sq];
  if constexpr (piece == WhiteKing || piece == BlackKing)
    return king_attack_bitboards[sq];
  if constexpr (piece == WhiteBishop || piece == BlackBishop)
    return get_bishop_attacks(sq, occ);
  if constexpr (piece == WhiteRook || piece == BlackRook)
    return get_rook_attacks(sq, occ);
  if constexpr (piece == WhiteQueen || piece == BlackQueen)
    return get_queen_attacks(sq, occ);
}
