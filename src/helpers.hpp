#pragma once

// Helpers for pattern-matching-ish syntax for std::visit taken from here:
// https://en.cppreference.com/w/cpp/utility/variant/visit
template <class ...Ts> struct PatternMatch : Ts... { using Ts::operator()...; };
template <class ...Ts> PatternMatch(Ts...) -> PatternMatch<Ts...>;
