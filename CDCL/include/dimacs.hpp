#ifndef DIMACS_HPP
#define DIMACS_HPP

#include  <iostream>
#include  <string>
#include  <cstdint>
#include  <vector>
#include  <unordered_map>
#include  <memory>

using lit_t = std::int32_t;

class dimacs_parser {
public:
    dimacs_parser() : cnf(), var_counter(0) {}
    virtual ~dimacs_parser() noexcept = 0;
    std::vector<std::vector<lit_t>> cnf;
    lit_t var_counter;
};

inline dimacs_parser::~dimacs_parser() = default;

std::unique_ptr<dimacs_parser> dimacs_parse(std::istream &in);

#endif // DIMACS_HPP