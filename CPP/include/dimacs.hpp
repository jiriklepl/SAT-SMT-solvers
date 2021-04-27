#ifndef DIMACS_HPP
#define DIMACS_HPP

#include  <iostream>
#include  <cstdint>
#include  <memory>

#include "cnf_parser.hpp"

class dimacs_parser : public cnf_parser {
public:
    dimacs_parser() : cnf_parser() {}
    ~dimacs_parser() noexcept override = 0;
};

inline dimacs_parser::~dimacs_parser() noexcept = default;

std::unique_ptr<dimacs_parser> dimacs_parse(std::istream &in);

#endif // DIMACS_HPP
