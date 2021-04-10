#ifndef SATLIB_HPP
#define SATLIB_HPP

#include  <iostream>
#include  <string>
#include  <unordered_map>
#include  <memory>

#include "cnf_parser.hpp"

class satlib_parser : public cnf_parser {
public:
    satlib_parser() : cnf_parser(), names() {}
    ~satlib_parser() noexcept override = 0;
    std::unordered_map<std::string, lit_t> names;
};

inline satlib_parser::~satlib_parser() = default;

std::unique_ptr<satlib_parser> satlib_parse(std::istream &in);

#endif // SATLIB_HPP