#ifndef SATLIB_HPP
#define SATLIB_HPP

#include  <iostream>
#include  <string>
#include  <cstdint>
#include  <vector>
#include  <unordered_map>
#include  <memory>

using lit_t = std::int32_t;

class satlib_parser {
public:
    satlib_parser() : names(), cnf(), var_counter(0) {}
    virtual ~satlib_parser() noexcept = 0;
    std::unordered_map<std::string, lit_t> names;
    std::vector<std::vector<lit_t>> cnf;
    lit_t var_counter;
};

inline satlib_parser::~satlib_parser() = default;

std::unique_ptr<satlib_parser> satlib_parse(std::istream &in);

#endif // SATLIB_HPP