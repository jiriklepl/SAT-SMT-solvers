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

inline satlib_parser::~satlib_parser() noexcept = default;

/**
 * @brief Parses a satlib-format file given in an input stream
 *
 * @param in the input stream
 * @param just_impl use only top-down implications ("gate implies literals/sub-gates")
 * @return std::unique_ptr<satlib_parser> containing the parsed cnf and a map of names
 */
std::unique_ptr<satlib_parser> satlib_parse(std::istream &in, bool just_impl = true);

#endif // SATLIB_HPP
