#ifndef CNF_PARSER_HPP
#define CNF_PARSER_HPP

#include  <cstdint>
#include  <vector>

using lit_t = std::int32_t;

class cnf_parser {
public:
    cnf_parser() : cnf(), var_counter(0) {};
    virtual ~cnf_parser() noexcept = 0;
    std::vector<std::vector<lit_t>> cnf;
    lit_t var_counter;
};

inline cnf_parser::~cnf_parser() noexcept = default;

#endif // CNF_PARSER_HPP
