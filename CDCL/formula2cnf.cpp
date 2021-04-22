#include <algorithm>
#include <cassert>
#include <fstream>
#include <iostream>
#include <utility>
#include <string>

#include "satlib.hpp"

int main(int argc, const char *argv[])
{
    std::unique_ptr<satlib_parser> parser;
    bool just_impl = false;

    if (argc == 2) {
        auto file = std::ifstream(argv[1]);
        parser = satlib_parse(file, just_impl);
    } else {
        parser = satlib_parse(std::cin, just_impl);
    }

    if (parser->cnf.empty()) {
        std::cout << "s TAUTOLOGY" << std::endl;
        return 10;
    }

    std::vector<lit_t> variables;

    if (!just_impl)
        variables.reserve(parser->names.size());

    for (auto &&[name, var] : parser->names) {
        assert(var > 0);

        std::cout << "c variable " << var << " : " << name << std::endl;

        if (!just_impl)
            variables.push_back(var);
    }

    if (!just_impl)
        std::sort(variables.begin(), variables.end());

    bool finish_and = false;
    for (auto it = parser->cnf.begin(), end = parser->cnf.end() - 1; it < end; ++it) {
        auto &&clause = *it;
        assert(!clause.empty());


        
        if (finish_and) {
            assert(clause[0] < 0);
            assert(clause.size() == 2);

            std::cout << " and " << clause[1] << std::endl;

            finish_and = false;
            continue;
        }

        if (clause[0] > 0)
            // the clause does not match "C -> ..."
            continue;

        assert(clause.size() == 2 || clause.size() == 3);

        // if just_impl, `clause[0]` has to be a gate, otherwise we check whether it is really a gate
        if (just_impl || !std::binary_search(variables.begin(), variables.end(), -clause[0])) {
            std::cout <<  "c gate " << -clause[0] << " : " << clause[1];

            if (clause.size() == 2)
                finish_and = true;
            else
                std::cout << " or " << clause[2] << std::endl;
        }
    }

    assert(parser->cnf.back().size() == 1);
    std::cout << "c root " << parser->cnf.back()[0] << std::endl;

    std::cout << "p cnf " << parser->var_counter << ' ' << parser->cnf.size() << std::endl;

    for (auto &&clause : parser->cnf) {
        for (auto &&lit : clause)
            std::cout << lit << ' ';
        std::cout << '0' << std::endl;
    }
}
