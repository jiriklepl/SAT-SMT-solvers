#include <algorithm>
#include <cassert>
#include <fstream>
#include <iostream>
#include <optional>
#include <string>
#include <utility>

#include "satlib.hpp"

using namespace std::string_literals;

void usage(std::ostream &out) {
    out << "Usage: formula2cnf [-e|--eq] [input [output]]" << std::endl;
}

int main(int argc, const char *argv[])
{
    std::optional<std::ifstream> input;
    std::optional<std::ofstream> output;
    std::unique_ptr<satlib_parser> parser;
    std::size_t parsed_files = 0;
    const char *input_name = "/dev/stdin";
    bool just_impl = true;

    for (const char** arg = argv + 1; *arg != nullptr; ++arg) {
        if (**arg == '-') {
            if (*arg == "-e"s || *arg == "--eq"s) {
                just_impl = false;
            } else if (*arg == "-h" || *arg == "--help") {
                usage(std::cout);
                return 0;
            } else {
                std::cerr << "Unknown option: " << *arg << std::endl;
                usage(std::cerr);
                exit(1);
            }

            continue;
        }

        switch (parsed_files++) {
        case 0:
            input = std::ifstream(input_name = *arg);
            if (!input->good()) {
                std::cerr << "Failed to open file: " << *arg << std::endl;
                exit(2);
            }
        break;
        case 1:
            output = std::ofstream(*arg);
            if (!output->good()) {
                std::cerr << "Failed to open file: " << *arg << std::endl;
                exit(2);
            }
        break;
        default:
            usage(std::cerr);
            exit(1);
        }

    }

    parser = satlib_parse(input.has_value() ? *input : std::cin, just_impl);

    std::ostream &out = output.has_value() ? *output : std::cout;

    out << "c file "  << input_name << std::endl;

    if (parser->cnf.empty()) {
        out << "p cnf 0 0" << std::endl;
        return 10;
    }

    std::vector<lit_t> variables;

    if (!just_impl)
        variables.reserve(parser->names.size());

    for (auto &&[name, var] : parser->names) {
        assert(var > 0);

        out << "c variable " << var << " : " << name << std::endl;

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

            out << " and " << clause[1] << std::endl;

            finish_and = false;
            continue;
        }

        if (clause[0] > 0)
            // the clause does not match "C -> ..."
            continue;

        assert(clause.size() == 2 || clause.size() == 3);

        // if just_impl, `clause[0]` has to be a gate, otherwise we check whether it is really a gate
        if (just_impl || !std::binary_search(variables.begin(), variables.end(), -clause[0])) {
            out <<  "c gate " << -clause[0] << " : " << clause[1];

            if (clause.size() == 2)
                finish_and = true;
            else
                out << " or " << clause[2] << std::endl;
        }
    }

    assert(parser->cnf.back().size() == 1);
    out << "c root " << parser->cnf.back()[0] << std::endl;

    out << "p cnf " << parser->var_counter << ' ' << parser->cnf.size() << std::endl;

    for (auto &&clause : parser->cnf) {
        for (auto &&lit : clause)
            out << lit << ' ';
        out << '0' << std::endl;
    }
}
