#include <chrono>
#include <cstdint>
#include <fstream>
#include <iostream>
#include <string>
#include <unordered_set>

#include "dimacs.hpp"
#include "satlib.hpp"
#include "watched.hpp"
#include "adjacency.hpp"

void usage(std::ostream &out) {
    out << "Usage: dpll [Options] [input [output]]" << std::endl;
}

using namespace std::string_literals;

int main(int, const char *argv[])
{
    std::optional<std::ifstream> input;
    std::optional<std::ofstream> output;
    std::unique_ptr<satlib_parser> parser;
    std::size_t parsed_files = 0;
    const char *input_name = "/dev/stdin";
    bool just_impl = true;
    bool forced_dimacs = false;
    bool forced_satlib = false;
    bool adjacency = false;

    for (const char** arg = argv + 1; *arg != nullptr; ++arg) {
        if (**arg == '-') {
            if (*arg == "-e"s || *arg == "--eq"s) {
                just_impl = false;
            } else if (*arg == "-c"s || *arg == "--dimacs"s) {
                forced_dimacs = true;
                forced_satlib = false;
            } else if (*arg == "-s"s || *arg == "--satlib"s) {
                forced_dimacs = false;
                forced_satlib = true;
            } else if (*arg == "-h"s || *arg == "--help"s) {
                usage(std::cout);
                return 0;
            } else if (*arg == "-a"s || *arg == "--adjacency"s) {
                adjacency = true;
            } else {
                std::cerr << "Unknown option: " << *arg << std::endl;
                usage(std::cerr);
                return 1;
            }

            continue;
        }

        switch (parsed_files++) {
        case 0:
            input = std::ifstream(input_name = *arg);
            if (!input->good()) {
                std::cerr << "Failed to open file: " << *arg << std::endl;
                return 2;
            }
        break;
        case 1:
            output = std::ofstream(*arg);
            if (!output->good()) {
                std::cerr << "Failed to open file: " << *arg << std::endl;
                return 2;
            }
        break;
        default:
            usage(std::cerr);
            return 1;
        }

    }

    std::unique_ptr<Solver> solver;

    if (adjacency)
        solver = std::make_unique<SolverAdjacency>();
    else
        solver = std::make_unique<SolverWatched>();

    std::istream& in = input.has_value() ? *input : std::cin;
    std::ostream& out = output.has_value() ? *output : std::cout;

    {
        std::unique_ptr<cnf_parser> parser;

        if (forced_dimacs) {
            parser = dimacs_parse(in);
        } else if (forced_satlib) {
            parser = satlib_parse(in, just_impl);
        } else {
            std::string name = input_name;
            auto pos = name.find_last_of('.');

            if (pos == name.npos) {
                std::cerr << "Couldn't determine the format";
                return 3;
            }

            std::string ext = name.substr(pos + 1);

            if (ext == "sat") {
                parser = satlib_parse(in, just_impl);
            } else if (ext == "cnf") {
                parser = dimacs_parse(in);
            } else {
                std::cerr << "The file has an unknown format";
                return 3;
            }

        }

        solver->set(parser->cnf);
    }


    auto start = std::chrono::high_resolution_clock::now();

    bool solved = solver->solve();

    auto end = std::chrono::high_resolution_clock::now();

    auto difference = end - start;

    if (solved)
        out << "s SATISFIABLE" << std::endl;
    else
        out << "s UNSATISFIABLE" << std::endl;

    out << "Took " << 1000.L * decltype(difference)::period::num * difference.count() / decltype(difference)::period::den << " ms" << std::endl;

    if (!solved)
        return 20;


    out << "Made " << solver->decided << " decisions" << std::endl;
    out << "Derived " << solver->derived << " variables" << std::endl;

    std::sort(solver->assign.assigned.begin(), solver->assign.assigned.end());

    for (auto &&a : solver->assign.assigned) {
        out << a << ": " << (solver->assign.variables[a] > 0 ?  "true" : solver->assign.variables[a] < 0 ? "false" : "free") << std::endl;
    }

    return 10;
}
