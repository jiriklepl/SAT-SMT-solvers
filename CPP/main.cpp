#include <chrono>
#include <cstdint>
#include <fstream>
#include <iostream>
#include <optional>
#include <string>
#include <sstream>
#include <unordered_set>

#include "dimacs.hpp"
#include "satlib.hpp"
#include "watched.hpp"
#include "watched_sep.hpp"
#include "watched_cdcl.hpp"
#include "watched_sep_cdcl.hpp"
#include "adjacency.hpp"

void usage(const char *name, std::ostream &out) {
    out << "Usage: " << name << " [Options] [input [output]]" << std::endl << std::endl;

    out << "Valid options:" << std::endl;
    out << "  -e, --equiv: for tseitsing encoding of satlib formulas, use equivalences" << std::endl;
    out << "  -c, --dimacs: the input is in the dimacs format (for files, regardless of the file extension)" << std::endl;
    out << "  -s, --satlib: the input is in the satlib format (for files, regardless of the file extension)" << std::endl;
    out << "  -h, --help: print this usage help" << std::endl;
    out << "  -l LIMIT, --cache-limit LIMIT: set the initial cache LIMIT for learned clauses (relevant only in the CDCL_WATCHED_SEP mode)" << std::endl;
    out << "  -u LENGTH, --cache-limit LENGTH: set the unit run LENGTH (where LENGTH is a positive number) which acts as a multiplicator for the number of contradictions between subsequent restarts (relevant only in the CDCL_WATCHED or CDCL_WATCHED_SEP mode)" << std::endl << std::endl;

    out << "  mode modifiers: (the default mode is CDCL_WATCHED_SEP)" << std::endl;
    out << "    -a,   --adjacency: switches the parser to the ADJACENCY mode" << std::endl;
    out << "    -w,   --watched: switches the parser to the WATCHED mode" << std::endl;
    out << "    -ws,  --watched-sep: switches the parser to the WATCHED_SEP mode" << std::endl;
    out << "    -cw,  --cdcl-watched: switches the parser to the CDCL_WATCHED mode" << std::endl;
    out << "    -cws, --cdcl-watched-sep: switches the parser to the CDCL_WATCHED_SEP mode" << std::endl;
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

    int unit_run = 100;
    std::size_t cache_limit = 10'000;

    enum {
        ADJACENCY,
        WATCHED,
        WATCHED_SEP,
        CDCL_WATCHED_SEP,
        CDCL_WATCHED,
    } solver_type = CDCL_WATCHED_SEP;

    for (const char** arg = argv + 1; *arg != nullptr; ++arg) {
        if (**arg == '-') {
            if (*arg == "-e"s || *arg == "--equiv"s) {
                just_impl = false;
            } else if (*arg == "-c"s || *arg == "--dimacs"s) {
                forced_dimacs = true;
                forced_satlib = false;
            } else if (*arg == "-s"s || *arg == "--satlib"s) {
                forced_dimacs = false;
                forced_satlib = true;
            } else if (*arg == "-h"s || *arg == "--help"s) {
                usage(*argv, std::cout);
                return 0;
            } else if (*arg == "-l"s || *arg == "--cache-limit"s) {
                ++arg;

                if (*arg == nullptr) {
                    usage(*argv, std::cerr);
                    return 1;
                }

                std::istringstream(*arg) >> cache_limit;
            } else if (*arg == "-u"s || *arg == "--unit-run"s) {
                ++arg;

                if (*arg == nullptr || (std::istringstream(*arg) >> unit_run, unit_run) <= 0) {
                    usage(*argv, std::cerr);
                    return 1;
                }
            } else if (*arg == "-a"s || *arg == "--adjacency"s) {
                solver_type = ADJACENCY;
            } else if (*arg == "-w"s || *arg == "--watched"s) {
                solver_type = WATCHED;
            } else if (*arg == "-ws"s || *arg == "--watched-sep"s) {
                solver_type = WATCHED_SEP;
            } else if (*arg == "-cw"s || *arg == "--cdcl-watched"s) {
                solver_type = CDCL_WATCHED;
            } else if (*arg == "-cws"s || *arg == "--cdcl-watched-sep"s) {
                solver_type = CDCL_WATCHED_SEP;
            } else {
                std::cerr << "Unknown option: " << *arg << std::endl;
                usage(*argv, std::cerr);
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
            usage(*argv, std::cerr);
            return 1;
        }

    }

    std::unique_ptr<Solver> solver;

    switch (solver_type) {
    case ADJACENCY:
        solver = std::make_unique<SolverAdjacency>();
    break;
    case WATCHED:
        solver = std::make_unique<SolverWatched>();
    break;
    case WATCHED_SEP:
        solver = std::make_unique<SolverWatchedSep>();
    break;
    case CDCL_WATCHED_SEP:
        solver = std::make_unique<SolverWatchedSepCDCL>(unit_run, cache_limit);
    break;
    case CDCL_WATCHED:
        solver = std::make_unique<SolverWatchedCDCL>(unit_run);
    break;
    }

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
