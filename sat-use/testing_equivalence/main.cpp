#include <chrono>
#include <fstream>
#include <iostream>

#include "minisat/core/Solver.h"
#include "minisat/core/Dimacs.h"
#include "./../../CPP/include/dimacs.hpp"

using namespace Minisat;

// all "+/- 1" in the following functions are due to the fact minisat indexes variables from 0 and we from 1

bool long_call(Solver &solver, const std::vector<std::vector<lit_t>> &cnf)
{
    vec<Lit> root_clause;
    vec<Lit> gate_lit_clause;
    gate_lit_clause.capacity(2);

    root_clause.capacity(cnf.size());

    for (auto &&clause : cnf) {
        auto gate = solver.newVar();
        root_clause.push_(mkLit(gate, false)); // gate1 \/ gate2 \/ ... (we want +gate)

        for (auto &&lit : clause) {
            gate_lit_clause.clear();
            gate_lit_clause.push_(mkLit(gate, true)); // gate -> -lit (we want -gate)

            while (solver.nVars() < std::abs(lit))
                solver.newVar();

            if (lit > 0)
                gate_lit_clause.push_(mkLit(lit - 1, true));  // change var to -var (= -lit)
            else
                gate_lit_clause.push_(mkLit(-lit - 1, false)); // change -var to var (= -lit)

            solver.addClause(gate_lit_clause);
        }
    }

    solver.addClause(root_clause);

    return !solver.solve();
}

bool long_eq_call(Solver &solver, const std::vector<std::vector<lit_t>> &cnf)
{
    vec<Lit> root_clause;
    vec<Lit> gate_clause;
    vec<Lit> gate_lit_clause;
    gate_lit_clause.capacity(2);

    root_clause.capacity(cnf.size());

    for (auto &&clause : cnf) {
        auto gate = solver.newVar();

        root_clause.push_(mkLit(gate, false)); // gate1 \/ gate2 \/ ... (we want +gate)

        gate_clause.clear();
        if ((std::size_t)gate_clause.capacity() < clause.size() + 1)
            gate_clause.capacity(clause.size() + 1);

        gate_clause.push_(mkLit(gate, false)); // ... -> gate (we want +gate)

        for (auto &&lit : clause) {
            gate_lit_clause.clear();
            gate_lit_clause.push_(mkLit(gate, true)); // gate -> -lit (we want -gate)


            while (solver.nVars() < std::abs(lit))
                solver.newVar();

            if (lit > 0) {
                gate_lit_clause.push_(mkLit(lit - 1, true));  // change var to -var (= -lit)
                gate_clause.push_(mkLit(lit - 1, false)); // lit1 /\ lit2 /\ ... -> gate (we want -(-lit) = lit)
            } else {
                gate_lit_clause.push_(mkLit(-lit - 1, false)); // change -var to var (= -lit)
                gate_clause.push_(mkLit(-lit - 1, true)); // lit1 /\ lit2 /\ ... -> gate (we want -(-lit) = lit)
            }

            solver.addClause(gate_lit_clause);
        }

        solver.addClause(gate_clause);
    }

    solver.addClause(root_clause);

    return !solver.solve();
}

bool short_calls(Solver &solver, const std::vector<std::vector<lit_t>> &cnf) {
    vec<Lit> assump;
    for (auto &&clause : cnf) {
        assump.clear();
        if ((std::size_t)assump.capacity() < clause.size())
            assump.capacity(clause.size());


        for (auto &&lit : clause) {
            if (solver.nVars() < std::abs(lit))
                continue;

            if (lit > 0)
                assump.push_(mkLit(lit - 1, true));  // change var to -var
            else
                assump.push_(mkLit(-lit - 1, false)); // change -var to var
        }

        if (solver.solve(assump))
            return false;
    }

    return true;
}

int main(int argc, char *argv[])
{
    using namespace std::string_literals;

    enum {
        LONG,
        LONG_EQ,
        SHORT
    } long_opt;

    if (argc != 4) {
        printf("Usage: long|long_eq|short INPUT1 INPUT2.\n");
        return 1;
    }

    if (argv[1] == "short"s) {
        long_opt = SHORT;
    } else if (argv[1] == "long"s) {
        long_opt = LONG;
    } else if (argv[1] == "long_eq"s) {
        long_opt = LONG_EQ;
    } else {
        printf("The first argument has to be either `long', `long_eq' or `short'.\n");
        return 1;
    }

    Solver solver;

    gzFile in1 = gzopen(argv[2], "rb");
    std::ifstream in2(argv[3]);

    if (!in2.good()) {
        printf("Failed to open %s.\n", argv[3]);
        return 1;
    }

    parse_DIMACS(in1, solver);
    gzclose(in1);

    auto parser = dimacs_parse(in2);

    in2.close();

    auto start = std::chrono::high_resolution_clock::now();

    bool result;

    switch (long_opt) {
    case LONG:
        result = long_call(solver, parser->cnf);
    break;
    case LONG_EQ:
        result = long_eq_call(solver, parser->cnf);
    break;
    case SHORT:
        result = short_calls(solver, parser->cnf);
    break;
    }

    auto end = std::chrono::high_resolution_clock::now();

    auto difference = end - start;

    std::cout << "Took " << 1000.L * decltype(difference)::period::num * difference.count() / decltype(difference)::period::den << " ms" << std::endl;


    if (result) {
        std::cout << "s IMPLIED" << std::endl;
        return 10;
    }

    std::cout << "s UNIMPLIED" << std::endl;
    return 20;
}
