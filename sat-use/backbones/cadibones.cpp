#include <cassert>
#include <cstddef>
#include <cstring>
#include <iostream>
#include <vector>

#include <cadical/cadical.hpp>

#include "main.hpp"

using namespace CaDiCaL;

namespace {

Solver solver;
int nvars;

void init(int argc, char *argv[])
{
    bool closeinputfile;
    const char *inputname = argc == 2 ? argv[1] : nullptr;
    FILE *inputfile;


    if (inputname) {
        if (!(inputfile = fopen (inputname, "r")))
        std::cerr << "can not read '" << inputname << '\'' << std::endl;
        closeinputfile = true;
    } else inputname = "<stdin>", inputfile = stdin, closeinputfile = false;

    if (argc == 1)
        std::cout << "Reading from standard input... Use '--help' for help." << std::endl;

    solver.read_dimacs(inputfile, 0, nvars);

    if (closeinputfile)
        fclose(inputfile);
}

int run()
{
    int called = 1;

    for (int i = 1; i <= nvars; ++i)
        solver.freeze(i);

    if (solver.solve() != 10)
        return 0;

    suspected.resize(nvars + 1);

    for (int i = 1; i <= nvars; ++i) {
        auto var = solver.val(i);
        if (var > 0)
            suspected[i] = TRUE;
        else if (var < 0)
            suspected[i] = FALSE;
        else
            solver.melt(i),
            suspected[i] = UNSUS;
    }

    for (int i = 1; i <= nvars; ++i) {
        if (suspected[i] == TRUE)
            solver.add(-i);
        else if (suspected[i] == FALSE)
            solver.add(i);
    }

    solver.add(0);

    while (++called, solver.solve() == 10) {

        for (int i = 1; i <= nvars; ++i) {
            auto var = solver.val(i);

            if (suspected[i] != UNSUS
            && (suspected[i] == TRUE ? var <= 0 : var >= 0))
                solver.melt(i),
                suspected[i] = UNSUS;
        }

        for (int i = 1; i <= nvars; ++i) {
            if (suspected[i] == TRUE)
                solver.add(-i);
            else if (suspected[i] == FALSE)
                solver.add(i);
        }

        solver.add(0);
    }

    return called;
}

}
