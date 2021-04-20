#include <iostream>


#include <minisat/core/Solver.h>
#include <minisat/core/Dimacs.h>

#include "main.hpp"

using namespace Minisat;

Solver solver;

namespace {

void init(int argc, char *argv[])
{

    if (argc == 1)
        printf("Reading from standard input... Use '--help' for help.\n");

    gzFile in = (argc == 1) ? gzdopen(0, "rb") : gzopen(argv[1], "rb");

    if (in == NULL)
        printf("ERROR! Could not open file: %s\n", argc == 1 ? "<stdin>" : argv[1]), exit(1);

    parse_DIMACS(in, solver);
    gzclose(in);
}

int run()
{
    int called = 1;

    if (!solver.solve())
        return 0;

    vec<Lit> block;

    suspected.resize(solver.model.size() + 1);
    block.capacity(solver.model.size());

    for (int i = 0; i < solver.model.size(); ++i) {
        if (solver.model[i] == l_True)
            suspected[i + 1] = TRUE,
            block.push_(mkLit(i, true));
        else if (solver.model[i] == l_False)
            suspected[i + 1] = FALSE,
            block.push_(mkLit(i, false));
        else
            suspected[i + 1] = UNSUS;
    }

    solver.addClause(block);

    while (++called, solver.solve()) {
        block.clear();

        for (int i = 0; i < solver.model.size(); ++i) {
            if (suspected[i + 1] == TRUE && solver.model[i] == l_True)
                block.push_(mkLit(i, true));
            else if (suspected[i + 1] == FALSE && solver.model[i] == l_False)
                block.push_(mkLit(i, false));
            else
                suspected[i + 1] = UNSUS;
        }

        if ((std::size_t)solver.model.size() < suspected.size())
            suspected.resize(solver.model.size() + 1);

        solver.addClause(block);
    }

    return called;
}

}
