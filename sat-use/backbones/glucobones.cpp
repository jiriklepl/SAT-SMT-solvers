#include <memory>

#include <glucose/core/Solver.h>
#include <glucose/core/Dimacs.h>

#include "main.hpp"

using namespace Glucose;


namespace {

std::unique_ptr<Solver> solver;

void init(int argc, char *argv[])
{
    solver = std::make_unique<Solver>();

    if (argc == 1)
        printf("Reading from standard input... Use '--help' for help.\n");

    gzFile in = (argc == 1) ? gzdopen(0, "rb") : gzopen(argv[1], "rb");

    if (in == NULL)
        printf("ERROR! Could not open file: %s\n", argc == 1 ? "<stdin>" : argv[1]), exit(1);

    parse_DIMACS(in, *solver);
    gzclose(in);
}

int run()
{
    int called = 1;

    vec<Lit> dummy;
    if (solver->solveLimited(dummy) != l_True)
        return 0;

    vec<Lit> block;

    suspected.resize(solver->model.size() + 1);
    block.capacity(solver->model.size());

    for (int i = 0; i < solver->model.size(); ++i) {
        if (solver->model[i] == l_True)
            suspected[i + 1] = TRUE,
            block.push_(mkLit(i, true));
        else if (solver->model[i] == l_False)
            suspected[i + 1] = FALSE,
            block.push_(mkLit(i, false));
        else
            suspected[i + 1] = UNSUS;
    }

    solver->addClause(block);

    while (++called, solver->solveLimited(dummy) == l_True) {
        block.clear();

        for (int i = 0; i < solver->model.size(); ++i) {
            if (suspected[i + 1] == TRUE && solver->model[i] == l_True)
                block.push_(mkLit(i, true));
            else if (suspected[i + 1] == FALSE && solver->model[i] == l_False)
                block.push_(mkLit(i, false));
            else
                suspected[i + 1] = UNSUS;
        }

        if ((std::size_t)solver->model.size() < suspected.size())
            suspected.resize(solver->model.size() + 1);

        solver->addClause(block);
    }

    return called;
}

}
