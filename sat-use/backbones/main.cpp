#include <iostream>

#include <minisat/core/Solver.h>
#include <minisat/core/Dimacs.h>

using namespace Minisat;

int main(int argc, char *argv[])
{
    Solver solver;

    if (argc == 1)
        printf("Reading from standard input... Use '--help' for help.\n");

    gzFile in = (argc == 1) ? gzdopen(0, "rb") : gzopen(argv[1], "rb");

    if (in == NULL)
        printf("ERROR! Could not open file: %s\n", argc == 1 ? "<stdin>" : argv[1]), exit(1);

    parse_DIMACS(in, solver);
    gzclose(in);

    if (!solver.solve()) {
        std::cout << "no backbones" << std::endl;

        return 20;
    }

    enum suspicion : std::uint8_t {
        TRUE,
        FALSE,
        UNSUS
    };

    vec<suspicion> suspected;
    vec<Lit> block;

    suspected.capacity(solver.model.size());
    block.capacity(solver.model.size());

    for (int i = 0; i < solver.model.size(); ++i) {
        if (solver.model[i] == l_True)
            suspected.push_(TRUE),
            block.push_(mkLit(i, true));
        else if (solver.model[i] == l_False)
            suspected.push_(FALSE),
            block.push_(mkLit(i, false));
        else
            suspected.push_(UNSUS);
    }

    solver.addClause(block);

    while (solver.solve()) {
        block.clear();

        for (int i = 0; i < solver.model.size(); ++i) {
            if (suspected[i] == TRUE && solver.model[i] == l_True)
                block.push_(mkLit(i, true));
            else if (suspected[i] == FALSE && solver.model[i] == l_False)
                block.push_(mkLit(i, false));
            else
                suspected[i] = UNSUS;
        }

        if (solver.model.size() < suspected.size())
            suspected.shrink_(solver.model.size() - solver.model.size());

        solver.addClause(block);
    }

    std::cout << "backbones:" << std::endl;

    for (int i = 0; i < suspected.size(); ++i) {
        if (suspected[i] == TRUE)
            std::cout << '\t' << i << ": true" << std::endl;
        else if (suspected[i] == FALSE)
            std::cout << '\t' << i << ": false" << std::endl;
    }

    return 10;
}
