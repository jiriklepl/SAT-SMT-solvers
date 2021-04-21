#include <cassert>
#include <cstddef>
#include <cstring>
#include <iostream>
#include <vector>

extern "C" {

#include <stdarg.h>
#include <lingeling/lglib.h>
#include <lingeling/lgldimacs.h>

}

#include "main.hpp"

namespace {

LGL *lgl;
int nvars;

void init(int argc, char *argv[])
{
    bool closeinputfile;
    const char *inputname = argc == 2 ? argv[1] : nullptr;
    int lineno;
    FILE *inputfile;
    lgl = lglinit();


    if (inputname) {
        if (!(inputfile = fopen (inputname, "r")))
        std::cerr << "can not read '" << inputname << '\'' << std::endl;
        closeinputfile = true;
    } else inputname = "<stdin>", inputfile = stdin, closeinputfile = false;

    if (argc == 1)
        std::cout << "Reading from standard input... Use '--help' for help." << std::endl;

    lglparsefile(lgl, inputfile, 0, &lineno, &nvars);

    if (closeinputfile)
        fclose(inputfile);
}

int run()
{
    int called = 1;

    for (int i = 1; i <= nvars; ++i)
        lglfreeze(lgl, i);

    if (lglsat(lgl) != 10)
        return 0;

    suspected.resize(nvars + 1);

    for (int i = 1; i <= nvars; ++i) {
        auto var = lglderef(lgl, i);
        if (var > 0)
            suspected[i] = TRUE;
        else if (var < 0)
            suspected[i] = FALSE;
        else
            lglmelt(lgl, i),
            suspected[i] = UNSUS;
    }

    for (int i = 1; i <= nvars; ++i) {
        if (suspected[i] == TRUE)
            lgladd(lgl, -i);
        else if (suspected[i] == FALSE)
            lgladd(lgl, i);
    }

    lgladd(lgl, 0);

    while (++called, lglsat(lgl) == 10) {

        for (int i = 1; i <= nvars; ++i) {
            auto var = lglderef(lgl, i);

            if (suspected[i] != UNSUS
            && (suspected[i] == TRUE ? var <= 0 : var >= 0))
                lglmelt(lgl, i),
                suspected[i] = UNSUS;
        }

        for (int i = 1; i <= nvars; ++i) {
            if (suspected[i] == TRUE)
                lgladd(lgl, -i);
            else if (suspected[i] == FALSE)
                lgladd(lgl, i);
        }

        lgladd(lgl, 0);
    }

    return called;
}

}
