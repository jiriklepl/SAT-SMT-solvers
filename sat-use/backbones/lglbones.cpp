#include <iostream>
#include <cstddef>
#include <cassert>
#include <cstring>
#include <vector>
#include <stdarg.h>

extern "C"{

#include <lgl/lglib.h>
#include <lgl/lgldimacs.h>

}


int main(int argc, char *argv[])
{
    bool closeinputfile;
    const char *inputname = argc == 2 ? argv[1] : nullptr;
    int lineno, nvars;
    FILE *inputfile;
    LGL *lgl = lglinit();


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

    for (int i = 1; i <= nvars; ++i)
        lglfreeze(lgl, i);


    if (lglsat(lgl) != 10) {
        std::cout << "no backbones" << std::endl;

        return 20;
    }

    enum suspicion : std::uint8_t {
        TRUE,
        FALSE,
        UNSUS
    };

    std::vector<suspicion> suspected;

    suspected.resize(nvars + 1);

    for (int i = 1; i <= nvars; ++i) {
        auto var = lglderef(lgl, i);
        if (var > 0)
            suspected[i] = TRUE;
        else if (var < 0)
            suspected[i] = FALSE;
        else
            suspected[i] = UNSUS;
    }

    for (int i = 1; i <= nvars; ++i) {
        if (suspected[i] == TRUE)
            lgladd(lgl, -i);
        else if (suspected[i] == FALSE)
            lgladd(lgl, i);
    }

    lgladd(lgl, 0);

    while (lglsat(lgl) == 10) {

        for (int i = 1; i <= nvars; ++i) {
            auto var = lglderef(lgl, i);
            if (suspected[i] == TRUE && var > 0);
            else if (suspected[i] == FALSE && var < 0);
            else if (suspected[i] != UNSUS)
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

    std::cout << "backbones:" << std::endl;

    for (std::size_t i = 1; i < suspected.size(); ++i) {
        if (suspected[i] == TRUE)
            std::cout << '\t' << i << ": true" << std::endl;
        else if (suspected[i] == FALSE)
            std::cout << '\t' << i << ": false" << std::endl;
    }

    return 10;
}
