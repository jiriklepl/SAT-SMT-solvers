# CDCL

The implementation can read a file in SATLIB or DIMACS format. It can deduce the format from the extension of the file name. The format can be enforced by using either `-c, --dimacs` option, or `-s, --satlib` options, this also allows the program to read from the standard input.

## Modes

- `ADJACENCY`: uses the DPLL algorithm with adjacency lists.
- `WATCHED`: uses the DPLL algorithm with watched literals (and stores clause literals in a contiguous manner).
- `WATCHED_SEP`: uses the DPLL algorithm with watched literals (and stores clauses separately).
- `CDCL_WATCHED`: uses the CDCL algorithm with watched literals and restarts after an amount of contradictions given by the luby sequence multiplied by the *unit run* variable.
- `CDCL_WATCHED_SEP`: uses the CDCL algorithm with watched literals, clauses stored separately, restarts after an amount of contradictions given by the luby sequence multiplied by the *unit run* variable, and removing a half of the learned clauses if the cache, initially set to the *cache limit* variable (the cache limit doubles after each removal wave), is full.

## Variables

- **unit run**: the multiplier used for computing the length of runs given by the luby sequence. The default value is `100`.
- **cache limit**: the initial size of cache for learned clauses. The default value is `10'000`.

## Build

The following command builds the release version of the CDCL program (`bin/main`):

```sh
make release
```

The following command will build the debug version of the CDCL program (`debug/main`) with extra assertions in the code ensuring the program works as intended and satisfies all invariants:

```sh
make debug
```

The following command builds the `formula2cnf` program:

```sh
make formula2cnf
```

## Run

The following command runs the CDCL program (the release version; change to `debug` or `profile` for the respective version of the program)

```sh
bin/main
```

To print out the help for the CDCL program:

```sh
bin/main --help
```

## Measuring

```sh
# prepare data
mkdir -p data
cd data
cat ../links.txt | while read link; do wget "$link"; done
for file in ./*.tar.gz; do tar -xzf $file; done
cd ..

# measure
./test.sh

# check validity
grep -qrE "^uf,[^,]*,[^,]*,0," out || echo "success" # satisfiable not identified as unsatisfiable
grep -qrE "^uuf,[^,]*,[^,]*,1," out || echo "success" # unsatisfiable not identified as satisfiable

# the following two scripts use only unsatisfiable instances with 50 variables and only one configuration (the one with the best performance), this is due to valgrind memcheck taking up too much time. These scripts where very important in the last steps of debugging (the first run of benchmarks showed multiple segmentation faults)

# check that the program does not violate any invariants
for i in data/UUF50.218.1000/*; do
  if (debug/main -u 100 -l 0 $i 2>&1 >/dev/null) | grep ".\+"; then
    echo $i
    break
  fi
done

# check that the program does no illegal accesses
for i in data/UUF50.218.1000/*; do
  if ! valgrind --tool=memcheck bin/main -u 100 -l 0 $i 2>&1 >/dev/null | grep -q ": 0 errors"; then
    echo $i
    break
  fi
done
```

## Aggregating the data

The results where collected using clang++-compiled program. g++, which was used in development, and other compilers should give similar results.

```sh
./script.R
```

## Results

The results for each set are listed in the following table.

The first column signifies whether the formulas in the given set where satisfiable (`uf`) or unsatisfiable (`uff`). The second one signifies the number of variables for each formula in the given set.

Sizes of sets are 1000 for the sets with 50 or 100 variables, and 100 for the sets with 125 or 150 variables.

The name of each set is: `out/cdcl-l<cache limit>-u<unit run>.csv` (angle brackets signify placeholders for variable, names of which are enclosed by the brackets).

```text
    type  variables name                          time decisions derivations
  1 uf           50 out/cdcl-l10000-u200.csv     0.703      262.       1352.
  2 uf           50 out/cdcl-l1000-u200.csv      0.704      262.       1352.
  3 uf           50 out/cdcl-l5000-u200.csv      0.705      262.       1352.
  4 uf           50 out/cdcl-l-1-u200.csv        0.706      262.       1352.
  5 uf           50 out/cdcl-l5000-u800.csv      0.707      261.       1357.
  6 uf           50 out/cdcl-l10000-u800.csv     0.707      261.       1357.
  7 uf           50 out/cdcl-l-1-u800.csv        0.708      261.       1357.
  8 uf           50 out/cdcl-l10000-u400.csv     0.710      261.       1361.
  9 uf           50 out/cdcl-l1000-u800.csv      0.711      261.       1357.
 10 uf           50 out/cdcl-l1000-u400.csv      0.712      261.       1361.
 11 uf           50 out/cdcl-l-1-u400.csv        0.713      261.       1361.
 12 uf           50 out/cdcl-l5000-u400.csv      0.715      261.       1361.
 13 uf           50 out/cdcl-l5000-u100.csv      0.745      275.       1414.
 14 uf           50 out/cdcl-l10000-u100.csv     0.747      275.       1414.
 15 uf           50 out/cdcl-l1000-u100.csv      0.749      275.       1414.
 16 uf           50 out/cdcl-l-1-u100.csv        0.752      275.       1414.
 17 uf           50 out/cdcl-l0-u100.csv         0.753      282.       1483.
 18 uf          100 out/cdcl-l0-u100.csv        39.7       4014.      53049.
 19 uf          100 out/cdcl-l1000-u200.csv     45.2       3023.      37503.
 20 uf          100 out/cdcl-l1000-u400.csv     46.3       3038.      38334.
 21 uf          100 out/cdcl-l1000-u100.csv     47.0       3250.      39853.
 22 uf          100 out/cdcl-l1000-u800.csv     48.2       3113.      39835.
 23 uf          100 out/cdcl-l5000-u200.csv     53.4       2995.      36927.
 24 uf          100 out/cdcl-l10000-u200.csv    54.7       2997.      36957.
 25 uf          100 out/cdcl-l-1-u200.csv       54.8       2997.      36960.
 26 uf          100 out/cdcl-l5000-u400.csv     55.3       3016.      37857.
 27 uf          100 out/cdcl-l5000-u100.csv     55.5       3195.      38939.
 28 uf          100 out/cdcl-l10000-u100.csv    57.9       3182.      38689.
 29 uf          100 out/cdcl-l-1-u100.csv       58.1       3185.      38733.
 30 uf          100 out/cdcl-l10000-u400.csv    59.9       3034.      38138.
 31 uf          100 out/cdcl-l-1-u400.csv       60.0       3034.      38142.
 32 uf          100 out/cdcl-l5000-u800.csv     60.8       3119.      39913.
 33 uf          100 out/cdcl-l10000-u800.csv    63.7       3134.      40171.
 34 uf          100 out/cdcl-l-1-u800.csv       65.1       3135.      40203.
 35 uf          125 out/cdcl-l0-u100.csv       454.       20036.     362736.
 36 uf          125 out/cdcl-l5000-u100.csv    659.       11737.     195447.
 37 uf          125 out/cdcl-l1000-u100.csv    680.       12684.     214167.
 38 uf          125 out/cdcl-l1000-u800.csv    764.       12629.     220402.
 39 uf          125 out/cdcl-l1000-u400.csv    776.       13462.     235391.
 40 uf          125 out/cdcl-l1000-u200.csv    844.       14045.     245717.
 41 uf          125 out/cdcl-l10000-u100.csv   870.       12218.     204399.
 42 uf          125 out/cdcl-l10000-u200.csv   870.       12724.     220023.
 43 uf          125 out/cdcl-l5000-u800.csv    901.       12986.     227364.
 44 uf          125 out/cdcl-l5000-u400.csv   1050.       14107.     248110.
 45 uf          125 out/cdcl-l-1-u200.csv     1087.       12248.     209979.
 46 uf          125 out/cdcl-l5000-u200.csv   1181.       13998.     244326.
 47 uf          125 out/cdcl-l10000-u800.csv  1240.       14015.     248559.
 48 uf          125 out/cdcl-l-1-u800.csv     1253.       12631.     220344.
 49 uf          125 out/cdcl-l-1-u100.csv     1307.       12673.     213826.
 50 uf          125 out/cdcl-l10000-u400.csv  1310.       14039.     246773.
 51 uf          125 out/cdcl-l-1-u400.csv     1371.       13128.     228576.
 52 uf          150 out/cdcl-l0-u100.csv      3780.       78421.    1654079.
 53 uf          150 out/cdcl-l5000-u100.csv  14338.       49005.    1010154.
 54 uf          150 out/cdcl-l1000-u400.csv  14670.       50057.    1058053.
 55 uf          150 out/cdcl-l1000-u100.csv  15567.       50108.    1034076.
 56 uf          150 out/cdcl-l10000-u200.csv 16148.       44925.     932584.
 57 uf          150 out/cdcl-l5000-u400.csv  17236.       48689.    1023059.
 58 uf          150 out/cdcl-l5000-u200.csv  18001.       47150.     981354.
 59 uf          150 out/cdcl-l5000-u800.csv  18318.       48402.    1022039.
 60 uf          150 out/cdcl-l10000-u400.csv 20934.       45056.     940456.
 61 uf          150 out/cdcl-l-1-u400.csv    21294.       42409.     880409.
 62 uf          150 out/cdcl-l1000-u800.csv  21885.       55756.    1187475.
 63 uf          150 out/cdcl-l10000-u100.csv 23078.       49742.    1027654.
 64 uf          150 out/cdcl-l-1-u100.csv    25238.       45776.     938726.
 65 uf          150 out/cdcl-l10000-u800.csv 27768.       49644.    1046432.
 66 uf          150 out/cdcl-l1000-u200.csv  28072.       55638.    1174795.
 67 uf          150 out/cdcl-l-1-u200.csv    45947.       54672.    1145532.
 68 uf          150 out/cdcl-l-1-u800.csv    56858.       52987.    1118816.
 69 uuf          50 out/cdcl-l0-u100.csv         1.69       486.       3146.
 70 uuf          50 out/cdcl-l1000-u100.csv      1.72       465.       2955.
 71 uuf          50 out/cdcl-l1000-u800.csv      1.72       456.       2969.
 72 uuf          50 out/cdcl-l1000-u200.csv      1.72       461.       2969.
 73 uuf          50 out/cdcl-l5000-u100.csv      1.73       465.       2955.
 74 uuf          50 out/cdcl-l1000-u400.csv      1.73       457.       2973.
 75 uuf          50 out/cdcl-l-1-u800.csv        1.74       456.       2969.
 76 uuf          50 out/cdcl-l5000-u400.csv      1.74       457.       2973.
 77 uuf          50 out/cdcl-l10000-u100.csv     1.74       465.       2955.
 78 uuf          50 out/cdcl-l10000-u400.csv     1.74       457.       2973.
 79 uuf          50 out/cdcl-l10000-u200.csv     1.74       461.       2969.
 80 uuf          50 out/cdcl-l10000-u800.csv     1.74       456.       2969.
 81 uuf          50 out/cdcl-l5000-u200.csv      1.75       461.       2969.
 82 uuf          50 out/cdcl-l5000-u800.csv      1.75       456.       2969.
 83 uuf          50 out/cdcl-l-1-u100.csv        1.75       465.       2955.
 84 uuf          50 out/cdcl-l-1-u200.csv        1.75       461.       2969.
 85 uuf          50 out/cdcl-l-1-u400.csv        1.76       457.       2973.
 86 uuf         100 out/cdcl-l1000-u800.csv    190.        7647.     111557.
 87 uuf         100 out/cdcl-l1000-u400.csv    191.        7711.     112229.
 88 uuf         100 out/cdcl-l1000-u100.csv    192.        7831.     111694.
 89 uuf         100 out/cdcl-l1000-u200.csv    195.        7732.     111621.
 90 uuf         100 out/cdcl-l0-u100.csv       212.       13801.     216349.
 91 uuf         100 out/cdcl-l5000-u100.csv    224.        7597.     107412.
 92 uuf         100 out/cdcl-l5000-u400.csv    230.        7539.     109067.
 93 uuf         100 out/cdcl-l5000-u200.csv    235.        7535.     107950.
 94 uuf         100 out/cdcl-l10000-u100.csv   241.        7531.     106167.
 95 uuf         100 out/cdcl-l5000-u800.csv    243.        7581.     110159.
 96 uuf         100 out/cdcl-l-1-u100.csv      246.        7527.     106078.
 97 uuf         100 out/cdcl-l10000-u200.csv   247.        7513.     107510.
 98 uuf         100 out/cdcl-l-1-u200.csv      252.        7499.     107265.
 99 uuf         100 out/cdcl-l10000-u400.csv   256.        7515.     108572.
100 uuf         100 out/cdcl-l-1-u400.csv      257.        7514.     108550.
101 uuf         100 out/cdcl-l10000-u800.csv   259.        7565.     109900.
102 uuf         100 out/cdcl-l-1-u800.csv      265.        7563.     109852.
```

We can clearly see that on all satisfiable instances the configuration with the *cache limit* being equal to 0 and *unit run* being equal to 100 outperformed all the other variants despite doing roughly double the amount of decisions and unit propagations. This configuration clears half of its cache after each restart.

This finding is very interesting and it probably means that doubling the cache limit after each restart (the current strategy) is not the best strategy. The time being better in the configuration discussed in the previous paragraph despite *doing more work* is probably due to keeping much fewer clauses.

On the unsatisfiable instances with 50 variables, all the configurations performed almost the same and the differences in their performance are almost indistinguishable from the effects of randomness.

The configurations performing the best on the unsatisfiable instances with 100 variables where the ones with the cache limit being equal to 1000. But the difference in performance is not really significant. The number of decisions and unit propagations for each configuration suggest that all the configurations, except the one discussed above, did roughly the same work, and performed very similarly to that configuration.

# Other programs

## Tseitin Encoding and DIMACS Format

The implementation for this assignment, `formula2cnf`, built by `Makefile`, takes a formula in the SATLIB format and outputs in the DIMACS format.

It recognized the option `-e` (or `--equiv`) if the user requires using equivalence in the Tseitin encoding.

The output is as requested with comments giving detailed description of the structure:

- the first line contains the source file of the formula
- then follows the list of variables
- and then the list of all gates
- the last comment contains the root of the structure

## DPLL Algorithm &  Watched Literals

The implementation can read a file in SATLIB or DIMACS format. It can deduce the format from the extension of the file name or the format can be specified by either `-c, --dimacs` option or `-s, --satlib` option.

By default, it will use the version of the algorithm with watched literals, this can be changed to the version with adjacency lists by using the `-a, --adjacency` option.

And it also supports the `-e, --equiv` option from the `formula2cnf` program.

## Comparison of watched literals and adjacency lists

The two version of the program were tested using [RND3SAT](https://www.cs.ubc.ca/~hoos/SATLIB/Benchmarks/SAT/RND3SAT/descr.html) uf test sets with 20, 50, and 100 variables (see [links.txt](links.txt)) by the `test.sh` script. And then analyzed by the  `script.R` R script.

The conclusion is that the 'watched literals' version offers almost 50% mean speedup while preserving the number of decisions and the number of derivation performed while finding the model.

Here is the relative mean comparison generated by the R script (watched literals speedup compared to adjacency lists):

```text
  variables     time decisions derivations
1        20 1.258361 1.0003466   1.0009109
2        50 1.560218 0.9664431   0.9666468
3       100 1.568032 0.9575588   0.9584289
```
