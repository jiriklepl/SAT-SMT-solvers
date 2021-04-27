# Decision procedures and SAT/SMT solvers

This repository contains solutions to various [NAIL094](http://ktiml.mff.cuni.cz/~kucerap/satsmt/index-en.php) homework assignments.

Please, do not copy :)

---

## Programming assignments - Build your own SAT solver and related tasks

**Prerequisites** (present on at least apt and pacman for their respective mainstream distributions)

- ghc
- cabal-install

All those prerequisites are present on the school lab computers.

### **Tseitin Encoding and DIMACS Format**

- source files: [Formula2CNF](Formula2CNF)
- useful test-files: [test](test)

**Build & run** easily (from the root folder) by:

```sh
cabal run formula2cnf -- [--impl] [inputFile [outputFile]]
```

`cabal run -v0 formula2cnf -- [--impl] [inputFile [outputFile]]` allows the program to be run in a pipeline if testing the following assignment

### **DPLL Algorithm**

- Source files: [DPLL](DPLL)
- Useful test-files: [test](test)

**Build & run** easily (from the root folder) by:

```sh
cabal run dpll -- [--sat|--cnf] [inputFile [outputFile]]
```


If no format option is used, the program derives it from the extension of the file.

---

## Programming assignments - Using a SAT solver

- [**N Queens Puzzle**](sat-use/task_n_queens)
- [**Backbones**](sat-use/task_n_queens)
- [**Testing Equivalence**](sat-use/task_n_queens)
