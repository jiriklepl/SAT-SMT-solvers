# Decision procedures and SAT/SMT solvers

This repository contains solutions to various [NAIL094](http://ktiml.mff.cuni.cz/~kucerap/satsmt/index-en.php) homework assignments.

Please, do not copy :)

---

## Programming assignments - Build your own SAT solver and related tasks

**Prerequisites** (present on at least apt and pacman for their respective mainstream distributions)

- ghc
- cabal-install

### **Tseitin Encoding and DIMACS Format**

Source files: [Formula2CNF](Formula2CNF)

**Build & run** easily (from the root folder) by:

```sh
cabal run formula2cnf [--impl] [inputFile [outputFile]]
```

### **DPLL Algorithm**

Source files: [DPLL](DPLL)

**Build & run** easily (from the root folder) by:

```sh
cabal run dpll [--sat|--cnf] [inputFile [outputFile]]
```

If no format option is used, the program derives it from the extension of the file.

---

## Programming assignments - Using a SAT solver

- [**N Queens Puzzle**](sat-use/task_n_queens)
