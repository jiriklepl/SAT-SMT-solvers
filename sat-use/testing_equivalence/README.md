# Testing Equivalence

For this assignment, I used **minisat**, which according to the previous assignments is the fastest of the tested SAT solvers.

The program `main` built by `make` takes three arguments: `short|long|long_eq FILE1 FILE2` and then determines whether the formula specified by the DIMACS file `FILE1` implies the formula specified by the DIMACS file `FILE2`.

The first argument determines which version of the algorithm is used: (lets call the first formula F and the second P)

- the `long` version transforms the negation of the second formula by the tseitin encoding using only top-down implications and then tries to satisfy `F /\ neg P`
- the `long_eq` version transforms the negation of the second formula by the tseitin encoding using equivalence for each clause (but skipping the root gate as that one would be immediately implied by unit propagation anyway) and then also tries to satisfy `F /\ neg P`
- the `short` makes a call for each clause `C` of the second formula using the negation of this clause as the set of assumptions and returns on the first satisfaction of `P /\ neg C`.

These three approaches are all equivalent (the `long_eq` was my terrible idea, as it is the slowest)

## Equivalence

You can check whether two formulas are equivalent, or at least whether one implies the other, by using the `equivalent.sh` wrapper script. It uses the `short` version as it is the fastest.

## Comparison

The program `main` was run on the first 20 formulas from each of the datasets in the [links.txt](links.txt) for `FILE1` and another one (not necessarily different) from these formulas for `FILE2` for each of the three version and then they grouped into 9 groups depending on the number of variables of each file (20, 50, 100) for each of the three versions. (60 * 60 * 3 = 10800 measurements in total)

Then the mean times of each group were divided by the reference version `short`. (All of this is in the files `test.bash` and `script.R`)

### **short**(reference) vs **long**:

```
  variables1 variables2      time
1         20         20 2.2279145
2         20         50 5.5673904
3         20        100 9.6004085
4         50         20 1.1704125
5         50         50 2.1184267
6         50        100 3.4359835
7        100         20 0.7847448
8        100         50 0.9734233
9        100        100 1.4101478
```

We can see that this version performs quite well if the second formula is significantly smaller than the first one (compared to the `short` version), and it performs extremely worse in the opposite situation.

In general, the `long` version performs worse than the `short` version.

Even though the `long` version can perform better for some instances; if we were interested in the implications in both ways, the slowdown in the other direction outweighs this improvement. This is why the `equivalent.sh` script uses the `short` version.

### **short**(reference) vs **long_eq**:

```
  variables1 variables2      time
1         20         20  3.582969
2         20         50  8.870889
3         20        100 15.510756
4         50         20  3.175353
5         50         50  5.747742
6         50        100  8.872894
7        100         20  1.197512
8        100         50  1.668156
9        100        100  2.480642
```

The `long_eq` version performed the worst of the three approaches.
