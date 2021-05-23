# Report for the "Cryptarithms" assignment

The solution uses a problem translator which translates the formula into the smt-lib format.

The accepted grammar follows the C notation for logic formulas, but all variables are of the form (described by regex with placeholder WORD for words; spaces are optional):

`WORD (\+ WORD | - WORD)* = WORD`

Most notably, one can write `!! (send + more = money)` or even `!!send + more = money` which is equivalent to `send + more = money`.

> If you you are using a posix shell and you feed the translator via piping from `echo`, be aware that the `!!` pattern is a placeholder for the previously run command. In that case, consider delimitating the translated formula with single-quotes.

The translator accepts both lowercase characters and uppercase characters and it is case-sensitive.

## How to build the translator

```sh
# builds the fomula2smtlib translator
c++ -std=c++20 -O2 -march=native -Wall -Wextra -pedantic main.cpp -o cryptarithms
```

You can use other c++ compilers if you want to try a particular one.

## Options

- `d` allows non-distinct values
- `b BASE` sets the base (sometimes referred to as *k*)

## Example

```sh
# translates the problem and solves it using yices-smt
echo SO+MANY+MORE+MEN+SEEM+TO+SAY+THAT+THEY+MAY+SOON+TRY+TO+STAY+AT+HOME+SO+AS+TO+SEE+OR+HEAR+THE+SAME+ONE+MAN+TRY+TO+MEET+THE+TEAM+ON+THE+MOON+AS+HE+HAS+AT+THE+OTHER+TEN=TESTS | ./cryptarithms | yices-smt -m -s
```

### Possible result

```text
Core
 restarts                : 8
 simplify db             : 6
 reduce db               : 2
 decisions               : 2961
 random decisions        : 12
 propagations            : 21838
 conflicts               : 5057
 theory propagations     : 9564
 propagation-lemmas      : 293
 theory conflicts        : 2028
 conflict-lemmas         : 142
 lits in pb. clauses     : 47
 lits in learned clauses : 7276
 total lits. in learned  : 26035
 subsumed lits.          : 6213
 deleted pb. clauses     : 1
 deleted learned clauses : 1542
 deleted binary clauses  : 55
 boolean variables       : 147
 atoms                   : 147
Egraph
 eq from simplex         : 196
 prop. to core           : 109
 conflicts               : 149
 non-distinct lemmas     : 0
 auxiliary eqs. created  : 0
 dyn boolack. lemmas     : 0
 other dyn ack.lemmas    : 0
 final checks            : 167
 interface equalities    : 24
 egraph terms            : 36
 egraph eq_quota         : 100
Simplex
 init. variables         : 11
 init. rows              : 1
 init. atoms             : 0
 end atoms               : 122
 elim. candidates        : 0
 elim. rows              : 0
 fixed vars after simpl. : 0
 rows after simpl.       : 1
 fixed vars              : 0
 rows in init. tableau   : 1
 rows in final tableau   : 25
 calls to make_feasible  : 7724
 pivots                  : 8169
 bland-rule activations  : 0
 simple lemmas           : 0
 prop. to core           : 9455
 derived bounds          : 5825
 productive propagations : 8707
 conflicts               : 1845
 interface lemmas        : 0
 reduced inter. lemmas   : 0
 trichotomy lemmas       : 24
 reduced tricho. lemmas  : 0
Integer arithmetic
 make integer feasible   : 155
 branch atoms            : 74
 Gomory cuts             : 0
bound strengthening
 conflicts               : 55
 recheck conflicts       : 26
integrality tests
 conflicts               : 34
 bound conflicts         : 21
 recheck conflicts       : 0
diohpantine solver
 gcd conflicts           : 0
 dioph checks            : 0
 dioph conflicts         : 0
 bound conflicts         : 0
 recheck conflicts       : 0

Search time             : 0.1139 s
Memory used             : 9.89 MB


sat

(= S 3)
(= A 7)
(= M 2)
(= O 1)
(= H 5)
(= Y 4)
(= N 6)
(= E 0)
(= R 8)
(= T 9)
```

The problem took approximately 1/10 s.

## Unsatisfiable example

```sh
echo "SEND + MORE = MONEY || SQUARE - DANCE = DANCER" | ./cryptarithms | yices-smt -m -s
```

This takes the solver approximately 2.5 s on the same computer as the example above.

We can allow repetitions with `-d` option:

```sh
echo "SEND + MORE = MONEY || SQUARE - DANCE = DANCER" | ./cryptarithms -d | yices-smt -m
```

This has the following solution

```text
(= Q 0)
(= A 1)
(= E 7)
(= N 8)
(= D 8)
(= S 9)
(= M 1)
(= Y 5)
(= C 9)
(= O 0)
(= R 8)
(= U 0)
```

Or we can solve the problem in the base 12 by `-b 12` option:

```sh
echo "SEND + MORE = MONEY || SQUARE - DANCE = DANCER" | ./cryptarithms -b 12 | yices-smt -m
```

This has the following solution

```text
(= Q 3)
(= A 8)
(= E 5)
(= N 6)
(= D 9)
(= S 11)
(= M 1)
(= Y 2)
(= C 4)
(= O 0)
(= R 10)
(= U 7)
```

## The translation

This is the translated version of `C+LOGIC+SCALA=ERLANG` (one of the problems on the site linked by the assignment).

```
(benchmark test.smt
:logic QF_UFLIA
:extrafuns ((A Int)(C Int)(E Int)(G Int)(I Int)(L Int)(N Int)(O Int)(R Int)(S Int))
:formula (and
(>= A 0)
(<= A 9)
(>= C 1)
(<= C 9)
(>= E 1)
(<= E 9)
(>= G 0)
(<= G 9)
(>= I 0)
(<= I 9)
(>= L 1)
(<= L 9)
(>= N 0)
(<= N 9)
(>= O 0)
(<= O 9)
(>= R 0)
(<= R 9)
(>= S 1)
(<= S 9)
(distinct A C E G I L N O R S)
(= (+ G (* 10 (+ N (* 10 (+ A (* 10 (+ L (* 10 (+ R (* 10 (+ E )))))))))))
(+
C
(+ C (* 10 (+ I (* 10 (+ G (* 10 (+ O (* 10 (+ L )))))))))
(+ A (* 10 (+ L (* 10 (+ A (* 10 (+ C (* 10 (+ S )))))))))
)
)
)
)
```
