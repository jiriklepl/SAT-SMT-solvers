# Report for the "Cryptarithms" assignment (updated to SMT-LIB 2)

The solution uses a problem translator which translates the formula into the smt-lib format.

The accepted grammar follows the C notation for logic formulas, but all variables are of the form (described by regex with placeholder WORD for words; spaces are optional):

`WORD (\+ WORD | - WORD)* = WORD`

Most notably, one can write `!! (send + more = money)` or even `!!send + more = money` which is equivalent to `send + more = money`.

> If you you are using a posix shell and you feed the translator via piping from `echo`, be aware that the `!!` pattern is a placeholder for the previously run command. In that case, consider delimitating the translated formula with single-quotes.

The translator accepts both lowercase characters and uppercase characters and it is case-sensitive.

## How to build the translator

```sh
# builds the formula->smtlib translator
c++ -std=c++20 -O2 -march=native -Wall -Wextra -pedantic main.cpp -o cryptarithms
```

You can use other c++ compilers if you want to try a particular one.

## Options

- `d` allows non-distinct values
- `b BASE` sets the base (sometimes referred to as *k*)

## Example

```sh
# translates the problem and solves it using yices-smt
echo SO+MANY+MORE+MEN+SEEM+TO+SAY+THAT+THEY+MAY+SOON+TRY+TO+STAY+AT+HOME+SO+AS+TO+SEE+OR+HEAR+THE+SAME+ONE+MAN+TRY+TO+MEET+THE+TEAM+ON+THE+MOON+AS+HE+HAS+AT+THE+OTHER+TEN=TESTS | ./cryptarithms | yices-smt2
```

alternatively:

```sh
# translates the problem and solves it using yices-smt
echo SO+MANY+MORE+MEN+SEEM+TO+SAY+THAT+THEY+MAY+SOON+TRY+TO+STAY+AT+HOME+SO+AS+TO+SEE+OR+HEAR+THE+SAME+ONE+MAN+TRY+TO+MEET+THE+TEAM+ON+THE+MOON+AS+HE+HAS+AT+THE+OTHER+TEN=TESTS | ./cryptarithms | z3 /dev/stdin
```

### Possible result

```text
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
echo "SEND + MORE = MONEY || SQUARE - DANCE = DANCER" | ./cryptarithms | yices-smt2
```

(or)

```sh
echo "SEND + MORE = MONEY || SQUARE - DANCE = DANCER" | ./cryptarithms | z3 /dev/stdin
```

We can allow repetitions with `-d` option:

```sh
echo "SEND + MORE = MONEY || SQUARE - DANCE = DANCER" | ./cryptarithms -d | yices-smt2
```

(or)

```sh
echo "SEND + MORE = MONEY || SQUARE - DANCE = DANCER" | ./cryptarithms -d | z3 /dev/stdin
```

This has the following yices solution

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
echo "SEND + MORE = MONEY || SQUARE - DANCE = DANCER" | ./cryptarithms -b 12 | yices-smt2
```

(or)

```sh
echo "SEND + MORE = MONEY || SQUARE - DANCE = DANCER" | ./cryptarithms -b 12 | z3 /dev/stdin
```

This has the following yices solution

```text
sat
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

```text
(set-logic QF_UFLIA)
(declare-const A Int)
(declare-const C Int)
(declare-const E Int)
(declare-const G Int)
(declare-const I Int)
(declare-const L Int)
(declare-const N Int)
(declare-const O Int)
(declare-const R Int)
(declare-const S Int)
(assert (>= A 0))
(assert (<= A 9))
(assert (>= C 1))
(assert (<= C 9))
(assert (>= E 1))
(assert (<= E 9))
(assert (>= G 0))
(assert (<= G 9))
(assert (>= I 0))
(assert (<= I 9))
(assert (>= L 1))
(assert (<= L 9))
(assert (>= N 0))
(assert (<= N 9))
(assert (>= O 0))
(assert (<= O 9))
(assert (>= R 0))
(assert (<= R 9))
(assert (>= S 1))
(assert (<= S 9))
(assert (distinct A C E G I L N O R S))
(assert (= (+ G (* 10 (+ N (* 10 (+ A (* 10 (+ L (* 10 (+ R (* 10 (+ E )))))))))))
(+
C
(+ C (* 10 (+ I (* 10 (+ G (* 10 (+ O (* 10 (+ L )))))))))
(+ A (* 10 (+ L (* 10 (+ A (* 10 (+ C (* 10 (+ S )))))))))
)
)
)
(check-sat)
(get-model)
```
