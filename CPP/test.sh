#!/bin/sh

mkdir -p out

printf "" > out/adjacency.csv
printf "" > out/watched.csv
printf "" > out/cdcl-l0-u100.csv
printf "" > out/cdcl-l1000-u100.csv
printf "" > out/cdcl-l1000-u200.csv
printf "" > out/cdcl-l1000-u400.csv
printf "" > out/cdcl-l1000-u800.csv
printf "" > out/cdcl-l5000-u100.csv
printf "" > out/cdcl-l5000-u200.csv
printf "" > out/cdcl-l5000-u400.csv
printf "" > out/cdcl-l5000-u800.csv
printf "" > out/cdcl-l10000-u100.csv
printf "" > out/cdcl-l10000-u200.csv
printf "" > out/cdcl-l10000-u400.csv
printf "" > out/cdcl-l10000-u800.csv
printf "" > out/cdcl-l-1-u100.csv
printf "" > out/cdcl-l-1-u200.csv
printf "" > out/cdcl-l-1-u400.csv
printf "" > out/cdcl-l-1-u800.csv

for file in $(find data -name "*.cnf"); do
    setup=$(echo "$file" | sed 's/.*\/\(uf\|uuf\)\([0-9]\+\)-\([0-9]\+\).*/\1,\2,\3/' | tr "\n" ',')

    if echo "$file" | grep -q "uf50"; then
        printf '%s' "$setup" >> out/adjacency.csv
        printf '%s' "$setup" >> out/watched.csv
        ./bin/main -a "$file" | ./mine.awk >> out/adjacency.csv
        ./bin/main -w "$file" | ./mine.awk >> out/watched.csv
    fi

    printf '%s' "$setup" >> out/cdcl-l0-u100.csv
    printf '%s' "$setup" >> out/cdcl-l1000-u100.csv
    printf '%s' "$setup" >> out/cdcl-l1000-u200.csv
    printf '%s' "$setup" >> out/cdcl-l1000-u400.csv
    printf '%s' "$setup" >> out/cdcl-l1000-u800.csv
    printf '%s' "$setup" >> out/cdcl-l5000-u100.csv
    printf '%s' "$setup" >> out/cdcl-l5000-u200.csv
    printf '%s' "$setup" >> out/cdcl-l5000-u400.csv
    printf '%s' "$setup" >> out/cdcl-l5000-u800.csv
    printf '%s' "$setup" >> out/cdcl-l10000-u100.csv
    printf '%s' "$setup" >> out/cdcl-l10000-u200.csv
    printf '%s' "$setup" >> out/cdcl-l10000-u400.csv
    printf '%s' "$setup" >> out/cdcl-l10000-u800.csv
    printf '%s' "$setup" >> out/cdcl-l-1-u100.csv
    printf '%s' "$setup" >> out/cdcl-l-1-u200.csv
    printf '%s' "$setup" >> out/cdcl-l-1-u400.csv
    printf '%s' "$setup" >> out/cdcl-l-1-u800.csv

    ./bin/main -l 0 -u 100 "$file" | ./mine.awk >> out/cdcl-l0-u100.csv
    ./bin/main -l 1000 -u 100 "$file" | ./mine.awk >> out/cdcl-l1000-u100.csv
    ./bin/main -l 1000 -u 200 "$file" | ./mine.awk >> out/cdcl-l1000-u200.csv
    ./bin/main -l 1000 -u 400 "$file" | ./mine.awk >> out/cdcl-l1000-u400.csv
    ./bin/main -l 1000 -u 800 "$file" | ./mine.awk >> out/cdcl-l1000-u800.csv
    ./bin/main -l 5000 -u 100 "$file" | ./mine.awk >> out/cdcl-l5000-u100.csv
    ./bin/main -l 5000 -u 200 "$file" | ./mine.awk >> out/cdcl-l5000-u200.csv
    ./bin/main -l 5000 -u 400 "$file" | ./mine.awk >> out/cdcl-l5000-u400.csv
    ./bin/main -l 5000 -u 800 "$file" | ./mine.awk >> out/cdcl-l5000-u800.csv
    ./bin/main -l 10000 -u 100 "$file" | ./mine.awk >> out/cdcl-l10000-u100.csv
    ./bin/main -l 10000 -u 200 "$file" | ./mine.awk >> out/cdcl-l10000-u200.csv
    ./bin/main -l 10000 -u 400 "$file" | ./mine.awk >> out/cdcl-l10000-u400.csv
    ./bin/main -l 10000 -u 800 "$file" | ./mine.awk >> out/cdcl-l10000-u800.csv
    ./bin/main -l -1 -u 100 "$file" | ./mine.awk >> out/cdcl-l-1-u100.csv
    ./bin/main -l -1 -u 200 "$file" | ./mine.awk >> out/cdcl-l-1-u200.csv
    ./bin/main -l -1 -u 400 "$file" | ./mine.awk >> out/cdcl-l-1-u400.csv
    ./bin/main -l -1 -u 800 "$file" | ./mine.awk >> out/cdcl-l-1-u800.csv
done
