#!/usr/bin/env bash

sed_command='s/[^0-9]*\([0-9]\+\)-\([0-9]\+\).*/\1,\2/'
files='find data -maxdepth 1 -mindepth 1'


mkdir -p out
make main

printf "" > out/short.csv
printf "" > out/long.csv
printf "" > out/long_eq.csv

for file1 in `$files`; do
    for file2 in `$files`; do
        echo $file1 | sed "$sed_command" | tr "\n" ',' >> out/short.csv
        echo $file1 | sed "$sed_command" | tr "\n" ',' >> out/long.csv
        echo $file1 | sed "$sed_command" | tr "\n" ',' >> out/long_eq.csv
        echo $file2 | sed "$sed_command" | tr "\n" ',' >> out/short.csv
        echo $file2 | sed "$sed_command" | tr "\n" ',' >> out/long.csv
        echo $file2 | sed "$sed_command" | tr "\n" ',' >> out/long_eq.csv
        ./main short   <(sed -n "/%/q;p" "$file1") <(sed -n "/%/q;p" "$file2") | awk 'NR == 1 {printf $2 ","} NR == 2 { print ($2 == "IMPLIED"); }' >> out/short.csv
        ./main long    <(sed -n "/%/q;p" "$file1") <(sed -n "/%/q;p" "$file2") | awk 'NR == 1 {printf $2 ","} NR == 2 { print ($2 == "IMPLIED"); }' >> out/long.csv
        ./main long_eq <(sed -n "/%/q;p" "$file1") <(sed -n "/%/q;p" "$file2") | awk 'NR == 1 {printf $2 ","} NR == 2 { print ($2 == "IMPLIED"); }' >> out/long_eq.csv
    done
done
