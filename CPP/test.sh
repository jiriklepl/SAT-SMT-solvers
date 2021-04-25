#!/bin/sh

mkdir -p out
printf "" > out/watched.csv
printf "" > out/adjacency.csv

for file in `find data -maxdepth 1 -mindepth 1`; do
    setup=`echo $file | sed 's/[^0-9]*\([0-9]\+\)-\([0-9]\+\).*/\1,\2/' | tr "\n" ','`
    printf $setup >> out/watched.csv
    printf $setup >> out/adjacency.csv
    ./bin/main $file | ./mine.awk >> out/watched.csv
    ./bin/main $file -a | ./mine.awk >> out/adjacency.csv
done
