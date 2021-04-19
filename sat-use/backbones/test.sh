#!/bin/sh
B_NUMS="10\n30"
DIR="data/cbs_b"

make

for num in `echo "$B_NUMS"`; do
    for file in `ls "$DIR$num"`; do
        lines=$(expr `bin/main "$DIR$num/$file" | wc -l` - 1)
        echo $lines
    done
done
