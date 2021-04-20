#!/bin/sh
SOLUTIONS="minibones\nlglbones"
B_NUMS="10\n30"
DIR="data/cbs_b"

make

for sol in `echo "$SOLUTIONS"`; do
    for num in `echo "$B_NUMS"`; do
        for file in `ls "$DIR$num"`; do
            lines=$(expr `./$sol "$DIR$num/$file" | wc -l` - 1)
            echo $lines
        done
    done
done
