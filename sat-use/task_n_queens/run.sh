#!/bin/sh
. ../set_path.sh
measure() {
    prinf "" > $1.out.txt
    for file in $(ls out/ | sed "s/\([^0-9]*\([0-9]\+\).*\)/\2 \1/" | sort -n | cut -d' ' -f2); do
        echo "measuring $1 on $file"
        start=`date +%s%N`
        $1 "out/$file" > /dev/null
        result=$?
        end=`date +%s%N`
        echo `expr $end - $start` >> $1.out.txt
    done
}

measure minisat
measure glucose
measure lingeling