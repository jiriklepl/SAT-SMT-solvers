#!/bin/sh

if [ $# != 2 ]; then
    echo "Please provide two DIMACS files"
    exit 1
fi

[ -f "./main" ] || make main

./main short "$1" "$2"
result=$?
if [ $result -eq 20 ]; then
    ./main short "$2" "$1"
    result=$?
    if [ $result -eq 20 ]; then
        echo "Neither implies the other."
    elif [ $result -eq 10 ]; then
        echo "Right implies left."
    else
        echo "ERROR. (But left doesn't imply right)"
    fi
elif [ $result -eq 10 ]; then
    ./main short "$2" "$1"
    result=$?
    if [ $result -eq 20 ]; then
        echo "Left implies right."
    elif [ $result -eq 10 ]; then
        echo "Both imply the other."
    else
        echo "ERROR. (But left implies right)"
    fi
else
    echo "ERROR."
fi
