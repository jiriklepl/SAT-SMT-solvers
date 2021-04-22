#!/bin/sh
SOLUTIONS="glucobones
lglbones
minibones
cadibones"
B_NUMS="10\n30"

make

[ -d "out" ] || mkdir "out"

for sol in `echo "$SOLUTIONS"`; do
    printf "" > "out/$sol.csv"
    for dir in `find data -maxdepth 1 -mindepth 1 -type d`; do
        for file in `ls "$dir/"`; do
            echo "$dir" | sed "s/.*m\([0-9]\+\).*b\([0-9]\+\).*/\1 \2/" | tr "\n" " "
            "./$sol" "$dir/$file" | grep -v "^c" | awk "
NR == 1 {
    time = \$1
}
NR == 2 {
    # nbones = \$1
    calls = \$4
}
# NR >= 3 {
#     if (\$2 == \"true\")
#         print substr(\$1, 1, length(\$1) - 1)
#     if (\$2 == \"true\")
#         print \"¬\" substr(\$1, 1, length(\$1) - 1)
# }
END {
    print time \" \" calls
}
"
        done >>  "out/$sol.csv"
    done
done
