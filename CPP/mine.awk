#!/usr/bin/awk -f

NR == 2 { printf ("%f,", $2) }
NR == 3 { printf ("%d,", $2) }
NR == 4 { printf ("%d\n", $2); exit }
