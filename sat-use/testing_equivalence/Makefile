CPP_IMPL_DIR:=../../CPP

.PHONY: all

all: main

$(CPP_IMPL_DIR)/bin/dimacs.o:
	make -C "$(CPP_IMPL_DIR)" bin/dimacs.o

%: %.cpp $(CPP_IMPL_DIR)/bin/dimacs.o
	g++ -O2 -std=c++20 -Wall -march=native -s -o $@ $^ -lz -lminisat
