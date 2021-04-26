.PHONY: run

run: out/queens100.cnf out
	./run.sh

out:
	mkdir out

out/queens100.cnf: out gen_queens
	./gen_queens 100

%: %.cpp
	g++ -O2 -std=c++20 -Wall -march=native -s -o $@ $<
