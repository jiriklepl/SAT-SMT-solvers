CXXOPTS := -std=c++20 -Wall -Wextra -pedantic -DNDEBUG -O3 -march=native
MINILIBS := minisat z
LGLLIBS := lgl
BIN := minibones lglbones

.PHONY: all

all: minibones lglbones

minibones: minibones.cpp
	$(CXX) $(CXXOPTS) $^ -o $@ $(patsubst %,-l%,$(MINILIBS))

lglbones: lglbones.cpp
	$(CXX) $(CXXOPTS) $^ -o $@ $(patsubst %,-l%,$(LGLLIBS))
