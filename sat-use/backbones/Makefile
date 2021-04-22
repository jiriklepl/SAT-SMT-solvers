CXXOPTS := -std=c++20 -Wall -Wextra -pedantic -DNDEBUG -g -O3 -march=native
MINILIBS := minisat z
GLUCOLIBS := glucose z
LGLLIBS := lgl
CADILIBS := cadical

.PHONY: all

all: minibones glucobones lglbones cadibones

minibones: minibones.cpp main.hpp
	$(CXX) $(CXXOPTS) $< -o $@ $(patsubst %,-l%,$(MINILIBS))

glucobones: glucobones.cpp main.hpp
	$(CXX) $(CXXOPTS) $< -o $@ $(patsubst %,-l%,$(GLUCOLIBS))

lglbones: lglbones.cpp main.hpp
	$(CXX) $(CXXOPTS) $< -o $@ $(patsubst %,-l%,$(LGLLIBS))

cadibones: cadibones.cpp main.hpp
	$(CXX) $(CXXOPTS) $< -o $@ $(patsubst %,-l%,$(CADILIBS))
