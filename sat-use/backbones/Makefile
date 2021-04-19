CXXOPTS := -std=c++20 -Wall -Wextra -pedantic -DNDEBUG -O0 -g -march=native
LIBS := minisat z
BINDIR := bin
BIN := main

.PHONY: all

all: $(BINDIR)/$(BIN)

$(BINDIR)/$(BIN): main.cpp
	@$(MAKE) $(BINDIR) > /dev/null
	$(CXX) $(CXXOPTS) $^ -o $@ $(patsubst %,-l%,$(LIBS))



$(BINDIR):
	mkdir $(BINDIR)