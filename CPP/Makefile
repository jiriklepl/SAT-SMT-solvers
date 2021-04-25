CXX := g++
LEXPP := flex++
CXXOPTS := -std=c++20 -Wall -Wextra -pedantic -DNDEBUG -O2 -s -march=native
CXXOPTS_DEBUG := -std=c++20 -Wall -Wextra -pedantic -DDEBUG -O0 -g
INCLUDE := include
BINDIR := bin/
DEBUGDIR := debug/
BIN := main

OBJECT_FILES := main.o satlib.o dimacs.o

.PHONY: all debug clean

all: $(BINDIR)$(BIN)
debug: $(DEBUGDIR)$(BIN)

$(BINDIR)$(BIN): $(patsubst %,$(BINDIR)%,$(OBJECT_FILES)) $(BINDIR)
	$(CXX) $(CXXOPTS) -o $@ $(patsubst %,$(BINDIR)%,$(OBJECT_FILES))

$(DEBUGDIR)$(BIN): $(patsubst %,$(DEBUGDIR)%,$(OBJECT_FILES))
	$(CXX) $(CXXOPTS_DEBUG) -o $@ $(patsubst %,$(DEBUGDIR)%,$(OBJECT_FILES))

$(BINDIR):
	mkdir $@

$(DEBUGDIR):
	mkdir $@

$(BINDIR)%.o: %.cpp $(wildcard $(INCLUDE)/*)
	@$(MAKE)  $(BINDIR) > /dev/null
	$(CXX) -c $(CXXOPTS) $(patsubst %,-I%,$(INCLUDE)) -o $@ $<

$(DEBUGDIR)%.o: %.cpp $(wildcard $(INCLUDE)/*)
	@$(MAKE)  $(DEBUGDIR) > /dev/null
	$(CXX) -c $(CXXOPTS_DEBUG) $(patsubst %,-I%,$(INCLUDE)) -o $@ $<

%.cpp: %.l
	$(LEXPP) -Cf -o $@ $<

clean:
	rm -r $(BINDIR) $(DEBUGDIR)
