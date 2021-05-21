CXX := g++
LEXPP := flex++
CXXOPTS := -std=c++20 -Wall -Wextra -pedantic -DNDEBUG -O2 -march=native
CXXOPTS_PROFILE := -std=c++20 -Wall -Wextra -pedantic -DNDEBUG -O2 -g
CXXOPTS_DEBUG := -std=c++20 -Wall -Wextra -pedantic -DDEBUG -O0 -g
INCLUDE := include
BINDIR := bin
DEBUGDIR := debug
PROFILEDIR := profile
BIN := main
MKDIR_P := mkdir -p

OBJECT_FILES := main.o satlib.o dimacs.o

.PHONY: all debug clean release

all: release debug profile formula2cnf
release: $(BINDIR)/$(BIN)
debug: $(DEBUGDIR)/$(BIN)
profile: $(PROFILEDIR)/$(BIN)

$(BINDIR)/$(BIN): $(patsubst %,$(BINDIR)/%,$(OBJECT_FILES))
	$(CXX) $(CXXOPTS) -o $@ $(patsubst %,$(BINDIR)/%,$(OBJECT_FILES))

$(DEBUGDIR)/$(BIN): $(patsubst %,$(DEBUGDIR)/%,$(OBJECT_FILES))
	$(CXX) $(CXXOPTS_DEBUG) -o $@ $(patsubst %,$(DEBUGDIR)/%,$(OBJECT_FILES))

$(PROFILEDIR)/$(BIN): $(patsubst %,$(PROFILEDIR)/%,$(OBJECT_FILES))
	$(CXX) $(CXXOPTS_DEBUG) -o $@ $(patsubst %,$(PROFILEDIR)/%,$(OBJECT_FILES))

formula2cnf: $(BINDIR)/formula2cnf.o $(BINDIR)/satlib.o
	$(CXX) $(CXXOPTS) -o $@ $^

$(BINDIR)/%.o: %.cpp $(wildcard $(INCLUDE)/*)
	$(MKDIR_P) $(BINDIR)
	$(CXX) -c $(CXXOPTS) $(patsubst %,-I%,$(INCLUDE)) -o $@ $<

$(DEBUGDIR)/%.o: %.cpp $(wildcard $(INCLUDE)/*)
	$(MKDIR_P) $(DEBUGDIR)
	$(CXX) -c $(CXXOPTS_DEBUG) $(patsubst %,-I%,$(INCLUDE)) -o $@ $<

$(PROFILEDIR)/%.o: %.cpp $(wildcard $(INCLUDE)/*)
	$(MKDIR_P) $(PROFILEDIR)
	$(CXX) -c $(CXXOPTS_PROFILE) $(patsubst %,-I%,$(INCLUDE)) -o $@ $<

%.cpp: %.l
	$(LEXPP) -Cf -o $@ $<

clean:
	rm -rf $(BINDIR) $(DEBUGDIR) $(PROFILEDIR)
