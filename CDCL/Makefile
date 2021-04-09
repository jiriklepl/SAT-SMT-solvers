CXX := g++
LEXPP := flex++
CXXOPTS := -std=c++20 -Wall -DNDEBUG -O2 -march=native
INCLUDE := include
BINDIR := bin
BIN := main

OBJECT_FILES := main.o satlib.o

.PHONY: all clean

all: $(BINDIR)/$(BIN)

$(BINDIR)/$(BIN): $(OBJECT_FILES) $(BINDIR)
	$(CXX) $(CXXOPTS) -o $@ $(OBJECT_FILES)

$(BINDIR):
	mkdir $@

%.o: %.cpp
	$(CXX) -c $(CXXOPTS) $(patsubst %,-I%,$(INCLUDE)) -o $@ $<

%.cpp: %.l
	$(LEXPP) -o $@ $<

clean:
	rm -r $(BINDIR)
	rm $(OBJECT_FILES)
