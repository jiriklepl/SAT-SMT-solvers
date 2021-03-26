.PHONY: build pdf

build:
	@cabal build

pdf: Tseitin.pdf

%.pdf: %.tex
	pdflatex $<

Tseitin.tex: Main.lhs
	lhs2TeX -o $@ $<