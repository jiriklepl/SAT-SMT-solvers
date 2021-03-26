.PHONY: build run pdf

build:
	cabal build

run:
	cabal run

pdf: Tseitin.pdf

%.pdf: %.tex
	pdflatex $<

Tseitin.tex: Main.lhs
	lhs2TeX -o $@ $<