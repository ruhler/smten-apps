
all: aiger

sources := src/Aiger.hs

aiger: src/aiger.hs  $(sources)
	smten --make -isrc -O -o $@ $< -rtsopts

