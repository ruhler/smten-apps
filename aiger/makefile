
test: aiger
	./aiger < reference/aiger-1.9.4/examples/or.aag


sources := src/Aiger.hs

aiger: src/aiger.hs  $(sources)
	smten --make -isrc -O -o $@ $< -rtsopts

