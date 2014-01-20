
all: aiger

sources := src/Aiger.hs src/AigerPCheck.hs src/PCheck.hs

aiger: src/aiger.hs  $(sources)
	smten --make -isrc -O -o $@ $< -rtsopts

clean:
	rm aiger src/*.hi src/*.smten_o
