
all: nqueens nqueens_prof

nqueens: nqueens.hs
	smten --make -O -o nqueens nqueens.hs -rtsopts

nqueens_prof: nqueens.hs
	smten --make -prof -o nqueens_prof nqueens.hs -rtsopts

clean:
	rm -rf nqueens *.hi *.smten_hi *.o nqueens_prof nqueens.smten_o Smten

