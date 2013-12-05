
all: nqueens

nqueens: nqueens.hs IntNQueens.hs IntegerNQueens.hs BitNQueens.hs
	smten --make -O -o nqueens nqueens.hs -rtsopts

nqueens_prof: nqueens.hs IntNQueens.hs IntegerNQueens.hs BitNQueens.hs
	smten --make -prof -o nqueens_prof nqueens.hs -rtsopts -fprof-auto-top

clean:
	rm -rf nqueens *.hi *.smten_hi *.o *.smten_o Smten

