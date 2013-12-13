
all: nqueens

sources := IntNQueens.hs IntegerNQueens.hs BitNQueens.hs BoolNQueens.hs

nqueens: nqueens.hs  $(sources)
	smten --make -O -o nqueens nqueens.hs -rtsopts

nqueens_prof: nqueens.hs $(sources)
	smten --make -prof -o nqueens_prof nqueens.hs -rtsopts -fprof-auto-top

blocktest: blocktest.hs Block.hs
	smten --make -o blocktest blocktest.hs
	./blocktest

boolnqtest: boolnqtest.hs BoolNQueens.hs
	smten --make -o boolnqtest boolnqtest.hs
	./boolnqtest



perf: nqueens
	time -f %E ./nqueens -s yices2 -e Int 24 > /dev/null
	time -f %E ./nqueens -s yices2 -e Integer 36 > /dev/null
	time -f %E ./nqueens -s yices2 -e Bit 128 > /dev/null

clean:
	rm -rf nqueens *.hi *.smten_hi *.o *.smten_o Smten

