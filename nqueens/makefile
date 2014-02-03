
all: nqueens

sources := IntNQueens.hs IntegerNQueens.hs BitNQueens.hs BoolNQueens.hs

nqueens: nqueens.hs  $(sources)
	smten --make -O -o nqueens nqueens.hs -rtsopts

nqueens_prof: nqueens.hs $(sources)
	smten --make -prof -o nqueens_prof nqueens.hs -rtsopts -fprof-auto-top

nqueens_debug: nqueens.hs $(sources)
	smten --make -debug -o nqueens_debug nqueens.hs -rtsopts

blocktest: blocktest.hs Block.hs
	smten --make -o blocktest blocktest.hs
	./blocktest

boolnqtest: boolnqtest.hs BoolNQueens.hs
	smten --make -o boolnqtest boolnqtest.hs
	./boolnqtest

z3nqueens: z3nqueens.c
	gcc -std=c99 -o $@ $< -lz3

z3txtnqueens: z3txtnqueens.c
	gcc -std=c99 -o $@ $<

%.data: %.tcl
	tclsh $< | tee $@

%.pdf: %.R %.data
	Rscript $<

perf: nqueens
	time -f %E ./nqueens -s yices2 -e Int 32 > /dev/null
	time -f %E ./nqueens -s yices2 -e Integer 36 > /dev/null
	time -f %E ./nqueens -s yices2 -e Bit 128 > /dev/null

clean:
	rm -rf nqueens nqueens_debug nqueens_prof *.hi *.smten_hi *.o *.smten_o Smten

