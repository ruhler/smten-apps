
SOURCES := sudoku.hs BitCell.hs Board.hs Cell.hs EnumCell.hs IntegerCell.hs IntCell.hs

sudoku: $(SOURCES)
	smten --make -o sudoku -O sudoku.hs

sudoku_prof: $(SOURCES)
	smten --make -o sudoku_prof sudoku.hs -prof

bench.data: sudoku bench.tcl
	tclsh bench.tcl | tee bench.data


perf: sudoku
	time -f %E ./sudoku -s yices2 -e Bit 100 < boards/sudoku17.unsolved > /dev/null
	time -f %E ./sudoku -s yices2 -e Enum 100 < boards/sudoku17.unsolved > /dev/null
	time -f %E ./sudoku -s yices2 -e Integer 2 < boards/sudoku17.unsolved > /dev/null

clean: 
	rm -rf *.o *.hi *.smten_o sudoku sudoku_prof Smten
