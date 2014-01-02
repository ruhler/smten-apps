
sudoku: sudoku.hs BitCell.hs Board.hs Cell.hs EnumCell.hs IntegerCell.hs
	smten --make -o sudoku sudoku.hs

sudoku_prof: sudoku.hs BitCell.hs Board.hs Cell.hs EnumCell.hs IntegerCell.hs
	smten --make -o sudoku_prof sudoku.hs -prof

perf: sudoku
	time -f %E ./sudoku -s yices2 -e Bit 10 < boards/sudoku17.unsolved > /dev/null
	time -f %E ./sudoku -s yices2 -e Enum 100 < boards/sudoku17.unsolved > /dev/null
	time -f %E ./sudoku -s yices2 -e Integer 2 < boards/sudoku17.unsolved > /dev/null

clean: 
	rm -rf *.o *.hi *.smten_o sudoku sudoku_prof Smten
