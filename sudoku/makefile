
SOURCES := src/sudoku.hs \
	src/BitCell.hs \
	src/Board.hs \
	src/Cell.hs \
	src/EnumCell.hs \
	src/IntegerCell.hs \
	src/IntCell.hs

all: build/release/sudoku build/profile/sudoku

build/release/sudoku: $(SOURCES)
	mkdir -p build/release
	smten --make -O -o build/release/sudoku -ibuild/release -isrc -hidir build/release -odir build/release src/sudoku.hs -rtsopts -fwarn-unused-imports

build/profile/sudoku: $(SOURCES)
	mkdir -p build/profile
	smten --make -prof -o build/profile/sudoku -ibuild/profile -isrc -hidir build/profile -odir build/profile src/sudoku.hs -rtsopts -fwarn-unused-imports

perf: build/release/sudoku
	time -f %E ./build/release/sudoku -s yices2 -e Bit 100 < tests/sudoku17.unsolved > /dev/null
	time -f %E ./build/release/sudoku -s yices2 -e Enum 100 < tests/sudoku17.unsolved > /dev/null
	time -f %E ./build/release/sudoku -s yices2 -e Integer 2 < tests/sudoku17.unsolved > /dev/null

graphs: build/bench.data
	Rscript utils/bench.R

build/bench.data: build/release/sudoku
	tclsh utils/bench.tcl | tee $@

clean: 
	rm -rf build
