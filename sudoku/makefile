
SOURCES := src/sudoku.hs \
	src/BitCell.hs \
	src/BitOneHotCell.hs \
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
	#cat tests/sudoku17.shuffled | timeout 30s ./build/release/sudoku -s yices2 -e Bit | wc -l
	#cat tests/sudoku17.shuffled | timeout 30s ./build/release/sudoku -s yices2 -e Enum | wc -l
	cat tests/sudoku17.shuffled | timeout 30s ./build/release/sudoku -s yices2 -e BitOneHot | wc -l
	#cat tests/sudoku17.shuffled | timeout 30s ./build/release/sudoku -s yices2 -e Int | wc -l
	#cat tests/sudoku17.shuffled | timeout 30s ./build/release/sudoku -s yices2 -e Integer | wc -l
	#cat tests/sudoku17.shuffled | timeout 30s ./build/release/sudoku -s yices1 -e Bit | wc -l
	#cat tests/sudoku17.shuffled | timeout 30s ./build/release/sudoku -s yices1 -e Enum | wc -l
	cat tests/sudoku17.shuffled | timeout 30s ./build/release/sudoku -s yices1 -e BitOneHot | wc -l
	#cat tests/sudoku17.shuffled | timeout 30s ./build/release/sudoku -s yices1 -e Int | wc -l
	#cat tests/sudoku17.shuffled | timeout 30s ./build/release/sudoku -s yices1 -e Integer | wc -l
	#cat tests/sudoku17.shuffled | timeout 30s ./build/release/sudoku -s z3 -e Bit | wc -l
	#cat tests/sudoku17.shuffled | timeout 30s ./build/release/sudoku -s z3 -e Enum | wc -l
	cat tests/sudoku17.shuffled | timeout 30s ./build/release/sudoku -s z3 -e BitOneHot | wc -l
	#cat tests/sudoku17.shuffled | timeout 30s ./build/release/sudoku -s z3 -e Int | wc -l
	#cat tests/sudoku17.shuffled | timeout 30s ./build/release/sudoku -s z3 -e Integer | wc -l
	#cat tests/sudoku17.shuffled | timeout 30s ./build/release/sudoku -s stp -e Bit | wc -l
	#cat tests/sudoku17.shuffled | timeout 30s ./build/release/sudoku -s stp -e Enum | wc -l
	cat tests/sudoku17.shuffled | timeout 30s ./build/release/sudoku -s stp -e BitOneHot | wc -l
	#cat tests/sudoku17.shuffled | timeout 30s ./build/release/sudoku -s stp -e Int | wc -l
	#cat tests/sudoku17.shuffled | timeout 30s ./build/release/sudoku -s minisat -e Bit | wc -l
	#cat tests/sudoku17.shuffled | timeout 30s ./build/release/sudoku -s minisat -e Enum | wc -l
	cat tests/sudoku17.shuffled | timeout 30s ./build/release/sudoku -s minisat -e BitOneHot | wc -l
	#cat tests/sudoku17.shuffled | timeout 30s ./build/release/sudoku -s minisat -e Int | wc -l

graphs: build/bench.data
	Rscript utils/bench.R

build/bench.data: build/release/sudoku
	tclsh utils/bench.tcl | tee $@

clean: 
	rm -rf build
