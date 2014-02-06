
sources := \
	src/IntNQueens.hs \
   	src/IntegerNQueens.hs \
	src/BitNQueens.hs \
	src/BoolNQueens.hs

all: build/release/nqueens build/profile/nqueens build/z3nqueens build/z3txtnqueens

build/release/nqueens: src/nqueens.hs $(sources)
	mkdir -p build/release
	smten --make -O -o build/release/nqueens -ibuild/release -isrc -hidir build/release -odir build/release src/nqueens.hs -rtsopts -fwarn-unused-imports

build/profile/nqueens: src/nqueens.hs $(sources)
	mkdir -p build/profile
	smten --make -prof -o build/profile/nqueens -ibuild/profile -isrc -hidir build/profile -odir build/profile src/nqueens.hs -rtsopts -fwarn-unused-imports

build/z3nqueens: src/z3nqueens.c
	mkdir -p build
	gcc -std=c99 -o $@ $< -lz3

build/z3txtnqueens: src/z3txtnqueens.c
	mkdir -p build
	gcc -std=c99 -o $@ $<

perf: build/release/nqueens
	time -f %E ./build/release/nqueens -s yices2 -e Int 32 > /dev/null
	time -f %E ./build/release/nqueens -s yices2 -e Integer 36 > /dev/null
	time -f %E ./build/release/nqueens -s yices2 -e Bit 128 > /dev/null

graphs: build/bench.data
	Rscript utils/bench.R

build/bench.data: build/release/nqueens
	tclsh utils/bench.tcl | tee $@

clean:
	rm -rf build

