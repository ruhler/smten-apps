
sources := \
	src/IntNQueens.hs \
   	src/IntegerNQueens.hs \
	src/BitNQueens.hs \
	src/BoolNQueens.hs

sourcesl := \
	src/IntNQueensL.hs

all: build/release/nqueens \
	build/profile/nqueens \
	build/z3nqueens \
	build/z3txtnqueens \
	build/release/nqueensl \
	build/release/nqueensl2 \
	build/msnqueens

build/release/nqueens: src/nqueens.hs $(sources)
	mkdir -p build/release
	smten --make -O -o build/release/nqueens -ibuild/release -isrc -hidir build/release -odir build/release src/nqueens.hs -rtsopts -fwarn-unused-imports

build/release/nqueensl: src/nqueensl.hs $(sourcesl)
	mkdir -p build/release
	ghc --make -O -o build/release/nqueensl -ibuild/release -isrc -hidir build/release -odir build/release src/nqueensl.hs -rtsopts -fwarn-unused-imports

build/release/nqueensl2: src/nqueens.hs $(sources)
	mkdir -p build/release
	ghc --make -O -o build/release/nqueensl2 -ibuild/release -isrc -hidir build/release -odir build/release src/nqueens.hs -rtsopts -fwarn-unused-imports -XNoImplicitPrelude

build/profile/nqueens: src/nqueens.hs $(sources)
	mkdir -p build/profile
	smten --make -prof -o build/profile/nqueens -ibuild/profile -isrc -hidir build/profile -odir build/profile src/nqueens.hs -rtsopts -fwarn-unused-imports

build/z3nqueens: src/z3nqueens.c
	mkdir -p build
	gcc -std=c99 -o $@ $< -lz3

build/msnqueens: src/msnqueens.cc
	mkdir -p build
	g++ -o $@ -D __STDC_LIMIT_MACROS -D __STDC_FORMAT_MACROS $< -lminisat

build/z3txtnqueens: src/z3txtnqueens.c
	mkdir -p build
	gcc -std=c99 -o $@ $<

perf: build/release/nqueens
	timeout 30s ./build/release/nqueens -s yices2 -e Int -500 | wc -l
	timeout 30s ./build/release/nqueens -s yices2 -e Integer -500 | wc -l
	timeout 30s ./build/release/nqueens -s yices2 -e Bit -500 | wc -l
	timeout 30s ./build/release/nqueens -s yices2 -e Bool -500 | wc -l
	timeout 30s ./build/release/nqueens -s yices1 -e Int -500 | wc -l
	timeout 30s ./build/release/nqueens -s yices1 -e Integer -500 | wc -l
	timeout 30s ./build/release/nqueens -s yices1 -e Bit -500 | wc -l
	timeout 30s ./build/release/nqueens -s yices1 -e Bool -500 | wc -l
	timeout 30s ./build/release/nqueens -s z3 -e Int -500 | wc -l
	timeout 30s ./build/release/nqueens -s z3 -e Integer -500 | wc -l
	timeout 30s ./build/release/nqueens -s z3 -e Bit -500 | wc -l
	timeout 30s ./build/release/nqueens -s z3 -e Bool -500 | wc -l
	timeout 30s ./build/release/nqueens -s stp -e Int -500 | wc -l
	timeout 30s ./build/release/nqueens -s stp -e Bit -500 | wc -l
	timeout 30s ./build/release/nqueens -s stp -e Bool -500 | wc -l
	timeout 30s ./build/release/nqueens -s minisat -e Int -500 | wc -l
	timeout 30s ./build/release/nqueens -s minisat -e Bit -500 | wc -l
	timeout 30s ./build/release/nqueens -s minisat -e Bool -500 | wc -l

graphs: build/bench.data
	Rscript utils/bench.R

build/bench.data: build/release/nqueens
	tclsh utils/bench.tcl | tee $@

clean:
	rm -rf build

