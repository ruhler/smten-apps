
sources := \
	src/IntNQueens.hs \
   	src/IntegerNQueens.hs \
	src/BitNQueens.hs \
	src/BoolNQueens.hs \
	src/Bool2NQueens.hs

sourcesl := \
	src/IntNQueensL.hs

all: build/release/nqueens \
	build/profile/nqueens \
	build/z3nqueens \
	build/z3txtnqueens \
	build/release/nqueensl \
	build/release/nqueensl2 \
	build/msnqueens \
	build/ms2nqueens \
	build/ms3nqueens \
	build/ms4nqueens \
	build/ms5nqueens \
	build/ms6nqueens \
	build/ms7nqueens \
	build/y2nqueens

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
	smten --make -prof -o build/profile/nqueens -ibuild/profile -isrc -hidir build/profile -odir build/profile src/nqueens.hs -rtsopts -fwarn-unused-imports -fprof-auto-top

build/z3nqueens: src/z3nqueens.c
	mkdir -p build
	gcc -std=c99 -o $@ $< -lz3

build/msnqueens: src/msnqueens.cc
	mkdir -p build
	g++ -o $@ -D __STDC_LIMIT_MACROS -D __STDC_FORMAT_MACROS $< -lminisat

build/ms2nqueens: src/ms2nqueens.cc src/minisat.cc
	mkdir -p build
	g++ -o $@ -D __STDC_LIMIT_MACROS -D __STDC_FORMAT_MACROS $^ -lminisat

build/ms3nqueens: src/ms3nqueens.hs
	ghc -o $@ --make -O $< -isrc -ibuild -hidir build -odir build

build/ms4nqueens: src/ms4nqueens.hs
	ghc -o $@ --make -O $< -isrc -ibuild -hidir build -odir build
	
build/ms5nqueens: src/ms5nqueens.hs
	ghc -o $@ --make -O $< -isrc -ibuild -hidir build -odir build

build/ms6nqueens: src/ms6nqueens.hs
	ghc -o $@ --make -O $< -isrc -ibuild -hidir build -odir build

build/ms7nqueens: src/ms7nqueens.hs
	ghc -o $@ --make -O $< -isrc -ibuild -hidir build -odir build
	
build/y2nqueens: src/y2nqueens.cc
	mkdir -p build
	g++ -o $@ $< -I/home/ruhler/sri/scratch/yices2/yices-2.1.0/include -L/home/ruhler/sri/scratch/yices2/yices-2.1.0/lib -lyices

build/z3txtnqueens: src/z3txtnqueens.c
	mkdir -p build
	gcc -std=c99 -o $@ $<

perf: build/release/nqueens
	tclsh utils/perf.tcl | tee build/perf.txt

graphs: build/bench.data
	Rscript utils/bench.R

build/bench.data: build/release/nqueens
	tclsh utils/bench.tcl | tee $@

clean:
	rm -rf build

