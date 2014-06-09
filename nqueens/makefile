
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
	build/release/ms3nqueens \
	build/release/ms4nqueens \
	build/release/ms5nqueens \
	build/release/ms6nqueens \
	build/release/ms7nqueens \
	build/release/ms8nqueens \
	build/release/ms9nqueens \
	build/release/ms10nqueens \
	build/profile/ms3nqueens \
	build/profile/ms4nqueens \
	build/profile/ms5nqueens \
	build/profile/ms6nqueens \
	build/profile/ms7nqueens \
	build/profile/ms8nqueens \
	build/profile/ms9nqueens \
	build/profile/ms10nqueens \
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

build/release/ms%nqueens: src/ms%nqueens.hs
	mkdir -p build/release/ms$*nqueens_
	ghc -o $@ --make -O $< -isrc -ibuild/release/ms$*nqueens_ -hidir build/release/ms$*nqueens_ -odir build/release/ms$*nqueens_

build/profile/ms%nqueens: src/ms%nqueens.hs
	mkdir -p build/profile/ms$*nqueens_
	ghc -o $@ --make -prof -auto-all $< -isrc -ibuild/profile/ms$*nqueens_ -hidir build/profile/ms$*nqueens_ -odir build/profile/ms$*nqueens_
	
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

overheads:
	tclsh utils/overheads.tcl
	Rscript utils/overheads.R

clean:
	rm -rf build

