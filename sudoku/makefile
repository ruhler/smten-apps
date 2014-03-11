
SOURCES := src/sudoku.hs \
	src/BitCell.hs \
	src/BitOneHotCell.hs \
	src/Board.hs \
	src/Cell.hs \
	src/EnumCell.hs \
	src/IntegerCell.hs \
	src/IntCell.hs

all: build/release/sudoku build/profile/sudoku build/y2bit build/y2enum

build/release/sudoku: $(SOURCES)
	mkdir -p build/release
	smten --make -O -o build/release/sudoku -ibuild/release -isrc -hidir build/release -odir build/release src/sudoku.hs -rtsopts -fwarn-unused-imports

build/profile/sudoku: $(SOURCES)
	mkdir -p build/profile
	smten --make -prof -o build/profile/sudoku -ibuild/profile -isrc -hidir build/profile -odir build/profile src/sudoku.hs -rtsopts -fwarn-unused-imports

build/y2bit: src/y2bit.c
	mkdir -p build
	gcc -o $@ $< -std=c99 -I/home/ruhler/sri/scratch/yices2/yices-2.1.0/include -L/home/ruhler/sri/scratch/yices2/yices-2.1.0/lib -lyices

build/y2enum: src/y2enum.c
	mkdir -p build
	gcc -o $@ $< -std=c99 -I/home/ruhler/sri/scratch/yices2/yices-2.1.0/include -L/home/ruhler/sri/scratch/yices2/yices-2.1.0/lib -lyices



perf: build/release/sudoku
	tclsh utils/perf.tcl | tee build/perf.txt

graphs: build/bench.data
	Rscript utils/bench.R

build/bench.data: build/release/sudoku
	tclsh utils/bench.tcl | tee $@

clean: 
	rm -rf build
