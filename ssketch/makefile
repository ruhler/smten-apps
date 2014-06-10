
.PHONY: all
all: 
	tclsh utils/make.tcl
	tclsh utils/test.tcl "./build/release/ssketch -s yices2"

bench:
	tclsh utils/bench.tcl "./build/release/ssketch +RTS -K1g -RTS" 5 benchmarks/tests/*.sk benchmarks/perf/*.sk | tee build/bench.log

graph:
	Rscript utils/mini.R

mini:
	tclsh ./utils/mini.tcl `cat utils/supported.txt` | tee build/mini.data

mini_fast:
	tclsh ./utils/mini.tcl `head -n 100 utils/supported.txt` | tee build/mini.data

check:
	tclsh utils/test.tcl "./build/release/ssketch -s yices2"

clean:
	rm -rf build
