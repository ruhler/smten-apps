
.PHONY: all
all: 
	tclsh utils/make.tcl
	tclsh utils/test.tcl "./build/release/ssketch -s yices2"

bench:
	tclsh utils/bench.tcl ./build/release/ssketch 5 benchmarks/tests/*.sk benchmarks/perf/*.sk | tee build/bench.log

perf:
	tclsh ./utils/perf.tcl | tee build/perf.txt

check:
	tclsh utils/test.tcl "./build/release/ssketch -s yices2"

build/sskVsk.pdf: build/sskVsk.data
	Rscript utils/sskVsk.R

build/sskVsk.data: utils/sskVsk.tcl
	tclsh utils/sskVsk.tcl | tee $@

clean:
	rm -rf build
