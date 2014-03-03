
.PHONY: all
all: 
	tclsh utils/make.tcl
	time -f %E tclsh utils/test.tcl "./build/release/sketch -s yices2"

perfer:
	tclsh utils/makeperf.tcl
	./build/profile/perf +RTS -p

perf:
	time -f %E tclsh utils/test.tcl "./build/release/sketch -s yices2" > /dev/null
	time -f %E tclsh utils/test.tcl "./build/release/sketch -s yices1" > /dev/null
	time -f %E tclsh utils/test.tcl "./build/release/sketch -s z3" > /dev/null
	time -f %E tclsh utils/test.tcl "./build/release/sketch -s stp" > /dev/null
	time -f %E tclsh utils/test.tcl "./build/release/sketch -s minisat" > /dev/null

check:
	tclsh utils/test.tcl "./build/release/sketch -s yices2"

build/sskVsk.pdf: build/sskVsk.data
	Rscript utils/sskVsk.R

build/sskVsk.data: utils/sskVsk.tcl
	tclsh utils/sskVsk.tcl | tee $@

clean:
	rm -rf *.hi *.smten_o *.smten_hi Smten test test_prof build/*
