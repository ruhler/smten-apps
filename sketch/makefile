
.PHONY: all
all: 
	tclsh utils/make.tcl
	tclsh utils/test.tcl

perf:
	tclsh utils/makeperf.tcl
	./build/profile/perf +RTS -p

check:
	tclsh utils/test.tcl

build/sskVsk.pdf: build/sskVsk.data
	Rscript utils/sskVsk.R

build/sskVsk.data: utils/sskVsk.tcl
	tclsh utils/sskVsk.tcl | tee $@

clean:
	rm -rf *.hi *.smten_o *.smten_hi Smten test test_prof build/*
