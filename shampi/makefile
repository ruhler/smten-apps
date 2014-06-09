
.PHONY: all
all:
	tclsh utils/make.tcl

.PHONY: perf
perf: utils/perf.tcl build/release/shampi
	tclsh utils/perf.tcl -runs 8 | tee build/perf.data

.PHONY: perf_fast
perf_fast: utils/perf.tcl build/release/shampi
	tclsh utils/perf.tcl -runs 1 | tee build/perf.data

.PHONY: graph
graph: build/perf.data
	Rscript utils/perf.R

.PHONY: clean
clean:
	rm -r build

