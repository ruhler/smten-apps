
.PHONY: all
all:
	tclsh utils/make.tcl

.PHONY: graphs
graphs: build/perf.data
	Rscript utils/perf.R

build/perf.data: utils/perf.tcl build/release/shampi
	tclsh utils/perf.tcl | tee $@

perf:
	tclsh utils/perffast.tcl `cat utils/benchmarks.txt` | tee build/perf.txt


clean:
	rm -r build/*

