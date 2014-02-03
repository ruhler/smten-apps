
.PHONY: all
all:
	tclsh utils/make.tcl

.PHONY: graphs
graphs: build/perf.data
	Rscript utils/perf.R

build/perf.data: utils/perf.tcl build/shampi
	tclsh utils/perf.tcl | tee $@

clean:
	rm -r build/*

