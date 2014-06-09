
all: build/release/saiger build/profile/saiger

SOURCES := src/Aiger.hs src/AigerPCheck.hs src/PCheck.hs

check: build/release/saiger
	tclsh utils/test.tcl

build/release/saiger: src/saiger.hs  $(SOURCES)
	mkdir -p build/release
	smten --make -O -o $@ -ibuild/release -isrc -hidir build/release -odir build/release $< -rtsopts -fwarn-unused-imports
	
build/profile/saiger: src/saiger.hs  $(SOURCES)
	mkdir -p build/profile
	smten --make -prof -o $@ -ibuild/release -isrc -hidir build/release -odir build/release $< -rtsopts -fwarn-unused-imports -fprof-auto-top

.PHONY: perf
perf:
	tclsh utils/perf.tcl `cat utils/sat.txt` | tee build/sat.data
	tclsh utils/perf.tcl `cat utils/unsat.txt` | tee build/unsat.data

.PHONY: perf_fast
perf_fast:
	tclsh utils/perf.tcl `cat utils/tsat.txt` | tee build/sat.data
	tclsh utils/perf.tcl `cat utils/tunsat.txt` | tee build/unsat.data

.PHONY: graph
graph: build/sat.data build/unsat.data
	Rscript utils/perf.R

clean:
	rm -r build
