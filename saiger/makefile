
all: build/release/saiger build/profile/saiger

SOURCES := src/Aiger.hs src/AigerPCheck.hs src/PCheck.hs

check: build/release/saiger
	tclsh utils/test.tcl

perf: build/release/saiger
	tclsh utils/perf.tcl | tee build/perf.txt

build/release/saiger: src/saiger.hs  $(SOURCES)
	mkdir -p build/release
	smten --make -O -o $@ -ibuild/release -isrc -hidir build/release -odir build/release $< -rtsopts -fwarn-unused-imports
	
build/profile/saiger: src/saiger.hs  $(SOURCES)
	mkdir -p build/profile
	smten --make -prof -o $@ -ibuild/release -isrc -hidir build/release -odir build/release $< -rtsopts -fwarn-unused-imports -fprof-auto-top

build/bench.data: utils/bench.tcl
	tclsh utils/bench.tcl | tee $@

build/bench.pdf: build/bench.data
	Rscript utils/bench.R

clean:
	echo "clean it your self"
