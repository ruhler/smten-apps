
all: build/release/aiger build/profile/aiger

SOURCES := src/Aiger.hs src/AigerPCheck.hs src/PCheck.hs

check: build/release/aiger
	tclsh utils/test.tcl

build/release/aiger: src/aiger.hs  $(SOURCES)
	mkdir -p build/release
	smten --make -O -o $@ -ibuild/release -isrc -hidir build/release -odir build/release $< -rtsopts -fwarn-unused-imports
	
build/profile/aiger: src/aiger.hs  $(SOURCES)
	mkdir -p build/profile
	smten --make -prof -o $@ -ibuild/release -isrc -hidir build/release -odir build/release $< -rtsopts -fwarn-unused-imports

clean:
	rm -rf build
