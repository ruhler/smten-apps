
all: build/release/aiger build/profile/aiger

SOURCES := src/Aiger.hs src/AigerPCheck.hs src/PCheck.hs

build/release/aiger: src/aiger.hs  $(sources)
	mkdir -p build/release
	smten --make -O -o $@ -ibuild/release -isrc -hidir build/release -odir build/release $< -rtsopts -fwarn-unused-imports
	
build/profile/aiger: src/aiger.hs  $(sources)
	mkdir -p build/profile
	smten --make -prof -o $@ -ibuild/release -isrc -hidir build/release -odir build/release $< -rtsopts -fwarn-unused-imports

clean:
	rm -rf build
