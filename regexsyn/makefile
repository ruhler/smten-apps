
all: build/release/resyn

build/release/resyn: src/resyn.hs src/RegEx.hs
	mkdir -p build/release
	smten --make -o $@ -ibuild/release -isrc -hidir build/release -odir build/release $< -rtsopts -fwarn-unused-imports

clean:
	rm -rf build
