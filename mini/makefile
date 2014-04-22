
build/strsolver: src/strsolver.hs
	mkdir -p build/release
	smten --make -o $@ -ibuild -isrc -hidir build -odir build $< -rtsopts -fwarn-unused-imports

clean:
	rm -rf build

