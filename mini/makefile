

check: build/bmc build/strsolver
	./build/bmc
	./build/strsolver

build/bmc: src/bmc.hs src/BMC/BMC.hs src/BMC/ShiftReg.hs
	smten --make -o $@ -ibuild -isrc -hidir build -odir build $< -rtsopts -fwarn-unused-imports

build/strsolver: src/strsolver.hs
	mkdir -p build
	smten --make -o $@ -ibuild -isrc -hidir build -odir build $< -rtsopts -fwarn-unused-imports

clean:
	rm -rf build

