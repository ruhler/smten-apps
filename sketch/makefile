
.PHONY: all
all: 
	tclsh utils/make.tcl

check:
	tclsh utils/test.tcl

# Make the test case for the S2QBF and Isolate0
test: test.hs S2QBF.hs Isolate0.hs
	smten --make -o test test.hs

clean:
	rm -rf *.hi *.smten_o Smten test test_prof build/*
