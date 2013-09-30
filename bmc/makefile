
all: test
	./test

test: test.hs BMC.hs ShiftReg.hs
	smten -o test test.hs

clean:
	rm -rf *.hi *.smten_o Smten test test_prof
