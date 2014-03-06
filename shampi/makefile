
.PHONY: all
all:
	tclsh utils/make.tcl

.PHONY: graphs
graphs: build/perf.data
	Rscript utils/perf.R

build/perf.data: utils/perf.tcl build/shampi
	tclsh utils/perf.tcl | tee $@

perf:
	time -f %E ./build/shampi -s stp -e Bit `cat utils/benchmarks.txt` > /dev/null
	time -f %E ./build/shampi -s yices2 -e Bit `cat utils/benchmarks.txt` > /dev/null
	time -f %E ./build/shampi -s yices2 -e Integer `cat utils/benchmarks.txt` > /dev/null
	time -f %E ./build/shampi -s yices1 -e Bit `cat utils/benchmarks.txt` > /dev/null
	time -f %E ./build/shampi -s yices1 -e Integer `cat utils/benchmarks.txt` > /dev/null
	time -f %E ./build/shampi -s z3 -e Bit `cat utils/benchmarks.txt` > /dev/null
	time -f %E ./build/shampi -s z3 -e Integer `cat utils/benchmarks.txt` > /dev/null
	time -f %E ./build/shampi -s minisat -e Bit `cat utils/benchmarks.txt` > /dev/null


clean:
	rm -r build/*

