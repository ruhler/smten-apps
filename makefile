
.PHONY: all
all:
	tclsh utils/make.tcl

build/hVsh.pdf: build/hVsh.data
	Rscript utils/hVsh.R

build/hVsh.data: utils/hVsh.tcl build/shampi
	tclsh utils/hVsh.tcl | tee $@

clean:
	rm -r build/*

