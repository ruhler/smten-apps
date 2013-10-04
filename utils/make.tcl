
proc run {args} {
    puts $args
    exec {*}$args "2>@" stderr
}

proc hrun {args} {
    puts $args
    exec {*}$args "2>@" stderr ">@" stdout
}

# Run from the shampi/ directory.
hrun mkdir -p build
hrun happy src/Grammar.y -o build/Grammar.hs
hrun smten --make -o build/shampi_prof -prof -fprof-auto-top -ibuild -isrc -hidir build/ -odir build/ src/shampi.hs
hrun smten --make -o build/shampi -O -ibuild -isrc -hidir build/ -odir build/ src/shampi.hs

run tclsh utils/runtests.tcl ./build/shampi > build/tests.shampi
hrun diff utils/tests.rhampi build/tests.shampi

