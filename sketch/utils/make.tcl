
proc run {args} {
    puts $args
    exec {*}$args "2>@" stderr
}

proc hrun {args} {
    puts $args
    exec {*}$args "2>@" stderr ">@" stdout
}

# Run from the sketch/ directory.
hrun mkdir -p build
hrun happy src/Grammar.y -o build/Grammar.hs
hrun smten --make -o build/sketch_prof -prof -fprof-auto-top -ibuild -isrc -hidir build/ -odir build/ src/sketch.hs
hrun smten --make -o build/sketch -O -ibuild -isrc -hidir build/ -odir build/ src/sketch.hs

