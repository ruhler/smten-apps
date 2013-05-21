
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
run smten --haskellf \
        --include src \
        --hsdir build \
        -f src/SHampi.smtn
hrun ghc -o build/shampi -O2 -hidir build/ -odir build/ -isrc -ibuild --make src/hampi.hs -rtsopts

# -fprof-auto-top
hrun ghc -o build/shampi_prof -hidir build/ -odir build/ -isrc -ibuild -prof -rtsopts -osuf o_prof src/hampi.hs -fprof-auto-top

run tclsh utils/runtests.tcl ./build/shampi > build/tests.shampi
hrun diff utils/tests.rhampi build/tests.shampi
