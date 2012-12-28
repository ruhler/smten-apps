
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
run ../seri/build/seri-bin/seri --haskellf \
        --include ../seri/seri/sri \
        --include src \
        --no-main \
        --mod-name SeriGen \
        -f src/SeriGen.sri > build/SeriGen.hs
hrun ghc -o build/shampi -O2 -hidir build/ -odir build/ -isrc -ibuild --make src/hampi.hs

# -fprof-auto-top
hrun ghc -o build/shampi_prof -hidir build/ -odir build/ -isrc -ibuild -prof -rtsopts -osuf o_prof src/hampi.hs

run tclsh utils/runtests.tcl ./build/shampi > build/tests.shampi
hrun diff utils/tests.rhampi build/tests.shampi
