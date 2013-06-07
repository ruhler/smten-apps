
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
run smten --include src --hsdir build -f src/shampi.smtn
hrun ghc -o build/shampi -O0 -hidir build/ -odir build/ -isrc -ibuild --make build/Smten/Lib/Main.hs -main-is Smten.Lib.Main.main__ -rtsopts -fno-warn-overlapping-patterns

hrun ghc -o build/shampi_prof -osuf o_prof -O0 -hidir build/ -odir build/ -isrc -ibuild --make build/Smten/Lib/Main.hs -main-is Smten.Lib.Main.main__ -rtsopts -prof -fno-warn-overlapping-patterns

run tclsh utils/runtests.tcl ./build/shampi > build/tests.shampi
hrun diff utils/tests.rhampi build/tests.shampi

