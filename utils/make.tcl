
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
hrun ghc --make -c -ibuild -isrc -hidir build/ -odir build/ src/shampi.hs \
    -fplugin=Smten.Plugin.Plugin
hrun ghc -o build/shampi -O2 -hidir build/ -odir build/ -isrc -ibuild --make build/Smten/Compiled/Main.hs -main-is Smten.Compiled.Main.main -rtsopts
hrun ghc -o build/shampi_prof -osuf o_prof -O2 -hidir build/ -odir build/ -isrc -ibuild --make build/Smten/Compiled/Main.hs -main-is Smten.Compiled.Main.main -rtsopts -prof -fprof-auto-top

run tclsh utils/runtests.tcl ./build/shampi > build/tests.shampi
hrun diff utils/tests.rhampi build/tests.shampi

