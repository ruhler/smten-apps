
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
hrun ghc \
 --make -c -ibuild -isrc -hidir build/ -odir build/ \
    src/sketch.hs -fplugin=Smten.Plugin.Plugin
hrun ghc -o build/sketch_prof -osuf o_prof -hidir build/ -odir build/ -isrc -ibuild --make build/Smten/Compiled/Main.hs -main-is Smten.Compiled.Main.main -rtsopts -prof -fprof-auto-top
hrun ghc -o build/sketch -O -hidir build/ -odir build/ -isrc -ibuild --make build/Smten/Compiled/Main.hs -main-is Smten.Compiled.Main.main -rtsopts

