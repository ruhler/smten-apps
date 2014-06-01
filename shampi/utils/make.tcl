
proc run {args} {
    puts $args
    exec {*}$args "2>@" stderr
}

proc hrun {args} {
    puts $args
    exec {*}$args "2>@" stderr ">@" stdout
}

# Run from the shampi/ directory.
hrun mkdir -p build/profile build/release
hrun happy src/Grammar.y -o build/profile/Grammar.hs
hrun happy src/Grammar.y -o build/release/Grammar.hs
hrun smten --make -o build/profile/shampi -prof -ibuild/profile -isrc -hidir build/profile -odir build/profile src/shampi.hs -fwarn-unused-imports -fwarn-unused-binds
hrun smten --make -o build/release/shampi -O -ibuild/release -isrc -hidir build/release -odir build/release src/shampi.hs -fwarn-unused-imports -fwarn-unused-binds

run tclsh utils/runtests.tcl ./build/release/shampi > build/tests.shampi
hrun diff utils/tests.rhampi build/tests.shampi

