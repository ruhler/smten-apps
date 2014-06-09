
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
hrun smten --make -o build/profile/shampi -prof -ibuild/profile -isrc -hidir build/profile -odir build/profile src/shampi.hs -fwarn-unused-imports -fwarn-unused-binds -rtsopts
hrun smten --make -o build/release/shampi -O -ibuild/release -isrc -hidir build/release -odir build/release src/shampi.hs -fwarn-unused-imports -fwarn-unused-binds -rtsopts

# Test using the different char types.
run tclsh utils/test.tcl ./build/release/shampi -e Bit > build/tests.shampi.bit
hrun diff utils/tests.rhampi build/tests.shampi.bit

run tclsh utils/test.tcl ./build/release/shampi -e Integer > build/tests.shampi.integer
hrun diff utils/tests.rhampi build/tests.shampi.integer

run tclsh utils/test.tcl ./build/release/shampi -e Char > build/tests.shampi.char
hrun diff utils/tests.rhampi build/tests.shampi.char

run tclsh utils/test.tcl ./build/release/shampi -e Int > build/tests.shampi.int
hrun diff utils/tests.rhampi build/tests.shampi.int

