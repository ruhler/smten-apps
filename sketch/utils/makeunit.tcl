
proc run {args} {
    puts $args
    exec {*}$args "2>@" stderr
}

proc hrun {args} {
    puts $args
    exec {*}$args "2>@" stderr ">@" stdout
}

# Run from the sketch/ directory.
hrun mkdir -p build/release
#hrun happy src/Grammar.y -o build/profile/Grammar.hs
hrun smten --make -o build/release/unit -ibuild/release -isrc -hidir build/release -odir build/release src/unit.hs -fwarn-unused-imports -rtsopts

