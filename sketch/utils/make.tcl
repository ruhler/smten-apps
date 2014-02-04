
proc run {args} {
    puts $args
    exec {*}$args "2>@" stderr
}

proc hrun {args} {
    puts $args
    exec {*}$args "2>@" stderr ">@" stdout
}

# Run from the sketch/ directory.
hrun mkdir -p build/release build/profile

hrun happy src/Grammar.y -o build/profile/Grammar.hs
hrun smten --make -o build/profile/sketch -prof -fprof-auto-top -ibuild/profile -isrc -hidir build/profile -odir build/profile src/sketch.hs -fwarn-unused-imports -rtsopts

hrun happy src/Grammar.y -o build/release/Grammar.hs
hrun smten --make -o build/release/sketch -ibuild/release -isrc -hidir build/release -odir build/release src/sketch.hs -fwarn-unused-imports -rtsopts

