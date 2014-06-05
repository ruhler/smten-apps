
proc run {args} {
    puts $args
    exec {*}$args "2>@" stderr
}

proc hrun {args} {
    puts $args
    exec {*}$args "2>@" stderr ">@" stdout
}

# Run from the sketch/ directory.
hrun mkdir -p build/profile
#hrun happy src/Grammar.y -o build/profile/Grammar.hs
hrun smten --make -o build/profile/perf -prof -fprof-auto-top -ibuild/profile -isrc -hidir build/profile -odir build/profile src/perf.hs -fwarn-unused-imports -rtsopts

