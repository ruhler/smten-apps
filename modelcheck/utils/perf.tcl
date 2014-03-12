
set ::runspertest 1

# Return the average time in seconds to run the given script.
proc mytime {script} {
    return [expr [lindex [time $script $::runspertest] 0] / 1.0e6]
}

# All tests from utils/perf.txt are used
set fin [open "utils/perf.txt" "r"]
set ::tests [split [string trim [read $fin]]]
close $fin

proc runall {slv n} {
    foreach x [lrange $::tests 0 $n] {
        exec ./utils/aigtoaig -a $x | ./build/release/aiger -s $slv -a 2
    }
}

proc runcfg {slv n} {
    puts -nonewline "$slv $n: "
    flush stdout
    set t [mytime "runall $slv $n"]
    puts "$t"
}

runcfg yices2 10
runcfg yices1 5
runcfg z3 6
runcfg stp 4
runcfg minisat 10

