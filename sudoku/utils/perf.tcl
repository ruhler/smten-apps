
set ::runspertest 1

# Return the average time in seconds to run the given script.
proc mytime {script} {
    return [expr [lindex [time $script $::runspertest] 0] / 1.0e6]
}

proc runcfg {slv rep n} {
    puts -nonewline "$slv $rep $n: "
    flush stdout
    set t [mytime "exec ./build/release/sudoku -s $slv -e $rep $n < tests/sudoku17.shuffled"]
    puts "$t"
}

runcfg yices2 Bit 400
runcfg yices2 Enum 500
runcfg yices2 BitOneHot 10
runcfg yices2 Int 300
runcfg yices2 Integer 2

runcfg yices1 Bit 125
runcfg yices1 Enum 225
runcfg yices1 BitOneHot 11
runcfg yices1 Int 140
runcfg yices1 Integer 90

runcfg z3 Bit 225
runcfg z3 Enum 200
runcfg z3 BitOneHot 22
runcfg z3 Int 150
runcfg z3 Integer 30

runcfg stp Bit 150
runcfg stp Enum 45
runcfg stp BitOneHot 21
runcfg stp Int 29

runcfg minisat Bit 72
runcfg minisat Enum 205
runcfg minisat BitOneHot 8
runcfg minisat Int 120

