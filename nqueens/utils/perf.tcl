
set ::runspertest 1

# Return the average time in seconds to run the given script.
proc mytime {script} {
    return [expr [lindex [time $script $::runspertest] 0] / 1.0e6]
}

proc runcfg {slv rep n} {
    puts -nonewline "$slv $rep $n: "
    flush stdout
    set t [mytime "exec ./build/release/nqueens -s $slv -e $rep $n"]
    puts "$t"
}

runcfg yices2 Int 59
runcfg yices2 Integer 31
runcfg yices2 Bit 90
runcfg yices2 Bool 150
runcfg yices2 Bool2 70
runcfg yices1 Int 30
runcfg yices1 Integer 42
runcfg yices1 Bit 65
runcfg yices1 Bool 90
runcfg yices1 Bool2 55
runcfg z3 Int 38
runcfg z3 Integer 36
runcfg z3 Bit 60
runcfg z3 Bool 80
runcfg z3 Bool2 60
runcfg stp Int 28
runcfg stp Bit 70
runcfg stp Bool 60
runcfg stp Bool2 35
runcfg minisat Int 35
runcfg minisat Bit 60
runcfg minisat Bool 150
runcfg minisat Bool2 70

