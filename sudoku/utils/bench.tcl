
set ::runspertest 1

# Return the average time in seconds to run the given script.
proc mytime {script} {
    return [expr [lindex [time $script $::runspertest] 0] / 1.0e6]
}

# Return the average time in seconds to run the solver on sudoku boards using
# the given representation.
proc runconfig {rep solver} {
    if {"$rep.$solver" == "Integer.stp" || "$rep.$solver" == "Integer.minisat"} {
        return "NA"
    }

    # Run the solver and representation on consecutive boards from the list
    # for about 60 seconds, then average the runtime.
    set fin [open "tests/sudoku17.unsolved" r]
    set t 0
    set n 0
    while {$t < 10} {
        incr n
        set me [mytime "exec ./build/release/sudoku -s $solver -e $rep 1 << [gets $fin]"]
        set t [expr $t + $me]
    }
    close $fin

    return [expr $t / $n]
}

set reps [list Integer Int Bit BitOneHot Enum]
set solvers [list yices2 stp z3 minisat yices1]

foreach r $reps {
    puts -nonewline " $r"
}
puts ""

foreach s $solvers {
    puts -nonewline $s
    foreach r $reps {
        puts -nonewline " [runconfig $r $s]"
        flush stdout
    }
    puts ""
}

