
proc runconfig {rep solver} {
    return [exec -ignorestderr bash utils/runcfg.sh ./build/release/sudoku -s $solver -e $rep]
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

