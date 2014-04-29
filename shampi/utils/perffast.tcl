
set ::runspertest 1

# Return the average time in seconds to run the given script.
proc mytime {script} {
    return [expr [lindex [time $script $::runspertest] 0] / 1.0e6]
}

set ::tests $argv

proc runcfg {slv rep} {
    puts -nonewline "$slv $rep: "
    flush stdout
    set t [mytime "exec ./build/shampi -s $slv -e $rep $::tests"]
    puts "$t"
}

runcfg stp Bit
runcfg yices2 Bit
runcfg yices2 Integer
runcfg yices2 Char
runcfg yices1 Bit
runcfg yices1 Integer
runcfg yices1 Char
runcfg z3 Bit
runcfg z3 Integer
runcfg z3 Char
runcfg minisat Bit
runcfg minisat Char

