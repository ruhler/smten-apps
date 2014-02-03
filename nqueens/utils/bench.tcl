
set ::runspertest 1

# Return the average time in seconds to run the given script.
proc mytime {script} {
    return [expr [lindex [time $script $::runspertest] 0] / 1.0e6]
}

# A configuration has a solver, and representation
proc cfg {solver rep} {
    return [list $solver $rep]
}

proc cfgname {cfg} {
    return "[lindex $cfg 0].[lindex $cfg 1]"
}

proc cfgsolver {cfg} {
    return [lindex $cfg 0]
}

proc cfgrep {cfg} {
    return [lindex $cfg 1]
}

proc elem {x l} {
    set pos [lsearch $l $x]
    return [expr [lsearch $l $x] != -1]
}

proc gendata {maxtimepertest configs} {
    # Output the configuration names
    foreach cfg $configs {
        puts -nonewline "[cfgname $cfg] "
    }
    puts ""

    # Increase n until all configurations are done.
    # Done means either:
    #    - there have been back to back errors or timeouts
    set done [list]
    set errprev [list]
    set errthis [list]
    for {set n 1} {[llength $done] < [llength $configs]} {incr n} {
        foreach cfg $configs {
           if {[elem [cfgname $cfg] $done]} {
               puts -nonewline "NA "
           } else {
               if { [catch {
                       set t [mytime "exec timeout $maxtimepertest ./build/release/nqueens -s [cfgsolver $cfg] -e [cfgrep $cfg] $n"]
                       puts -nonewline "$t "
                       }]} {
                   # An error occurred
                   puts -nonewline "NA "
                   if {[elem [cfgname $cfg] $errprev]} {
                       lappend done [cfgname $cfg]
                   } else {
                       lappend errthis [cfgname $cfg]
                   }
               }
           }
       }
       puts ""
       set errprev $errthis
       set errthis [list]
    }

    # Output a blank line as indication that we finished.
    puts ""
}

set cfgs [list [cfg "minisat" "Int"] \
               [cfg "minisat" "Bit"] \
               [cfg "minisat" "Bool"] \
               [cfg "stp" "Int"] \
               [cfg "stp" "Bit"] \
               [cfg "stp" "Bool"] \
               [cfg "yices1" "Int"] \
               [cfg "yices1" "Integer"] \
               [cfg "yices1" "Bit"] \
               [cfg "yices1" "Bool"] \
               [cfg "yices2" "Int"] \
               [cfg "yices2" "Integer"] \
               [cfg "yices2" "Bit"] \
               [cfg "yices2" "Bool"] \
               [cfg "z3" "Int"] \
               [cfg "z3" "Integer"] \
               [cfg "z3" "Bit"] \
               [cfg "z3" "Bool"]]
gendata 1m $cfgs

