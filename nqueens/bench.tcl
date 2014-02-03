
set ::runspertest 1

# Return the average time in seconds to run the given script.
proc mytime {script} {
    return [expr [lindex [time $script $::runspertest] 0] / 1.0e6]
}

# A configuration has a name, solver, and representation
proc cfg {name solver rep} {
    return [list $name $solver $rep]
}

proc cfgname {cfg} {
    return [lindex $cfg 0]
}

proc cfgsolver {cfg} {
    return [lindex $cfg 1]
}

proc cfgrep {cfg} {
    return [lindex $cfg 2]
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
    #    - it has taken longer than maxtimepertest to get an answer for some n
    #    - or there have been back to back errors
    set done [list]
    set errprev [list]
    set errthis [list]
    for {set n 1} {[llength $done] < [llength $configs]} {incr n} {
        foreach cfg $configs {
           if {[elem [cfgname $cfg] $done]} {
               puts -nonewline "NA "
           } else {
               if { [catch {
                       set t [mytime "exec ./nqueens -s [cfgsolver $cfg] -e [cfgrep $cfg] $n"]
                       puts -nonewline "$t "
                       if {$maxtimepertest < $t} {
                           lappend done [cfgname $cfg]
                       }}]} {
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

