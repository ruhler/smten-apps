
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

proc gendata {maxtimepertest configs} {
    # Output the configuration names
    foreach cfg $configs {
        puts -nonewline "[cfgname $cfg] "
    }
    puts ""

    # Increase n until all configurations are done.
    set done [list]
    for {set n 1} {[llength $done] < [llength $configs]} {incr n} {
        foreach cfg $configs {
           # yices1 Bit 55 hits a seg fault, so skip it.
           if {[cfgsolver $cfg] == "yices1" && [cfgrep $cfg] == "Bit" && $n == 55} {
               puts -nonewline "NA "
               continue
           }

           if {[lsearch $done [cfgname $cfg]] == -1} {
               set t [mytime "exec ./nqueens -s [cfgsolver $cfg] -e [cfgrep $cfg] $n"]
               puts -nonewline "$t "
               if {$maxtimepertest < $t} {
                   lappend done [cfgname $cfg]
               }
           } else {
               puts -nonewline "NA "
           }
       }
       puts ""
    }
}

