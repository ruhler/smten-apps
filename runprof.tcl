
puts "test AssertIn Check FixN Parse RunCmds SeriS SmtE Other"

foreach x [glob "tests/*.hmp" "tests/hampi/*.hmp" "tests/slow/*.hmp"] {
    set sccs [dict create AssertIn 0 Check 0 FixN 0 Parse 0 RunCmds 0 SeriS 0 SmtE 0]

    puts -nonewline "$x "
    exec ./shampi_prof $x +RTS -p "-V0.0001"
    #exec grep {^   [^ ]} shampi_prof.prof | awk {{ print $1 " " $7}} | sort > $x.prof
    
    set other 100
    set lines [split [read [open "shampi_prof.prof"]] "\n"]
    foreach l $lines {
        set scc [lindex $l 0]
        if {[dict exists $sccs $scc] && ([llength $l] > 5)} {
            set percent [lindex $l 6]
            set was [dict get $sccs $scc]
            dict set sccs $scc [expr $percent + $was]
            set other [expr $other - $percent]
        }
    }

    foreach {k v} $sccs {
        puts -nonewline "[format "%.2f" $v] "
    }
    puts [format "%.2f" $other]
}
    
