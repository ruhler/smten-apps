
set ::hampi [lindex $argv 0]
set ::runspertest 10
set ::timeout 30

set i 0
foreach x [glob "tests/*.hmp" "tests/hampi/*.hmp" "tests/slow/*.hmp"] {
    puts -nonewline "$i $x: "
    set mintime [expr $::timeout * 1000000]
    for {set ti 0} {$ti < $runspertest} {incr ti} { 
        set thistime [lindex [time {catch {exec $::hampi $x $::timeout }}] 0]
        if {$thistime < $mintime} {
            set mintime $thistime
        }
        puts -nonewline "$thistime "
    }
    puts "$mintime"
    incr i
}
    
