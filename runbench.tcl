
set ::hampi [lindex $argv 0]

set i 0
foreach x [glob "tests/*.hmp" "tests/hampi/*.hmp"] {
    puts -nonewline "$i $x: "
    puts [lindex [time {catch {exec $::hampi $x 30 }}] 0]
    incr i
}
    
