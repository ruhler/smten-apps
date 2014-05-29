
# bench.tcl sketch timeout test1.sk test2.sk ...
set ::SKETCH [lindex $argv 0]
set ::TIMEOUT [lindex $argv 1]
set ::TESTS [lrange $argv 2 end]

# Run sketch on each test case with the given timeout.
# Report for each test:
#     PASS/FAIL, Runtime in seconds

foreach x $TESTS {
    puts -nonewline "$x ... "
    flush stdout
    set result FAIL
    set script "catch \{exec timeout ${TIMEOUT}s $SKETCH $x\n set result PASS\}"
    set t [expr [lindex [time $script] 0] / 1.0e6]
    puts "$result $t"
}

