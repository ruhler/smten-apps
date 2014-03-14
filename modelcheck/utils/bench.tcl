
set ::cmd [lindex $argv 0]
set ::tests [lrange $argv 1 end]

set ::runspertest 1
set ::timeout 900

# Return the average time in seconds to run the given script.
proc mytime {script} {
    return [expr [lindex [time $script $::runspertest] 0] / 1.0e6]
}

proc withtimeout {args} {
    catch "exec timeout ${::timeout}s $args"
}

puts "$::cmd"
foreach x $::tests {
    set t [mytime "withtimeout $::cmd $x"]
    puts "$x $t"
}

