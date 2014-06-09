
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

puts "  PdTrav Saiger"
foreach x $::tests {
    set xfull "$::env(HWMCC10)/$x"
    puts -nonewline "$xfull "
    flush stdout

    set pdtrav [mytime "withtimeout ./utils/pdtrav $xfull"]
    set saiger [mytime "withtimeout ./utils/run $xfull"]
    puts "$pdtrav $saiger"
}

