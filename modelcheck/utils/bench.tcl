
set ::runspertest 1
set ::timeout 900

# Return the average time in seconds to run the given script.
proc mytime {script} {
    return [expr [lindex [time $script $::runspertest] 0] / 1.0e6]
}

proc withtimeout {args} {
    catch "exec timeout ${::timeout}s $args"
}

puts "  saiger pdtrav"
foreach x [glob tests/sat/*.aig tests/unsat/*.aig tests/slow/sat/*.aig tests/slow/unsat/*.aig] {
    set saiger [mytime "withtimeout ./utils/run $x"]
    set pdtrav [mytime "withtimeout ./utils/pdtrav $x"]
    puts "$x $saiger $pdtrav"
}

