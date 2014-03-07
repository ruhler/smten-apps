
set ::runspertest 1

# Return the average time in seconds to run the given script.
proc mytime {script} {
    return [expr [lindex [time $script $::runspertest] 0] / 1.0e6]
}

puts "  saiger pdtrav"
foreach x [glob tests/sat/*.aig tests/unsat/*.aig] {
    set saiger [mytime "exec ./utils/run $x"]
    set pdtrav [mytime "exec ./utils/pdtrav $x"]
    puts "$x $saiger $pdtrav"
}

