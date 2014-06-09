
# All tests from utils/tsat.txt should pass
set fin [open "utils/tsat.txt" "r"]
set tests [split [string trim [read $fin]]]
close $fin
foreach x $tests {
    puts "$::env(HWMCC10)/$x ..."
    exec ./utils/runsat "$::env(HWMCC10)/$x"
}

# All tests from utils/tunsat.txt should be unsat
set fin [open "utils/tunsat.txt" "r"]
set tests [split [string trim [read $fin]]]
close $fin
foreach x $tests {
    puts "$::env(HWMCC10)/$x ..."
    exec ./utils/rununsat "$::env(HWMCC10)/$x"
}

