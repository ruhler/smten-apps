
set ::SKETCH [lindex $argv 0]

# All tests of the form benchmarks/unit/*.sk should pass
foreach x [glob benchmarks/unit/*.sk] {
    puts "$x ..."
    exec {*}$::SKETCH $x
}

# All tests of the form benchmarks/unit/expectfail/*.sk should fail
foreach x [glob benchmarks/unit/expectfail/*.sk] {
    puts "$x ..."
    if {[catch "exec {*}$::SKETCH $x"] == 0} {
        error "expected failure, but completed"
    }
}

