
set ::SKETCH ./build/release/sketch
#set ::SKETCH sketch

# All tests of the form tests/*.sk should pass
foreach x [glob tests/*.sk] {
    puts "$x ..."
    exec $::SKETCH $x
}

# All tests of the form tests/expectfail/*.sk should fail
foreach x [glob tests/expectfail/*.sk] {
    puts "$x ..."
    if {[catch "exec $::SKETCH $x"] == 0} {
        error "expected failure, but completed"
    }
}

