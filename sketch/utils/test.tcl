
# All tests of the form tests/*.sk should pass
foreach x [glob tests/*.sk] {
    puts "$x ..."
    exec cat $x | ./build/release/sketch 
}

# All tests of the form tests/expectfail/*.sk should fail
foreach x [glob tests/expectfail/*.sk] {
    puts "$x ..."
    if {[catch "exec cat $x | ./build/release/sketch"] == 0} {
        error "expected failure, but completed"
    }
}

