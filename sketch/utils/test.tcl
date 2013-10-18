
foreach x [glob tests/*.sk] {
    puts $x...
    exec cat $x | ./build/release/sketch 
}


