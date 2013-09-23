
foreach x [glob tests/*.sk] {
    exec cat $x | ./build/sketch 
}


