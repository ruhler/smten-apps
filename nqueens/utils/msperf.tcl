
for {set n 1} {true} {incr n} {
    exec /usr/bin/time -f %E ./build/msnqueens $n "2>@" stderr
}

