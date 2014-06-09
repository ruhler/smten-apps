
set ::runspertest 4

# Return the average time in seconds to run the given script.
proc mytime {script} {
    return [expr [lindex [time $script $::runspertest] 0] / 1.0e6]
}

proc runcfg {outfile maxn cmd} {
    set fout [open $outfile "w"]
    for {set n 1} {$n <= $maxn} {incr n} {
        puts "$cmd $n..."
        set t [mytime "exec $cmd $n"]
        puts $fout $t
        flush $fout
    }
    close $fout
}

runcfg build/ms.data 150 ./build/msnqueens
runcfg build/ms2.data 150 ./build/ms2nqueens
runcfg build/ms3.data 150 ./build/release/ms3nqueens
runcfg build/ms4.data 150 ./build/release/ms4nqueens
runcfg build/ms5.data 150 ./build/release/ms5nqueens
runcfg build/ms6.data 150 ./build/release/ms6nqueens
runcfg build/ms7.data 150 ./build/release/ms7nqueens
runcfg build/ms8.data 150 ./build/release/ms8nqueens
runcfg build/ms10.data 150 ./build/release/ms10nqueens
runcfg build/sms.data 150 "./build/release/nqueens -e Bool2 -s minisat"

