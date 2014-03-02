
set ::runspertest 1

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


# For the ICFP14 paper we have a graph with the following configurations:

# 1. haskell list monad based on Int type.
#    Ranges from n = 1 to n = 10 before blowing up too much.
#runcfg build/nqueensl.data 10 ./build/release/nqueensl

# 2. minisat coded in c.
#    Ranges from n = 1 to n = 200 easily enough.


# 3. Smten using the Int type.
#   Ranges from n = 1 to n = 57
#runcfg build/smtenmsint.data 57 "./build/release/nqueens -e Int -s minisat"

# 4. Smten using the Bit type.
#   Ranges from n = 1 to n = 100 easily enough
#runcfg build/smtenmsbit.data 100 "./build/release/nqueens -e Bit -s minisat"

# 5. Smten using the Bool type.
#   Ranges from n = 1 to n = 200 easily enough
#runcfg build/smtenmsbool.data 200 "./build/release/nqueens -e Bool -s minisat"

#runcfg build/cy2.data 150 ./build/y2nqueens
#runcfg build/smteny2bool2.data 160 "./build/release/nqueens -e Bool2 -s yices2"



#runcfg build/ms.data 150 ./build/msnqueens
#runcfg build/ms2.data 150 ./build/ms2nqueens
#runcfg build/ms3.data 150 ./build/ms3nqueens
#runcfg build/ms4.data 150 ./build/ms4nqueens
#runcfg build/ms5.data 150 ./build/ms5nqueens
#runcfg build/ms6.data 150 ./build/ms6nqueens
runcfg build/ms7.data 150 ./build/ms7nqueens
#runcfg build/sms.data 150 "./build/release/nqueens -e Bool2 -s minisat"

