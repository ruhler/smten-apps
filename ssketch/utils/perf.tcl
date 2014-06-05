
# Return the average time in seconds to run the given script.
proc mytime {script} {
    return [expr [lindex [time $script] 0] / 1.0e6]
}

proc runsketch {bench} {
    puts -nonewline "sketch.$bench: "
    flush stdout
    set t [mytime "exec ./utils/sketch $bench"]
    puts "$t"
}

proc runssketch {slv bench} {
    puts -nonewline "ssketch.$slv.$bench: "
    flush stdout
    set t [mytime "exec ./build/release/ssketch -s $slv $bench"]
    puts "$t"
}

proc runcfgs {bench args} {
    puts "$bench: "

    # Run the original sketch:
    puts -nonewline "  sketch: "
    flush stdout
    set t [mytime "exec ./utils/sketch $bench"]
    puts "$t"

    # Run each config:
    foreach slv $args {
        puts -nonewline " ssketch $slv: "
        flush stdout
        set t [mytime "exec ./build/release/ssketch -s $slv $bench"]
        puts "$t"
    }
    puts ""
}
        
#runcfgs benchmarks/gallery/compress.sk yices2 stp z3
#runcfgs benchmarks/gallery/jburnim_morton.sk yices2 yices1 stp z3
#runcfgs benchmarks/gallery/logcount.sk yices2 yices1 stp z3
#runcfgs benchmarks/gallery/parity.sk yices2 yices1 stp z3 minisat
#runcfgs benchmarks/gallery/polynomial.sk yices2 yices1 stp z3 minisat
#runcfgs benchmarks/gallery/tutorial1.sk yices2 yices1 stp z3 minisat
#runcfgs benchmarks/gallery/tutorial2.sk yices2 yices1 stp z3 minisat
#runcfgs benchmarks/gallery/registerlesSwap.sk yices2 yices1 stp z3 minisat
#runcfgs benchmarks/gallery/tableBasedAddition.sk yices2 yices1 stp z3 minisat
#runcfgs benchmarks/gallery/log2.sk yices2 yices1 stp z3 minisat
#runcfgs benchmarks/gallery/isolateRightmost.sk yices2 yices1 stp z3 minisat
#runcfgs benchmarks/gallery/reverse.sk yices2 yices1 stp z3 minisat
#runcfgs benchmarks/gallery/xpose.sk yices1 stp z3 minisat
runcfgs benchmarks/gallery/Pollard.sk yices2 yices1 stp z3 minisat
    
