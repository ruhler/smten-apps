
# Gather data comparing Sketch with SSketch.
#  Uses SSketch with yices2 solver only.
#  Prints out individual test times (average)
set ::runspertest 10

# Return the average time in seconds to run the given script.
proc mytime {script} {
    return [expr [lindex [time $script $::runspertest] 0] / 1.0e6]
}

puts "  sketch ssketch"
foreach x [glob tests/*.sk] {
    set sketch [mytime "exec ./utils/sketch $x"]
    set ssketch [mytime "exec ./build/release/sketch -s yices2 $x"]
    puts "$x $sketch $ssketch"
}

