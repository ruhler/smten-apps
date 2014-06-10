
# Gather data comparing Sketch with SSketch on the supported mini tests.
#  Uses SSketch with yices2 solver only.
#  Prints out individual test times (average)
set ::runspertest 1
set ::timeout 30
set ::tests $argv

# Return the average time in seconds to run the given script.
proc mytime {script} {
    set cmd "catch \{exec timeout ${::timeout}s $script\}"
    return [expr [lindex [time $cmd $::runspertest] 0] / 1.0e6]
}

puts "  sketch ssketch"
foreach x $::tests {
    puts -nonewline "$x "
    flush stdout
    set sketch [mytime "./utils/sketch $x"]
    set ssketch [mytime "./build/release/ssketch -s yices2 $x"]
    puts "$sketch $ssketch"
}

