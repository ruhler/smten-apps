
set ::runspertest 10
set ::timeout 60

proc mintime {script} {
    set mintime [expr $::timeout * 1000000]
    for {set ti 0} {$ti < $::runspertest} {incr ti} {
        set thistime [lindex [time $script] 0]
        if {$thistime < $mintime} {
            set mintime $thistime
        }
    }
    return "$mintime"
}

puts "test hampi yices2.integer yices2.bit yices1.integer yices1.bit stp"

exec ./utils/rhampi_s &
exec sleep 1
foreach x [glob "tests/easy/*.hmp" "tests/wsu/*.hmp" "tests/hard/*.hmp"] {
    set hampi [mintime "exec ./utils/rhampi_c $x"]
    set y2int [mintime "exec ./build/shampi -s yices2 -e Integer $x +RTS -K1g"]
    set y2bit [mintime "exec ./build/shampi -s yices2 -e Bit $x +RTS -K1g"]
    set y1int [mintime "exec ./build/shampi -s yices1 -e Integer $x +RTS -K1g"]
    set y1bit [mintime "exec ./build/shampi -s yices1 -e Bit $x +RTS -K1g"]
    set stp [mintime "exec ./build/shampi -s stp -e Bit $x +RTS -K1g"]
    puts "$x $hampi $y2int $y2bit $y1int $y1bit $stp"
}
exec ./utils/rhampi_shutdown
    
