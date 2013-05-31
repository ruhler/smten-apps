
set ::runspertest 1

proc mintime {script} {
    set sumtime 0
    for {set ti 0} {$ti < $::runspertest} {incr ti} {
        set thistime [lindex [time $script] 0]
        set sumtime [expr $sumtime + $thistime]
    }
    return [expr $sumtime / $::runspertest]
}


exec ./utils/rhampi_s &
exec sleep 1

set hampi [mintime "exec tclsh ./utils/rhampi.tcl $argv"]
set y2int [mintime "exec ./build/shampi -s yices2 -e Integer $argv +RTS -K1g"]
set y2bit [mintime "exec ./build/shampi -s yices2 -e Bit $argv +RTS -K1g"]
set y1int [mintime "exec ./build/shampi -s yices1 -e Integer $argv +RTS -K1g"]
set y1bit [mintime "exec ./build/shampi -s yices1 -e Bit $argv +RTS -K1g"]
set stp [mintime "exec ./build/shampi -s stp -e Bit $argv +RTS -K1g"]

puts "hampi yices2.integer yices2.bit yices1.integer yices1.bit stp"
puts "$hampi $y2int $y2bit $y1int $y1bit $stp"

exec ./utils/rhampi_shutdown
    
