
# Gather data comparing HAMPI with SHAMPI.
#  Uses SHAMPI with Bit representation and STP solver only.
#  Prints out individual test times (average)
set ::runspertest 10

# Return the average time in seconds to run the given script.
proc mytime {script} {
    return [expr [lindex [time $script $::runspertest] 0] / 1.0e6]
}

exec ./utils/rhampi_s &
exec sleep 1

puts "  hampi shampi"
foreach x [glob tests/cfg/* tests/cfg2/* tests/named/* tests/size/* tests/wsu/*] {
    set hampi [mytime "exec tclsh ./utils/rhampi.tcl $x"]
    set shampi [mytime "exec ./build/shampi -s stp -e Bit $x"]
    puts "$x $hampi $shampi"
}

exec ./utils/rhampi_shutdown

