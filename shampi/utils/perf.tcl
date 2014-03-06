
# Gather performance data for Hampi and Shampi.
#  Prints out individual test times (average)
set ::runspertest 8

# Return the average time in seconds to run the given script.
proc mytime {script} {
    return [expr [lindex [time $script $::runspertest] 0] / 1.0e6]
}

proc mkcfg {slv rep} {
  return [list "Shampi.$slv.$rep" "exec ./build/shampi -s $slv -e $rep"]
}

set cfgs [list  \
  [list hampi "exec tclsh ./utils/rhampi.tcl"]  \
  [mkcfg yices2 Integer] \
  [mkcfg yices2 Bit]    \
  [mkcfg yices1 Integer] \
  [mkcfg yices1 Bit] \
  [mkcfg z3 Integer] \
  [mkcfg z3 Bit]    \
  [mkcfg stp Bit]   \
  [mkcfg minisat Bit]   \
  ]

foreach {cfg} $cfgs {
    puts -nonewline " [lindex $cfg 0]"
}
puts ""

exec ./utils/rhampi_s &
exec sleep 1

foreach x [glob tests/cfg/* tests/cfg2/* tests/named/* tests/size/* tests/wsu/*] {
    puts -nonewline "$x"
    foreach {cfg} $cfgs {
        set t [mytime "[lindex $cfg 1] $x"]
        puts -nonewline " $t"
    }
    puts ""
}

exec ./utils/rhampi_shutdown

