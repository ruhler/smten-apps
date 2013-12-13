
set ::runspertest 1
set ::maxtimepertest 30

# Return the average time in seconds to run the given script.
proc mytime {script} {
    return [expr [lindex [time $script $::runspertest] 0] / 1.0e6]
}

proc runconf {solver rep} {
  puts "$solver.$rep"
  set times [list ]
  for {set n 1} {true} {incr n} {
     set t [mytime "exec ./nqueens -s $solver -e $rep $n"]
     puts "$n $t"
     lappend times $t

     if {$t > $::maxtimepertest} { 
         break
     }
  }
  return $times
}

set y1int [runconf yices1 Int]
set y1integer [runconf yices1 Integer]
#set y1bit [runconf yices1 Bit]     # n = 55 doesn't work
set y1bool [runconf yices1 Bool]
set y2int [runconf yices2 Int]
set y2integer [runconf yices2 Integer]
set y2bit [runconf yices2 Bit]
set y2bool [runconf yices2 Bool]
set z3int [runconf z3 Int]
set z3integer [runconf z3 Integer]
set z3bit [runconf z3 Bit]
set z3bool [runconf z3 Bool]
set stpint [runconf stp Int]
#set stpinteger [runconf stp Integer]   # stp doesn't support integers.
set stpbit [runconf stp Bit]
set stpbool [runconf stp Bool]

# Print the summary
puts "y1.int [llength $y1int] [lindex $y1int end]"
puts "y1.integer [llength $y1integer] [lindex $y1integer end]"
#puts "y1.bit [llength $y1bit] [lindex $y1bit end]"
puts "y1.bool [llength $y1bool] [lindex $y1bool end]"
puts "y2.int [llength $y2int] [lindex $y2int end]"
puts "y2.integer [llength $y2integer] [lindex $y2integer end]"
puts "y2.bit [llength $y2bit] [lindex $y2bit end]"
puts "y2.bool [llength $y2bool] [lindex $y2bool end]"
puts "z3.int [llength $z3int] [lindex $z3int end]"
puts "z3.integer [llength $z3integer] [lindex $z3integer end]"
puts "z3.bit [llength $z3bit] [lindex $z3bit end]"
puts "z3.bool [llength $z3bool] [lindex $z3bool end]"
puts "stp.int [llength $stpint] [lindex $stpint end]"
#puts "stp.integer [llength $stpinteger] [lindex $stpinteger end]"
puts "stp.bit [llength $stpbit] [lindex $stpbit end]"
puts "stp.bool [llength $stpbool] [lindex $stpbool end]"

