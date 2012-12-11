
set ::hampi [lindex $argv 0]

foreach x [glob "tests/*.hmp"] {
    catch {exec $::hampi $x ">@" stdout}
}
    
