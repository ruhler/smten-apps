
set ::hampi [lindex $argv 0]

foreach x [glob "tests/*.hmp"] {
    puts -nonewline "$x: "
    catch {exec $::hampi $x 10 ">@" stdout "2>@" stdout}
}
    
