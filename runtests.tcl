
set ::hampi [lindex $argv 0]

foreach x [glob "tests/*.hmp"] {
    puts -nonewline "$x: "
    catch {exec $::hampi $x ">@" stdout "2>@" stdout}
}
    
