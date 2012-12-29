
set ::hampi [lindex $argv 0]

foreach x [glob "tests/unit/*.hmp"] {
    puts -nonewline "$x: "
    catch {exec $::hampi $x 3 ">@" stdout "2>@" stdout}
}
    
