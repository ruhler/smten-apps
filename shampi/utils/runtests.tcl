
set ::hampi [lindex $argv 0]

foreach x [lsort [glob "tests/unit/*.hmp"]] {
    catch {exec $::hampi -t 3 $x ">@" stdout "2>@" stdout}
}
    
