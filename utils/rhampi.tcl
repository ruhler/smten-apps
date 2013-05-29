
set files $argv

foreach x $files {
    puts -nonewline "$x: "
    exec ./utils/rhampi_c $x ">@" stdout "2>@" stdout
}

