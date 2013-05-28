
set files $argv

#exec ./utils/rhampi_s &
foreach x $files {
    puts -nonewline "$x: "
    exec ./utils/rhampi_c $x ">@" stdout "2>@" stdout
}
exec ./utils/rhampi_shutdown

