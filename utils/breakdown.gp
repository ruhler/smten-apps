
set terminal png size 1200, 800
set output "breakdown.png"

set style data histograms
set style histogram rows
set style fill solid border -1
set key left
plot "data/prof.y2.int.hard.txt" using 2 title "Match", '' using 3 title "Check", '' using 4 title "Fix", '' using 5 title "Parse", '' using 6 title "RunCmds", '' using 7 title "SeriS", '' using 8 title "SmtE", '' using 9 title "Other"

