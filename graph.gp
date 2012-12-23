
set terminal png size 1200, 800
set output "graph.png"
set logscale y
set key left

plot "graph.data" using ($1) smooth csplines title "hampi", "graph.data" using ($2) title "shampi"

