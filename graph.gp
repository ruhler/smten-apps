
set terminal png size 1200, 800
set output "graph.png"
set logscale y
set key left

plot 1 smooth csplines title "hampi", "graph.data" using ($1) smooth csplines title "A", "graph.data" using ($2) smooth csplines title "B", "graph.data" using ($3) smooth csplines title "C", "graph.data" using ($4) smooth csplines title "D", "graph.data" using ($5) smooth csplines title "E

