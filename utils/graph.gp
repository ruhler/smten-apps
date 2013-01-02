
set terminal png size 1200, 800
set output "graph.png"
set logscale y
set key left

plot \
  "data/wsu.1.txt" using ($2) title "hampi", \
  '' using ($5) title "y1int", \
  '' using ($6) title "y1bit", \
  '' using ($3) title "y2int", \
  '' using ($4) title "y2bit", \
  '' using ($7) title "stp"

