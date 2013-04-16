
#set terminal png size 1200, 800
#set output "graph.png"
set terminal latex size 2.5, 1.67
set output "graph.tex"
set logscale y
set autoscale yfixmax
set key left
set xlabel "Test"
set xtics nomirror out 0, 100, 500
set ytics nomirror out
set ylabel "\\begin{sideways}Time (sec)\\end{sideways}"

plot \
  "bench.smten.3.txt" using ($2/1000000) title "\\textsc{Hampi}" smooth csplines, \
  '' using ($7/1000000) title "\\textsc{Shampi}" with points pt 0 \
#  '' using ($5) title "y1int", \
#  '' using ($6) title "y1bit", \
#  '' using ($4) title "y2bit", \
#  '' using ($7) title "stp"

