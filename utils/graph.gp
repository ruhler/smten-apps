
set terminal svg
set output "graph.svg"
#set terminal latex size 2.5, 1.67
#set output "graph.tex"
set logscale y
set autoscale yfixmax
set key left
set xlabel "Test"
set xtics nomirror out 0, 100, 500
set ytics nomirror out
set ylabel "Time (sec)"

plot \
  "cvmsah_sorted.bench" using ($2/1000000) title "Hampi", \
  '' using ($7/1000000) title "Shampi with STP", \
  '' using ($21/1000000) title "Shampi Potential with STP", \
  '' using ($17/1000000) title "Shampi Potential with Yices2, Integers" \
#  '' using ($3/1000000) title "\\textsc{cv.y2i}", \
#  '' using ($14/1000000) title "\\textsc{ms.stp}", \
#  '' using ($10/1000000) title "\\textsc{ms.y2i}", \
#  '' using ($4/1000000) title "\\textsc{cv.y2b}" \

