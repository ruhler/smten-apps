
set term png size 600, 400
set output "bit.png"
set logscale y
plot "bit.txt" using ($2) title "yices1",  \
     "" using ($3) title "yices2", \
     "" using ($4) title "z3"

