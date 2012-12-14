
set terminal png
set output "graph.png"
set logscale y

plot "graph.data" using ($2/$1), "graph.data" using ($3/$1)

