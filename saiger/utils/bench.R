
pdf(file="build/bench.pdf")

sat <- read.table("data/bench.2.sat")
unsat <- read.table("data/bench.2.unsat")

lim = c(min(sat, unsat), max(sat, unsat))
plot(sat, log="xy", xlim=lim, ylim=lim, pch=1)
points(unsat, pch=2)
abline(0, 1)
legend("topleft", c("SAT", "UNSAT"), pch=1:2)

