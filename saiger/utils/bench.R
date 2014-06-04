
pdf(file="build/bench.pdf", width=4, height=4)

sat <- read.table("data/bench.2.sat")
unsat <- read.table("data/bench.2.unsat")

lim = c(min(sat, unsat), max(sat, unsat))
plot(sat, log="xy", xlim=lim, ylim=lim, pch=1)
points(unsat, pch=18)
abline(0, 1)
legend("topleft", c("SAT", "UNSAT"), pch=c(1,18))

