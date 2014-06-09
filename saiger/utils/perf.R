
pdf(file="build/perf.pdf", width=4, height=4)

sat <- read.table("build/sat.data")
unsat <- read.table("build/unsat.data")

lim = c(min(sat, unsat), max(sat, unsat))
plot(sat, log="xy", xlim=lim, ylim=lim, pch=1)
points(unsat, pch=18)
abline(0, 1)
legend("topleft", c("SAT", "UNSAT"), pch=c(1,18))

