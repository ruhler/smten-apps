
pdf(file="build/bench.pdf")
data <- read.table("build/bench.data")
lim = c(min(data), max(data))
plot(data, log="xy", xlim=lim, ylim=lim)
abline(0, 1)

