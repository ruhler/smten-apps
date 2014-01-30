
pdf(file="build/sskVsk.pdf")
data <- read.table("build/sskVsk.data")
lim = c(min(data), max(data))
plot(data, log="xy", xlim=lim, ylim=lim)
abline(0, 1)

