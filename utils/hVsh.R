
pdf(file="build/hVsh.pdf")
data <- read.table("build/hVsh.data")
lim = c(min(data), max(data))
plot(data, log="xy", xlim=lim, ylim=lim)
abline(0, 1)

