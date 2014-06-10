
pdf(file="build/mini.pdf", width=4, height=4)
data <- read.table("build/mini.data")
lim = c(min(data), max(data))
plot(data, log="xy", xlim=lim, ylim=lim, pch=20)
abline(0, 1)

