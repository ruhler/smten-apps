
pdf(file="y2.pdf")
y2data <- read.table("y2.data", header=TRUE);

cfgs = c("Bool", "Integer", "Int", "Bit")

plot(c(),
     xlim=c(0,length(row.names(y2data))), ylim=c(0,max(y2data, na.rm=TRUE)),
     xlab="n", ylab="runtime(s)", main="n-queens using yices2")

for (i in 1:length(cfgs)) {
    points(y2data[cfgs[i]], pch=i, col=i, type="b")
}

legend("top", cfgs, pch=1:length(cfgs), col=1:length(cfgs))

