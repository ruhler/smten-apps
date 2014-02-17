
data <- read.table("foo.data", header=TRUE);

png(file="foo.png", height=250)
plot(c(), xlim=c(0, length(row.names(data))),
          ylim=c(0, max(data, na.rm=TRUE)),
          xlab="n", ylab="runtime(s)")
points(data["Fast"], pch=20, col=2)
points(data["Easy"], pch=20, col=1, type="b", lty=2)
legend("topright", c("Easy", "Fast"), pch=c(20, 20), col=c(1,2))



