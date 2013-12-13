
pdf(file="y2.pdf")
y2data <- read.table("y2.data", header=TRUE);
y2style <- read.table("y2.style");

plot(c(),
     xlim=c(0,length(row.names(y2data))), ylim=c(0,max(y2data, na.rm=TRUE)),
     xlab="n", ylab="runtime(s)", main="n-queens using yices2")
points(y2data$Bool, pch=y2style["Bool", "PCH"], col=y2style["Bool", "COL"], type="b")
points(y2data$Integer, pch=y2style["Integer", "PCH"], col=y2style["Integer", "COL"], type="b")
points(y2data$Int, pch=y2style["Int", "PCH"], col=y2style["Int", "COL"], type="b")
points(y2data$Bit, pch=y2style["Bit", "PCH"], col=y2style["Bit", "COL"], type="b")
legend("top", row.names(y2style), pch=y2style$PCH, col=y2style$COL)

