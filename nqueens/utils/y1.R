
pdf(file="y1.pdf")
y1data <- read.table("y1.data", header=TRUE);
y1style <- read.table("style.data");

plot(c(),
     xlim=c(0,length(row.names(y1data))), ylim=c(0,max(y1data, na.rm=TRUE)),
     xlab="n", ylab="runtime(s)", main="n-queens using yices1")
points(y1data$Bool, pch=y1style["Bool", "PCH"], col=y1style["Bool", "COL"], type="b")
points(y1data$Integer, pch=y1style["Integer", "PCH"], col=y1style["Integer", "COL"], type="b")
points(y1data$Int, pch=y1style["Int", "PCH"], col=y1style["Int", "COL"], type="b")
points(y1data$Bit, pch=y1style["Bit", "PCH"], col=y1style["Bit", "COL"], type="b")
legend("top", row.names(y1style), pch=y1style$PCH, col=y1style$COL)

