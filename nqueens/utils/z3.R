
pdf(file="z3.pdf")
z3data <- read.table("z3.data", header=TRUE);
z3style <- read.table("style.data");

plot(c(),
     xlim=c(0,length(row.names(z3data))), ylim=c(0,max(z3data, na.rm=TRUE)),
     xlab="n", ylab="runtime(s)", main="n-queens using z3")
points(z3data$Bool, pch=z3style["Bool", "PCH"], col=z3style["Bool", "COL"], type="b")
points(z3data$Integer, pch=z3style["Integer", "PCH"], col=z3style["Integer", "COL"], type="b")
points(z3data$Int, pch=z3style["Int", "PCH"], col=z3style["Int", "COL"], type="b")
points(z3data$Bit, pch=z3style["Bit", "PCH"], col=z3style["Bit", "COL"], type="b")
legend("top", row.names(z3style), pch=z3style$PCH, col=z3style$COL)

