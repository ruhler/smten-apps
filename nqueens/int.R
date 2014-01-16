
pdf(file="int.pdf")
y1data <- read.table("y1.data", header=TRUE);
y2data <- read.table("y2.data", header=TRUE);
z3data <- read.table("z3.data", header=TRUE);
msdata <- read.table("minisat.data", header=TRUE);
stpdata <- read.table("stp.data", header=TRUE);
style <- read.table("style.data")

xmax <- max(length(y1data$Int),
            length(y2data$Int),
            length(z3data$Int),
            length(msdata$Int),
            length(stpdata$Int))
ymax <- max(y1data$Int, y2data$Int, z3data$Int, msdata$Int, stpdata$Int, na.rm=TRUE)

plot(c(),
     xlim=c(0,xmax), ylim=c(0,ymax),
     xlab="n", ylab="runtime(s)", main="n-queens using Int")
points(y1data$Int, pch=style["Bool", "PCH"], col=style["Bool", "COL"], type="b")
points(y2data$Int, pch=style["Integer", "PCH"], col=style["Integer", "COL"], type="b")
points(z3data$Int, pch=style["Int", "PCH"], col=style["Int", "COL"], type="b")
points(msdata$Int, pch=style["Bit", "PCH"], col=style["Bit", "COL"], type="b")
legend("top", c("Yices1", "Yices2", "Z3", "MiniSat"), pch=style$PCH, col=style$COL)

