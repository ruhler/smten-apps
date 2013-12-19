
pdf(file="minisat.pdf")
minisatdata <- read.table("minisat.data", header=TRUE);
minisatstyle <- read.table("style.data");

plot(c(),
     xlim=c(0,length(row.names(minisatdata))), ylim=c(0,max(minisatdata, na.rm=TRUE)),
     xlab="n", ylab="runtime(s)", main="n-queens using minisat")
points(minisatdata$Bool, pch=minisatstyle["Bool", "PCH"], col=minisatstyle["Bool", "COL"], type="b")
points(minisatdata$Int, pch=minisatstyle["Int", "PCH"], col=minisatstyle["Int", "COL"], type="b")
points(minisatdata$Bit, pch=minisatstyle["Bit", "PCH"], col=minisatstyle["Bit", "COL"], type="b")
legend("top", row.names(minisatstyle), pch=minisatstyle$PCH, col=minisatstyle$COL)

