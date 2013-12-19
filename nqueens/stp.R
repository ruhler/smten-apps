
pdf(file="stp.pdf")
stpdata <- read.table("stp.data", header=TRUE);
stpstyle <- read.table("style.data");

plot(c(),
     xlim=c(0,length(row.names(stpdata))), ylim=c(0,max(stpdata, na.rm=TRUE)),
     xlab="n", ylab="runtime(s)", main="n-queens using stp")
points(stpdata$Bool, pch=stpstyle["Bool", "PCH"], col=stpstyle["Bool", "COL"], type="b")
points(stpdata$Int, pch=stpstyle["Int", "PCH"], col=stpstyle["Int", "COL"], type="b")
points(stpdata$Bit, pch=stpstyle["Bit", "PCH"], col=stpstyle["Bit", "COL"], type="b")
legend("top", row.names(stpstyle), pch=stpstyle$PCH, col=stpstyle$COL)

