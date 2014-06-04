
data <- read.table("build/perf.data")
lim = c(min(data), max(data))

pdf(file="build/hVy2b.pdf", width=4,height=4)
plot(data[,c("hampi","Shampi.yices2.Bit")], log="xy", xlim=lim, ylim=lim, pch=20)
abline(0, 1)

pdf(file="build/hVstpb.pdf", width=4, height=4)
plot(data[,c("hampi","Shampi.stp.Bit")], log="xy", xlim=lim, ylim=lim, pch=20)
abline(0, 1)

#pdf(file="build/hVms.pdf")
#plot(data[,c("hampi","Shampi.minisat.Bit")], log="xy", xlim=lim, ylim=lim)
#abline(0, 1)
#
#pdf(file="build/y2bVstpb.pdf")
#plot(data[,c("Shampi.yices2.Bit","Shampi.stp.Bit")], log="xy", xlim=lim, ylim=lim)
#abline(0, 1)
#
#pdf(file="build/y2bVy2i.pdf")
#plot(data[,c("Shampi.yices2.Bit","Shampi.yices2.Integer")], log="xy", xlim=lim, ylim=lim)
#abline(0, 1)
#
#pdf(file="build/y2bVy1i.pdf")
#plot(data[,c("Shampi.yices2.Bit","Shampi.yices1.Integer")], log="xy", xlim=lim, ylim=lim)
#abline(0, 1)
#
#pdf(file="build/y2bVz3b.pdf")
#plot(data[,c("Shampi.yices2.Bit","Shampi.z3.Bit")], log="xy", xlim=lim, ylim=lim)
#abline(0, 1)
#
#pdf(file="build/y2bVz3i.pdf")
#plot(data[,c("Shampi.yices2.Bit","Shampi.z3.Integer")], log="xy", xlim=lim, ylim=lim)
#abline(0, 1)
#
#pdf(file="build/y2bVmsb.pdf")
#plot(data[,c("Shampi.yices2.Bit","Shampi.minisat.Bit")], log="xy", xlim=lim, ylim=lim)
#abline(0, 1)
#
