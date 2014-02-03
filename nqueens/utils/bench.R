
data <- read.table("build/bench.data", header=TRUE);

plotcfgs <- function(name, cfgs) {
    pdf(file=paste("build/", name, ".pdf", sep=""))
    plot(c(),
         xlim=c(0,length(row.names(data))), ylim=c(0,max(data, na.rm=TRUE)),
         xlab="n", ylab="runtime(s)", main=paste("n-queens using", name))
    for (i in 1:length(cfgs)) {
        points(data[cfgs[i]], pch=i, col=i, type="b")
    }
    legend("top", cfgs, pch=1:length(cfgs), col=1:length(cfgs))
}

plotcfgs("minisat", c("minisat.Int", "minisat.Bit", "minisat.Bool"))
plotcfgs("stp", c("stp.Int", "stp.Bit", "stp.Bool"))
plotcfgs("yices2", c("yices2.Int", "yices2.Bit", "yices2.Bool", "yices2.Integer"))
plotcfgs("yices1", c("yices1.Int", "yices1.Bit", "yices1.Bool", "yices1.Integer"))
plotcfgs("z3", c("z3.Int", "z3.Bit", "z3.Bool", "z3.Integer"))

