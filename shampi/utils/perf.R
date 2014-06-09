
data <- read.table("build/perf.data")
lim = c(min(data), max(data))

cmpcfgs <- function(name, cfg1, cfg2) {
    pdf(file=paste("build/", name, ".pdf", sep=""), width=4,height=4)
    plot(data[,c(cfg1, cfg2)], log="xy", xlim=lim, ylim=lim, pch=20)
    abline(0, 1)
}

cmpcfgs("hVstpb", "hampi", "Shampi.stp.Bit")
cmpcfgs("hVy2b", "hampi", "Shampi.yices2.Bit")

