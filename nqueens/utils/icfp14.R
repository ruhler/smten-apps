
lm <- read.table("build/nqueensl.data")
cms <- read.table("build/cminisat.data")
msint <- read.table("build/smtenmsint.data")
msbit <- read.table("build/smtenmsbit.data")
msbool <- read.table("build/smtenmsbool.data")

cfgnms = c("List Monad", "Hand SAT", "Smten Int", "Smten Bit", "Smten Bool")
cfgs=list(lm, cms, msint, msbit, msbool)

pdf(file="build/nqueens_icfp14.pdf", height=5 )
plot(c(),
     xlim=c(0, max(length(row.names(cms)))),
     ylim=c(min(unlist(lapply(cfgs, FUN=min)))
          , max(unlist(lapply(cfgs, FUN=max)))),
     xlab="n",
     ylab="runtime(s)",
     log="y"
     )
for (i in 1:length(cfgs)) {
    points(cfgs[[i]], pch=i, col=i, type="b", lty=2)
}
legend("bottomright", cfgnms, pch=1:length(cfgnms), col=1:length(cfgnms))


