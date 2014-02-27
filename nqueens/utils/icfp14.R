
lm <- read.table("build/nqueensl.data")
cms <- read.table("build/cminisat.data")
cms2 <- read.table("build/cms2.data")
cy2 <- read.table("build/cy2.data")
msint <- read.table("build/smtenmsint.data")
msbit <- read.table("build/smtenmsbit.data")
msbool <- read.table("build/smtenmsbool.data")
msbool2 <- read.table("build/smtenmsbool2.data")
y2bool2 <- read.table("build/smteny2bool2.data")

#cfgnms = c("List Monad", "Hand SAT MS", "Hand SAT Y2", "Smten Int", "Smten Bit", "Smten Bool", "Smten Bool2 Y2")
#cfgs=list(lm, cms, cy2, msint, msbit, msbool, y2bool2)
cfgnms = c("List Monad", "SAT Based I", "SAT Based II", "Smten Int", "Smten Bool")
cfgs=list(lm, cms, cms2, msint, msbool2)

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


