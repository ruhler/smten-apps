
ms <- read.table("build/ms.data")
ms2 <- read.table("build/ms2.data")
ms3 <- read.table("build/ms3.data")
ms4 <- read.table("build/ms4.data")
ms5 <- read.table("build/ms5.data")
ms6 <- read.table("build/ms6.data")
ms7 <- read.table("build/ms7.data")
sms <- read.table("build/sms.data")

cfgnms = c("MS", "MS2", "MS3", "MS4", "MS5", "MS6", "MS7", "SMS")
cfgs=list(ms, ms2, ms3, ms4, ms5, ms6, ms7, sms)

pdf(file="build/nqueens_icfp14.pdf", height=5 )
plot(c(),
     xlim=c(0, max(length(row.names(ms)))),
     ylim=c(min(unlist(lapply(cfgs, FUN=min)))
          , max(unlist(lapply(cfgs, FUN=max)))),
     xlab="n",
     ylab="runtime(s)"
     )
for (i in 1:length(cfgs)) {
    points(cfgs[[i]], pch=i, col=i, type="b", lty=2)
}
legend("topleft", cfgnms, pch=1:length(cfgnms), col=1:length(cfgnms))


