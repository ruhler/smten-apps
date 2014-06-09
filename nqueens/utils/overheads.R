
ms <- read.table("build/ms.data")
ms2 <- read.table("build/ms2.data")
ms3 <- read.table("build/ms3.data")
ms4 <- read.table("build/ms4.data")
ms5 <- read.table("build/ms5.data")
ms6 <- read.table("build/ms6.data")
ms7 <- read.table("build/ms7.data")
ms8 <- read.table("build/ms8.data")
ms10 <- read.table("build/ms10.data")
sms <- read.table("build/sms.data")

plotcfgs <- function(name, cfgnms, cfgs) {
    pdf(file=paste("build/oh_", name, ".pdf", sep=""), height=5)
    plot(c(),
         xlim=c(0.1, max(length(row.names(ms)))),
         ylim=c(min(unlist(lapply(cfgs, FUN=min)))
              , max(unlist(lapply(cfgs, FUN=max)))),
         xlab="n",
         ylab="Runtime(s)"
         )
    for (i in 1:length(cfgs)) {
        df <- cfgs[[i]]
        ix <- seq(1, nrow(df), 2)
        points(ix, df[ix,], pch=i, type="p", lty=2, cex=0.5)
    }
    legend("topleft", cfgnms, pch=1:length(cfgnms))
}

plotcfgs("all", 
    c("Hand", "CNF", "HaskellFFI", "BoolFF.Lazy", "BoolFF.Opt", "CacheCreate", "CacheUse", "CustomBuild", "Finite", "Smten"),
    list(ms, ms2, ms3, ms5, ms6, ms8, ms7, ms10, ms4, sms))

plotcfgs("norm", 
    c("CNF", "HaskellFFI", "BoolFF.Lazy", "BoolFF.Opt", "CacheCreate", "CacheUse", "CustomBuild", "Finite", "Smten"),
    list(ms2/sms, ms3/sms, ms5/sms, ms6/sms, ms8/sms, ms7/sms, ms10/sms,
         ms4/sms, sms/sms))

plotcfgs("nice", 
    c("with Unbounded", "with Sharing", "with Haskell, Formula AST", "with CNF Generation", "C-MiniSat"),
    list(sms, ms4, ms6, ms2, ms))

plotcfgs("handVsmten", 
    c("Hand", "Smten"),
    list(ms, sms))

plotcfgs("cnf", 
    c("Hand", "CNF", "Smten"),
    list(ms, ms2, sms))
