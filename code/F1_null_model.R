# data from code/Fu_null_data.r
load("data/list.null.Rdata")
load("data/list.one.Rdata")

### FIGURE 1 in paper
#pdf("null-model.pdf", height=8, width=7)
setEPS()
postscript("F1-null-model.eps", height=8, width=7)
par(mfrow=c(2,2))

plot(list.null$rnorm.null.effect[,1],list.null$rnorm.null.effect[,8], ylim=c(0,2), type="l",
    main=expression(Epsilon[d]), lwd=2, xlab="sample size", ylab="effect size")
points(list.null$rnorm.null.effect[,1],list.null$rnorm.null.effect[,5], type="l", lwd=2, lty=4)

points(list.null$runif.null.effect[,1],list.null$runif.null.effect[,8], ylim=c(0,5), type="l", lwd=2, col="red")
points(list.null$runif.null.effect[,1],list.null$runif.null.effect[,5], type="l", lwd=2, lty=4, col="red")

points(list.null$rbeta.null.effect[,1],list.null$rbeta.null.effect[,8], ylim=c(0,5), type="l", lwd=2, col="cyan")
points(list.null$rbeta.null.effect[,1],list.null$rbeta.null.effect[,5], type="l", lwd=2, lty=4, col="cyan")

points(list.null$rcauchy.null.effect[,1],list.null$rcauchy.null.effect[,8], ylim=c(0,5), type="l", lwd=2, col="orange")
points(list.null$rcauchy.null.effect[,1],list.null$rcauchy.null.effect[,5], type="l", lwd=2, lty=4, col="orange")
legend(20,2, legend=c("Normal","Uniform","Cauchy","Beta"), col=c("black", "red", "orange", "cyan"), pch=15)
abline(h=1, col="grey")
abline(h=0.5,col="grey")

plot(list.null$rnorm.cohen.null.effect[,1],list.null$rnorm.cohen.null.effect[,8], ylim=c(0,2), type="l",
    main="Cohen's d", lwd=2, xlab="sample size", ylab="effect size")
points(list.null$rnorm.cohen.null.effect[,1],list.null$rnorm.cohen.null.effect[,5], type="l", lwd=2, lty=4)

points(list.null$runif.cohen.null.effect[,1],list.null$runif.cohen.null.effect[,8], ylim=c(0,5), lwd=2, type="l", col="red")
points(list.null$runif.cohen.null.effect[,1],list.null$runif.cohen.null.effect[,5], type="l", lwd=2, lty=4, col="red")

points(list.null$rbeta.cohen.null.effect[,1],list.null$rbeta.cohen.null.effect[,8], ylim=c(0,5), lwd=2, type="l", col="cyan")
points(list.null$rbeta.cohen.null.effect[,1],list.null$rbeta.cohen.null.effect[,5], type="l", lwd=2, lty=4, col="cyan")

points(list.null$rcauchy.cohen.null.effect[,1],list.null$rcauchy.cohen.null.effect[,8], ylim=c(0,5), lwd=2, type="l", col="orange")
points(list.null$rcauchy.cohen.null.effect[,1],list.null$rcauchy.cohen.null.effect[,5], type="l", lwd=2, lty=4, col="orange")

abline(h=1, col="grey")
abline(h=0.5, col="grey")


plot(list.one$rnorm.diff1.effect[,1],list.one$rnorm.diff1.effect[,8], ylim=c(0,5), type="l",
    main=expression(Epsilon[d]), lwd=2, xlab="sample size", ylab="effect size")
points(list.one$rnorm.diff1.effect[,1],list.one$rnorm.diff1.effect[,5], type="l", lwd=2, lty=4)

points(list.one$runif.diff1.effect[,1],list.one$runif.diff1.effect[,8],  type="l", lwd=2, col="red")
points(list.one$runif.diff1.effect[,1],list.one$runif.diff1.effect[,5], type="l", lwd=2, lty=4, col="red")

points(list.one$rbeta.diff1.effect[,1],list.one$rbeta.diff1.effect[,8],  type="l", lwd=2, col="cyan")
points(list.one$rbeta.diff1.effect[,1],list.one$rbeta.diff1.effect[,5], type="l", lwd=2, lty=4, col="cyan")

points(list.one$rcauchy.diff1.effect[,1],list.one$rcauchy.diff1.effect[,8],  type="l", lwd=2, col="orange")
points(list.one$rcauchy.diff1.effect[,1],list.one$rcauchy.diff1.effect[,5], type="l", lwd=2, lty=4, col="orange")
abline(h=1, col="grey")
abline(h=0.5, col="grey")

plot(list.one$rnorm.cohen1.effect[,1],list.one$rnorm.cohen1.effect[,8], ylim=c(0,5), type="l",
    main="Cohen's d", lwd=2, xlab="sample size", ylab="effect size")
points(list.one$rnorm.cohen1.effect[,1],list.one$rnorm.cohen1.effect[,5], type="l", lwd=2, lty=4)

points(list.one$runif.cohen1.effect[,1],list.one$runif.cohen1.effect[,8], type="l", lwd=2, col="red")
points(list.one$runif.cohen1.effect[,1],list.one$runif.cohen1.effect[,5], type="l", lwd=2, lty=4, col="red")

points(list.one$rbeta.cohen1.effect[,1],list.one$rbeta.cohen1.effect[,8], type="l", lwd=2, col="cyan")
points(list.one$rbeta.cohen1.effect[,1],list.one$rbeta.cohen1.effect[,5], type="l", lwd=2, lty=4, col="cyan")

points(list.one$rcauchy.cohen1.effect[,1],list.one$rcauchy.cohen1.effect[,8], type="l", lwd=2, col="orange")
points(list.one$rcauchy.cohen1.effect[,1],list.one$rcauchy.cohen1.effect[,5], type="l", lwd=2, lty=4, col="orange")

abline(h=1, col="grey")
abline(h=0.5, col="grey")
dev.off()



