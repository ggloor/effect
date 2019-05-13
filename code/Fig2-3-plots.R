# reqires functions from analyze_subsets.R

source("code/analyze_subsets.R")

#pdf("yeast_TPFP.pdf", height=10,width=6)
setEPS()
postscript("F2-yeast_TPFP.eps",  height=10,width=6)
par(mfrow=c(3,2))
# effect size 1, 2 fold difference
ret.list_10 <- get.tpfp(ref.set.yeast, test.set.yeast, 1,0)
ret.list_11 <- get.tpfp(ref.set.yeast, test.set.yeast, 1,1)

plot.ret.list.q(ret.list_10, main="q<0.1, D=0")
plot.ret.list.q(ret.list_11, main="q<0.1, D=2")

plot.ret.list(ret.list_10, main="E=1, D=0")
plot.ret.list(ret.list_11, main="E=1, D=2")


plot.FP.density(ret.list_10, ref.set.yeast, main="E=1, D=0", pos=c(1,4,9,19), leg=T)
plot.FP.density(ret.list_11, ref.set.yeast, main="E=1, D=2", pos=c(1,4,9,19))
dev.off()



### yeast volcano and effect plots
#pdf("yeast_volcano.pdf", height=5, width=9)
setEPS()
postscript("F3-yeast_volcano.eps", height=5, width=9)
par(mfrow=c(1,2))
p.sig <- ref.set.yeast[[1]]$we.eBH < 0.1
e.sig <- abs(ref.set.yeast[[1]]$effect) > 1
both.sig <- abs(ref.set.yeast[[1]]$effect) > 1 & abs(ref.set.yeast[[1]]$diff.btw) > 1

# volcano
plot(log10(ref.set.yeast[[1]]$we.eBH) * -1, ref.set.yeast[[1]]$diff.btw,
    pch=19, cex=0.8, col="grey80", main="Volcano",
    xlab="Difference", ylab="-1 * log(q)")
points(log10(ref.set.yeast[[1]]$we.eBH[p.sig]) * -1, ref.set.yeast[[1]]$diff.btw[p.sig], pch=19, cex=0.8, col="red")
points( log10(ref.set.yeast[[1]]$we.eBH[e.sig]) * -1,ref.set.yeast[[1]]$diff.btw[e.sig], pch=19, cex=0.8, col="orange")
points(log10(ref.set.yeast[[1]]$we.eBH[both.sig]) * -1,ref.set.yeast[[1]]$diff.btw[both.sig],  pch=19, cex=0.8, col="dodgerblue")
abline(v=log10(0.1) *-1, col="grey", lwd=2)
abline(h=-1, col="grey", lty=2, lwd=2)
abline(h=1, col="grey", lty=2, lwd=2)

# t(effect)
plot( ref.set.yeast[[1]]$diff.win, ref.set.yeast[[1]]$diff.btw,pch=19, cex=0.8, col="grey80",
    main="Effect",
    xlab="Dispersion", ylab="Difference")
points( ref.set.yeast[[1]]$diff.win[p.sig], ref.set.yeast[[1]]$diff.btw[p.sig],pch=19, cex=0.8, col="red")
points( ref.set.yeast[[1]]$diff.win[e.sig], ref.set.yeast[[1]]$diff.btw[e.sig],pch=19, cex=0.8, col="orange")
points( ref.set.yeast[[1]]$diff.win[both.sig], ref.set.yeast[[1]]$diff.btw[both.sig],pch=19, cex=0.8, col="dodgerblue")
abline()
legend(2.5,8, legend=c("not different", "q < 0.1", "E > 1", "E > 1, D>2"), pch=19,
    col=c(col="grey80","red","orange","dodgerblue"))
abline(h=-1, col="grey", lty=2, lwd=2)
abline(h=1, col="grey", lty=2, lwd=2)
abline(0,1, col="grey", lwd=2)
abline(0,-1, col="grey", lwd=2)
dev.off()





