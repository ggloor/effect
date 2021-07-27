# depends on TP-FP-yeast-setup.r

#https://www.andifugard.info/cohens-u1-u2-and-u3/
eff <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.2, 1.4, 1.6, 1.8, 2, 2.5, 3, 4)

prob <- c(0.50,0.54,0.58,0.62,0.66,0.69,0.73,0.76,0.79,0.82,0.84,0.88,0.92,0.945,0.964,0.977,0.99,0.999,0.99997)

pdf('null-effect.pdf', height=4.5, width=8.5)
par(mfrow=c(1,2))

plot(x.e$effect, x.t$wi.eBH, pch=19, col=rgb(0,0,1,0.2), log='y', xlim=c(-1.5,1.5),
   ylim=c(1e-10, 1), xlab="Effect size", ylab="Adjusted p-value", main='A')
points(x.e$effect, x.t$we.eBH, pch=19, col=rgb(1,0,0,0.2))
abline(h=0.1, lty=2, lwd=2, col='grey')
legend(-0.5, 1e-7,legend=c('Wilcoxon',"Welch's t"),pch=19, col=c("blue", "red"))

plot(abs(x.e$effect), x.e$overlap, pch=19, cex=0.2, col=rgb(0,0.5,1,0.2), xlim=c(0,4), xlab="Effect size", ylab="Overlap", main='B')
points(abs(t.e$effect), t.e$overlap, pch=19, cex=0.4, col=rgb(1,.6,0,0.2))
points(eff, 1-prob, col="red", pch=1, type='b', lwd=1.5)

dev.off()
