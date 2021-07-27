###
# Figure 2
###
# depends on TP-FP-yeast.setup, TP-FP-ty.setup, FP-only-summary-parallel.r


pdf('FP-plots.pdf', height=4.5, width=12)
par(mfrow=c(1,3))

plot(med.data.FP.yeast[,1], (med.data.FP.yeast[,3])/nrow(d.good),  col='grey30', 
  ylim=c(1e-4,0.6), log='y', type='b', xlab='N samples', ylab='False positive proportion', main='A')
points(med.data.FP.yeast[,1], (med.data.FP.yeast[,2])/nrow(d.good), col='grey70', type='b')
points(med.data.FP.yeast[,1], (med.data.FP.yeast[,4])/nrow(d.good), col='black', type='b')
points(med.data.FP.yeast[,1], (med.data.FP.yeast[,5])/nrow(d.good), col='brown', type='b')
points(med.data.FP.yeast[,1], (med.data.FP.yeast[,6])/nrow(d.good), col='cornflowerblue', type='b')
points(med.data.FP.yeast[,1], (med.data.FP.yeast[,7])/nrow(d.good), col='orange', type='b')
#points(med.data.FP.yeast[,1], (upper.data.FP.yeast[,7])/nrow(d.good), col='orange', type='b', pch=19)


points(med.data.FP.ty[,1], (med.data.FP.ty[,2])/nrow(d), pch=0, col='grey70', type='b')
points(med.data.FP.ty[,1], (med.data.FP.ty[,3])/nrow(d), pch=0,col='grey30', type='b')
points(med.data.FP.ty[,1], (med.data.FP.ty[,4])/nrow(d),pch=0, col='black', type='b')
points(med.data.FP.ty[,1], (med.data.FP.ty[,5])/nrow(d),pch=0, col='brown', type='b')
points(med.data.FP.ty[,1], (med.data.FP.ty[,6])/nrow(d),pch=0, col='cornflowerblue', type='b')
points(med.data.FP.ty[,1], (med.data.FP.ty[,7])/nrow(d),pch=0, col='orange', type='b')
#points(med.data.FP.ty[,1], (upper.data.FP.ty[,7])/nrow(d),pch=17, col='orange', type='b')

legend(15, 3.5e-4, legend=c('yeast', '16S'), col=c('grey','grey'), pch=c(1,0) )

legend(10, 1.5e-3, legend=c('diff >1', 'E > 0.5', 'E >1', 'E >2', 'ol <0.1', 'triple' ), col=c('cornflowerblue', 'grey70', 'grey30','black', 'brown', 'orange'), pch=15 )

plot(x.e$diff.win, x.e$diff.btw, pch=19, col=rgb(0,0,0,0.1), xlab="Dispersion", ylab='Difference', main='B')
points(x.e[setdiff(test.diff, yeast.diff),'diff.win'], x.e[setdiff(test.diff, yeast.diff),'diff.btw'], pch=19, col=rgb(0,0.7,1,0.4))
points(x.e[setdiff(test.eff, yeast.eff),'diff.win'], x.e[setdiff(test.eff, yeast.eff),'diff.btw'], pch=19, col=rgb(1,0.6,0,0.4))
legend(2, 7,legend=c('FP difference',"FP effect", 'not FP'),pch=19, col=c(rgb(0,0.6,0.9,1), rgb(.9,0.55,0,1), rgb(0,0,0,0.3)))


plot(t.e$diff.win, t.e$diff.btw, pch=19, col=rgb(0,0,0,0.1), xlab='Dispersion', ylab='Difference', main='C')
points(t.e[setdiff(test.ty.diff, ty.diff),'diff.win'], t.e[setdiff(test.ty.diff, ty.diff),'diff.btw'], pch=19, col=rgb(0,0.7,1,0.4))
points(t.e[setdiff(test.ty.eff, ty.eff),'diff.win'], t.e[setdiff(test.ty.eff, ty.eff),'diff.btw'], pch=19, col=rgb(1,0.6,0,0.4))

dev.off()
