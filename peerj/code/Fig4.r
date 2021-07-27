###### Figure 4 
# depends on a bunch of data!
# TP-FP-yeast-setup.r, TP-FP-ty-setup.r
# TP-FP-yeast-summary-parallel.r, TP-FP-summary-parallel-ty.r
# TP-FP-summary-parallel-edgeR.r

# summarize the data

# calculated on yeast dataset
med.data.yeast <- matrix(data=NA, nrow=(max.samples - 1), ncol=13)
colnames(med.data.yeast) <- c('N samples', colnames(output.summary.yeast[[1]]) )
for( i in 1:(max.samples - 1) ){
  med.data.yeast[i,1] <- i+1
  med.data.yeast[i,2:13] <- apply(output.summary.yeast[[i]], 2, median)
}

# calculated on yeast dataset
med.data.edgeR <- matrix(data=NA, nrow=(max.samples - 1), ncol=7)
for( i in 1:(max.samples - 1) ){
  med.data.edgeR[i,1] <- i+1
  med.data.edgeR[i,2:7] <- apply(output.summary.edgeR[[i]], 2, median)
}

# calculated on ty 16S dataset
med.data.ty <- matrix(data=NA, nrow=(max.samples - 1), ncol=13)
colnames(med.data.ty) <- c('N samples', colnames(output.summary.ty[[1]]) )
for( i in 1:(max.samples - 1) ){
  med.data.ty[i,1] <- i+1
  med.data.ty[i,2:13] <- apply(output.summary.ty[[i]], 2, median)
}


####### lets plot

pdf('TPvsFP.pdf', height=6.5, width=11)
par(mfrow=c(1,2))

# TP open, FP closed in yeast dataset
# proportion of TP found
plot(med.data.yeast[,1], med.data.yeast[,2]/length(yeast.eff), ylim=c(0,1), main='A: Yeast TP/FP',
  xlab='N samples per group', 
  ylab='TP and FP rate')
points(med.data.edgeR[,1], med.data.edgeR[,2]/length(diff.p), type='b', col="grey70", lwd=2)
points(med.data.edgeR[,1], med.data.edgeR[,6]/length(diff.p.FC), type='b', col="grey40", lwd=2)
points(med.data.yeast[,1], med.data.yeast[,4]/length(yeast.diff), col="cornflowerblue")
#points(med.data.yeast[,1], med.data.yeast[,6]/length(yeast.diff.eff), col="blue")
points(med.data.yeast[,1], med.data.yeast[,8]/length(yeast.ol), col="brown")
points(med.data.yeast[,1], med.data.yeast[,12]/length(yeast.diff.eff), col="orange")

# FP - proportion of all that are FP
points(med.data.edgeR[,1], med.data.edgeR[,3]/(med.data.edgeR[,2] + med.data.edgeR[,3]), type='b', pch=19, col="grey70", lwd=2)
points(med.data.edgeR[,1], med.data.edgeR[,7]/(med.data.edgeR[,6] + med.data.edgeR[,7]), type='b', pch=19, col="grey40", lwd=2)
points(med.data.yeast[,1], med.data.yeast[,3]/(med.data.yeast[,2] + med.data.yeast[,3]), pch=19)
points(med.data.yeast[,1], med.data.yeast[,5]/(med.data.yeast[,4] + med.data.yeast[,5]), pch=19, col="cornflowerblue")
#points(med.data.yeast[,1], med.data.yeast[,7]/(med.data.yeast[,6] + med.data.yeast[,7]), pch=19, col="blue")
points(med.data.yeast[,1], med.data.yeast[,9]/(med.data.yeast[,8] + med.data.yeast[,9]), pch=19, col="brown")
points(med.data.yeast[,1], med.data.yeast[,13]/(med.data.yeast[,12] + med.data.yeast[,13]), pch=19, col="orange")


legend(15.5,0.75, legend=c('effect','difference','overlap','triple','q','q+diff'),pch=19, col=c("black", "cornflowerblue", 'brown','orange','grey70','grey40'))

legend(12.8,0.75, legend=c('TP','FP'),pch=c(1,19))
abline(h=0.8, lty=2, lwd=3, col='darkgrey')
abline(h=0.1, lty=2, lwd=3, col='darkgrey')

# TP open, FP closed in ty dataset
# proportion of TP found
plot(med.data.ty[,1], med.data.ty[,2]/length(ty.eff), ylim=c(0,1),
  main='B: 16S TP/FP', xlab='N samples per group', 
  ylab='TP and FP rate')
points(med.data.ty[,1], med.data.ty[,4]/length(ty.diff), col="cornflowerblue")
points(med.data.ty[,1], med.data.ty[,8]/length(ty.ol), col="brown")
points(med.data.ty[,1], med.data.ty[,12]/length(ty.diff.eff), col="orange")

# FP - proportion of all that are FP
points(med.data.ty[,1], med.data.ty[,3]/(med.data.ty[,2] + med.data.ty[,3]), pch=19)
points(med.data.ty[,1], med.data.ty[,5]/(med.data.ty[,4] + med.data.ty[,5]), pch=19, col="cornflowerblue")
points(med.data.ty[,1], med.data.ty[,9]/(med.data.ty[,8] + med.data.ty[,9]), pch=19, col="brown")
points(med.data.ty[,1], med.data.ty[,13]/(med.data.ty[,12] + med.data.ty[,11]), pch=19, col="orange")

legend(15.5,0.5, legend=c('effect','difference','overlap','triple'),pch=19, col=c("black", "cornflowerblue",'brown','orange','grey70','grey40'))

legend(12.8,0.5, legend=c('TP','FP'),pch=c(1,19))
abline(h=0.8, lty=2, lwd=3, col='darkgrey')
abline(h=0.1, lty=2, lwd=3, col='darkgrey')


dev.off()