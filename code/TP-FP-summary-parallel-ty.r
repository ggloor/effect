library(ALDEx2)
load('~/Documents/0_git/effect/data/tiyani_pup_vs_ys.Rdata')

# THIS RAN SUCCESSFULLY ON SERVER
 conds.ty <- c(rep("Pup", 161), rep("YS", 212))

t <- aldex.clr(d, conds=conds.ty)
t.e <- aldex.effect(t)

ty.ol <- rownames(t.e)[which(t.e$overlap < 0.05)]
ty.diff <- rownames(t.e)[which(abs(t.e$diff.btw) > 1)]
ty.eff <- rownames(t.e)[which(abs(t.e$effect) > 1)]

ty.diff.ol <- intersect(ty.ol, ty.diff)
ty.diff.eff <- intersect(ty.eff, ty.diff)
ty.ol.eff <- intersect(ty.eff, ty.ol)


#######


output.summary.ty <- list()
output.summary2 <- list()

# original tests done at 100 replicates
test.size <- 100
max.samples <- 20
registerDoMC(6) # this is the MC backend. 
# 4 cores seems to be the performance limit
# beyond that there is throttling of each thread

output.summary.ty <- foreach(j = 2:max.samples)%dopar%{
    print(j)
	n.samples = j

	summary.stats <- matrix(data=NA, ncol=12, nrow=test.size)
	
	colnames(summary.stats) <- c('eff.TP', 'eff.FP', 'diff.TP', 'diff.FP', 'eff.diff.TP', 'eff.diff.FP', 'ol.TP', 'ol.FP', 'ol.diff.TP','ol.diff.FP', 'ol.eff.diff.TP', 'ol.eff.diff.FP')
	
	for(i in 1:test.size){
		test.col <- c(sample(1:161, n.samples), sample(162:373, n.samples))
		test.conds <- c(rep("P", n.samples), rep("Y", n.samples))
		test.data <- d[,test.col]
		
		x.test <- aldex.clr(test.data, test.conds, verbose=F)
		x.e.test <- aldex.effect(x.test, CI=T, verbose=F)
		
		test.diff <- rownames(x.e.test)[which(abs(x.e.test$diff.btw) > 1)]
		
		test.ol <- rownames(x.e.test)[which(x.e.test$overlap < 0.1)]

		test.eff <- rownames(x.e.test)[which(abs(x.e.test$effect) > 1)]
		

		test.diff.eff <- intersect(test.diff, test.eff) 
		test.diff.ol <- intersect(test.diff, test.ol) 
		test.ol.eff.diff <- intersect(test.eff, intersect(test.ol, test.diff)) 

		summary.stats[i,1] <- length(intersect(ty.eff, test.eff))
		summary.stats[i,2] <- length(setdiff(test.eff, ty.eff))

		summary.stats[i,3] <- length(intersect(ty.diff, test.diff))
		summary.stats[i,4] <- length(setdiff(test.diff, ty.diff))

		summary.stats[i,5] <- length(intersect(ty.diff.eff, test.diff.eff))
		summary.stats[i,6] <- length(setdiff(test.diff.eff, ty.diff.eff))

		summary.stats[i,7] <- length(intersect(ty.ol, test.ol))
		summary.stats[i,8] <- length(setdiff(test.ol, ty.ol))

		summary.stats[i,9] <- length(intersect(ty.diff.ol, test.diff.ol))
		summary.stats[i,10] <- length(setdiff(test.diff.ol, ty.diff.ol))
		
		summary.stats[i,11] <- length(intersect(ty.diff.eff, test.ol.eff.diff))
		summary.stats[i,12] <- length(setdiff(test.ol.eff.diff, ty.diff.eff))
	}
    
    output.summary2[[j]] <- summary.stats


}

med.data.ty <- matrix(data=NA, nrow=(max.samples - 1), ncol=13)
colnames(med.data.ty) <- c('N samples', colnames(output.summary.ty[[1]]) )
for( i in 1:(max.samples - 1) ){
  med.data.ty[i,1] <- i+1
  med.data.ty[i,2:13] <- apply(output.summary.ty[[i]], 2, median)
}

# TP open, FP closed
# proportion of TP found
plot(med.data.ty[,1], med.data.ty[,2]/length(ty.eff), ylim=c(0,1), main='Proportion',
  xlab='N samples per group', ylab='TP and FP rate')
points(med.data.ty[,1], med.data.ty[,4]/length(ty.diff), col="cornflowerblue")
#points(med.data.ty[,1], med.data.ty[,8]/length(ty.ol), col="brown")
points(med.data.ty[,1], med.data.ty[,12]/length(ty.diff.eff), col="orange")

# FP - proportion of all that are FP
points(med.data.ty[,1], med.data.ty[,3]/(med.data.ty[,2] + med.data.ty[,3]), pch=19)
points(med.data.ty[,1], med.data.ty[,5]/(med.data.ty[,4] + med.data.ty[,5]), pch=19, col="cornflowerblue")
#points(med.data.ty[,1], med.data.ty[,9]/(med.data.ty[,8] + med.data.ty[,9]), pch=19, col="brown")
points(med.data.ty[,1], med.data.ty[,13]/(med.data.ty[,12] + med.data.ty[,11]), pch=19, col="orange")

legend(15.5,0.5, legend=c('effect','difference','overlap','triple'),pch=19, col=c("black", "cornflowerblue",'brown','orange','grey70','grey40'))

legend(12.8,0.5, legend=c('TP','FP'),pch=c(1,19))
abline(h=0.8, lty=2, lwd=3, col='darkgrey')
abline(h=0.1, lty=2, lwd=3, col='darkgrey')

#######

n.samples = 10
test.col <- c(sample(1:161, n.samples), sample(162:373, n.samples))
test.conds <- c(rep("Pup", n.samples), rep("YS", n.samples))
test.data <- d[,test.col]

t.s <- aldex.clr(test.data, test.conds)
t.s.e <- aldex.effect(t.s)

test.ty.ol <- rownames(t.s.e)[which(t.s.e$overlap < 0.05)]
test.ty.diff <- rownames(t.s.e)[which(abs(t.s.e$diff.btw) > 1)]
test.ty.eff <- rownames(t.s.e)[which(abs(t.s.e$effect) > 1)]
test.ty.diff.ol <- intersect(test.ty.ol, test.ty.diff)
test.ty.ol.ef <- intersect(test.ty.ol, test.ty.eff)
test.ty.diff.eff <- intersect(test.ty.eff, test.ty.diff)

TP.ty.ol <- intersect(ty.ol, test.ty.ol)
TP.ty.eff <- intersect(ty.eff, test.ty.eff)
TP.ty.diff <- intersect(ty.diff, test.ty.diff)
TP.ty.diff.ol <- intersect(ty.diff.ol, test.ty.diff.ol)
TP.ty.diff.eff <- intersect(ty.diff.eff, test.ty.diff.eff)
TP.ty.ol.eff <- intersect(ty.ol.eff, test.ty.ol.eff)

FP.ty.ol <- setdiff(test.ty.ol, ty.ol)
FP.ty.eff <- setdiff(test.ty.eff, ty.eff)
FP.ty.diff <- setdiff(test.ty.diff, ty.diff)
FP.ty.diff.ol <- setdiff(test.ty.diff.ol, ty.diff.ol)
FP.ty.diff.eff <- setdiff(test.ty.diff.eff, ty.diff.eff)

plot(t.e$diff.win, t.e$diff.btw, col='grey')	
points(t.e[FP.ty.eff,'diff.win'], t.e[FP.ty.eff,'diff.btw'], col='blue')
points(t.e[FP.ty.ol,'diff.win'], t.e[FP.ty.ol,'diff.btw'], col='orange', pch=19, cex=0.5)
points(t.e[FP.ty.diff,'diff.win'], t.e[FP.ty.diff,'diff.btw'], col='red')
points(t.e[TP.ty.ol,'diff.win'], t.e[TP.ty.ol,'diff.btw'], col='cyan')
points(t.e[TP.ty.eff,'diff.win'], t.e[TP.ty.eff,'diff.btw'], col='cyan', pch=19, cex=0.5)
