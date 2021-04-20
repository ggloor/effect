library(foreach)
library(doMC)

load('~/Documents/0_git/effect/data/d.good.Rdata')

# original tests done at 100 replicates
test.size <- 100
max.samples <- 20
registerDoMC(4) # this is the MC backend. 
# 4 cores seems to be the performance limit
# beyond that there is throttling of each thread

output.summary <- list()
output.summaryFP <- list()

output.summaryFP <- foreach(j = 2:max.samples)%dopar%{
    print(j)
	n.samples = j

	summary.stats <- matrix(data=NA, ncol=4, nrow=test.size)
	
	colnames(summary.stats) <- c('eff.FP1','eff.FP2', 'ol.FP', 'diff.FP')
	
	for(i in 1:test.size){
		test.col  <- c(sample(1:43,n.samples *2))
		test.conds <- c(rep("a", n.samples), rep("b",n.samples))
		test.data <- d.good[,test.col]
		x.test <- aldex.clr(test.data, test.conds, verbose=F)
		x.e.test <- aldex.effect(x.test, verbose=F)
		
		test.diff.ol <- rownames(x.e.test)[which(x.e.test$overlap < 0.05)]
		test.diff <- rownames(x.e.test)[which(abs(x.e.test$diff.btw) > 1)]
		test.diff.eff <- rownames(x.e.test)[which(abs(x.e.test$effect) > 1)]
		test.diff.eff2 <- rownames(x.e.test)[which(abs(x.e.test$effect) > 2)]

		summary.stats[i,1] <- length(test.diff.eff)
		summary.stats[i,2] <- length(test.diff.eff2)
		summary.stats[i,3] <- length(test.diff.ol)
		summary.stats[i,4] <- length(test.diff)

	}    
    output.summary[[j]] <- summary.stats
}

# dataset is called 'd'
load('~/Documents/0_git/effect/data/tiyani_pup_vs_ys.Rdata')

# original tests done at 100 replicates
test.size <- 100
max.samples <- 20
registerDoMC(6) # this is the MC backend. 
# 4 cores seems to be the performance limit
# beyond that there is throttling of each thread

output.summary <- list()
output.summaryFP.ty2 <- list()

output.summaryFP.ty2 <- foreach(j = 2:max.samples)%dopar%{
    print(j)
	n.samples = j

	summary.stats <- matrix(data=NA, ncol=5, nrow=test.size)
	
	colnames(summary.stats) <- c('eff.FP1','eff.FP2', 'ol.FP', 'diff.FP', 'diff.eff.FP')
	
	for(i in 1:test.size){
		test.col  <- c(sample(161:373,n.samples *2))
		test.conds <- c(rep("a", n.samples), rep("b",n.samples))
		test.data <- d[,test.col]
		x.test <- aldex.clr(test.data, test.conds, verbose=F)
		x.e.test <- aldex.effect(x.test, verbose=F)
		
		test.diff.ol <- rownames(x.e.test)[which(x.e.test$overlap < 0.05)]
		test.diff <- rownames(x.e.test)[which(abs(x.e.test$diff.btw) > 1)]
		test.diff.eff <- rownames(x.e.test)[which(abs(x.e.test$effect) > 1)]
		test.diff.eff2 <- rownames(x.e.test)[which(abs(x.e.test$effect) > 2)]
		test.diff.eff3 <- rownames(x.e.test)[which(x.e.test$overlap < 0.05 & abs(x.e.test$diff.btw) > 1) ]

		summary.stats[i,1] <- length(test.diff.eff)
		summary.stats[i,2] <- length(test.diff.eff2)
		summary.stats[i,3] <- length(test.diff.ol)
		summary.stats[i,4] <- length(test.diff)
		summary.stats[i,5] <- length(test.diff.eff3)

	}    
    output.summary[[j]] <- summary.stats
}

med.data.FP <- matrix(data=NA, nrow=(max.samples - 1), 5)
upper.data.FP <- matrix(data=NA, nrow=(max.samples - 1), 5)
upper.data.FP <- matrix(data=NA, nrow=(max.samples - 1), 5)
for( i in 1:(max.samples - 1) ){
  med.data.FP[i,1] <- i+1
  med.data.FP[i,2:5] <- apply(output.summaryFP[[i]], 2, median)
  upper.data.FP[i,2:5] <- apply(output.summaryFP[[i]], 2, function(x) quantile(x, probs=c(0.025, 0.5, 0.975))[3])
}

# youth cohort
med.data.FP.ty <- matrix(data=NA, nrow=(max.samples - 1), 5)
upper.data.FP.ty <- matrix(data=NA, nrow=(max.samples - 1), 5)
upper.data.FP.ty <- matrix(data=NA, nrow=(max.samples - 1), 5)
for( i in 1:(max.samples - 1) ){
  med.data.FP.ty[i,1] <- i+1
  med.data.FP.ty[i,2:5] <- apply(output.summaryFP.ty[[i]], 2, median)
  upper.data.FP.ty[i,2:5] <- apply(output.summaryFP.ty[[i]], 2, function(x) quantile(x, probs=c(0.025, 0.5, 0.975))[3])
}
# young soldier cohort
med.data.FP.ty2 <- matrix(data=NA, nrow=(max.samples - 1), 6)
upper.data.FP.ty2 <- matrix(data=NA, nrow=(max.samples - 1), 6)
upper.data.FP.ty2 <- matrix(data=NA, nrow=(max.samples - 1), 6)
for( i in 1:(max.samples - 1) ){
  med.data.FP.ty2[i,1] <- i+1
  med.data.FP.ty2[i,2:6] <- apply(output.summaryFP.ty2[[i]], 2, median)
  upper.data.FP.ty2[i,2:6] <- apply(output.summaryFP.ty2[[i]], 2, function(x) quantile(x, probs=c(0.025, 0.5, 0.975))[3])
}

plot(med.data.FP[,1], (med.data.FP[,2])/nrow(d.good),  col='black', 
  ylim=c(1e-4,0.6), log='y', type='b', xlab='N samples', ylab='False positive proportion')
points(med.data.FP[,1], (med.data.FP[,3])/nrow(d.good), col='grey40', type='b')
points(med.data.FP[,1], (med.data.FP[,4])/nrow(d.good), col='red', type='b')
points(med.data.FP[,1], (med.data.FP[,5])/nrow(d.good), col='cornflowerblue', type='b')

points(med.data.FP[,1], (med.data.FP.ty[,2])/nrow(d), pch=0, col='black', type='b')
points(med.data.FP[,1], (med.data.FP.ty[,3])/nrow(d), pch=0,col='grey40', type='b')
points(med.data.FP[,1], (med.data.FP.ty[,4])/nrow(d),pch=0, col='red', type='b')
points(med.data.FP[,1], (med.data.FP.ty[,5])/nrow(d),pch=0, col='cornflowerblue', type='b')

points(med.data.FP[,1], (med.data.FP.ty2[,2])/nrow(d), pch=6, col='black', type='b')
points(med.data.FP[,1], (med.data.FP.ty2[,3])/nrow(d), pch=6,col='grey40', type='b')
points(med.data.FP[,1], (med.data.FP.ty2[,4])/nrow(d),pch=6, col='red', type='b')
points(med.data.FP[,1], (med.data.FP.ty2[,5])/nrow(d),pch=6, col='cornflowerblue', type='b')
points(med.data.FP[,1], (med.data.FP.ty2[,6])/nrow(d),pch=6, col='orange', type='b')

legend(10, 1e-1, legend=c('yeast', 'soldier', 'student'), col=c('grey','grey','grey'), pch=c(1,0,6) )

legend(12, 1e-1, legend=c('diff >1', 'E >1', 'E >2', 'ol <0.05', 'ol & diff' ), col=c('cornflowerblue', 'black', 'grey40', 'red', 'brown'), pch=15 )