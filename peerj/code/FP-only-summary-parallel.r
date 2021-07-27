# this takes about 6 hours on a mobile i9 with 32 G of RAM
# data saved as data/output.summary.yeast.Rda

# change this to run the simulation if wanted
# depends on TP-FP-yeast-setup.r for datasets
run.this = FALSE


if(!exists('output.summaryFP.yeast')){  
  load(paste(my.path,'data/output.summaryFP.yeast.Rda', sep=''))
}
if(run.this == TRUE){

library(foreach)
library(doMC)

# original tests done at 100 replicates
test.size <- 100
max.samples <- 20
registerDoMC(4) # this is the MC backend. 
# 4 cores seems to be the performance limit
# beyond that there is throttling of each thread

output.summary <- list()
output.summaryFP.yeast <- list()

output.summaryFP.yeast <- foreach(j = 2:max.samples)%dopar%{
    print(j)
	n.samples = j

	summary.stats <- matrix(data=NA, ncol=6, nrow=test.size)
	
	colnames(summary.stats) <- c('eff.FP.5','eff.FP1','eff.FP2', 'ol.1.FP', 'diff.1.FP', 'triple.FP')
	
	for(i in 1:test.size){
		test.col  <- c(sample(1:43,n.samples *2))
		test.conds <- c(rep("a", n.samples), rep("b",n.samples))
		test.data <- d.good[,test.col]
		x.test <- aldex.clr(test.data, test.conds, verbose=F)
		x.e.test <- aldex.effect(x.test, verbose=F)
		
		test.diff.ol <- rownames(x.e.test)[which(x.e.test$overlap < 0.1)]
		test.diff <- rownames(x.e.test)[which(abs(x.e.test$diff.btw) > 1)]
		test.diff.eff <- rownames(x.e.test)[which(abs(x.e.test$effect) > 0.5)]
		test.diff.eff1 <- rownames(x.e.test)[which(abs(x.e.test$effect) > 1)]
		test.diff.eff2 <- rownames(x.e.test)[which(abs(x.e.test$effect) > 2)]
		test.triple <- intersect(test.diff.eff1, intersect(test.diff.ol, test.diff))

		summary.stats[i,1] <- length(test.diff.eff)
		summary.stats[i,2] <- length(test.diff.eff1)
		summary.stats[i,3] <- length(test.diff.eff2)
		summary.stats[i,4] <- length(test.diff.ol)
		summary.stats[i,5] <- length(test.diff)
		summary.stats[i,6] <- length(test.triple)


	}    
    output.summary[[j]] <- summary.stats
}
save(output.summaryFP.yeast, file=paste(my.path,'data/output.summaryFP.yeast.Rda', sep=''))
}

if(!exists('output.summaryFP.ty')){  
  load(paste(my.path,'data/output.summaryFP.ty.Rda', sep=''))
}

# original tests done at 100 replicates
test.size <- 100
max.samples <- 20
registerDoMC(6) # this is the MC backend. 
# 4 cores seems to be the performance limit
# beyond that there is throttling of each thread
# takes less than an hour on i9 32G 

if(run.this == TRUE){

output.summary <- list()
output.summaryFP.ty <- list() # this is the young soldier cohort

output.summaryFP.ty <- foreach(j = 2:max.samples)%dopar%{
    print(j)
	n.samples = j

	summary.stats <- matrix(data=NA, ncol=6, nrow=test.size)
	
	colnames(summary.stats) <- c('eff.FP.5','eff.FP1','eff.FP2', 'ol.1.FP', 'diff.1.FP', 'triple.FP')
	
	for(i in 1:test.size){
		test.col  <- c(sample(161:373,n.samples *2))
		test.conds <- c(rep("a", n.samples), rep("b",n.samples))
		test.data <- d[,test.col]
		x.test <- aldex.clr(test.data, test.conds, verbose=F)
		x.e.test <- aldex.effect(x.test, verbose=F)
		
		test.diff.ol <- rownames(x.e.test)[which(x.e.test$overlap < 0.1)]
		test.diff <- rownames(x.e.test)[which(abs(x.e.test$diff.btw) > 1)]
		test.diff.eff <- rownames(x.e.test)[which(abs(x.e.test$effect) > 0.5)]
		test.diff.eff1 <- rownames(x.e.test)[which(abs(x.e.test$effect) > 1)]
		test.diff.eff2 <- rownames(x.e.test)[which(abs(x.e.test$effect) > 2)]
		test.triple <- intersect(test.diff.eff1, intersect(test.diff.ol, test.diff))

		summary.stats[i,1] <- length(test.diff.eff)
		summary.stats[i,2] <- length(test.diff.eff1)
		summary.stats[i,3] <- length(test.diff.eff2)
		summary.stats[i,4] <- length(test.diff.ol)
		summary.stats[i,5] <- length(test.diff)
		summary.stats[i,6] <- length(test.triple)


	}    
    output.summary[[j]] <- summary.stats
}
save(output.summaryFP.ty, file=paste(my.path,'peerj/data/output.summaryFP.ty.Rda', sep=''))
}

med.data.FP.yeast <- matrix(data=NA, nrow=(max.samples - 1), 7)
colnames(med.data.FP.yeast) <- c('n', colnames(output.summaryFP.yeast[[1]]))
upper.data.FP.yeast <- matrix(data=NA, nrow=(max.samples - 1), 7)

for( i in 1:(max.samples - 1) ){
  med.data.FP.yeast[i,1] <- i+1
  med.data.FP.yeast[i,2:7] <- apply(output.summaryFP.yeast[[i]], 2, median)
  upper.data.FP.yeast[i,1] <- i+1
  upper.data.FP.yeast[i,2:7] <- apply(output.summaryFP.yeast[[i]], 2, function(x) quantile(x, probs=c(0.025, 0.5, 0.95))[3])
}

# young soldier cohort, youth cohort gives essentially same result
med.data.FP.ty <- matrix(data=NA, nrow=(max.samples - 1), 7)
colnames(med.data.FP.ty) <- c('n', colnames(output.summaryFP.ty[[1]]))
upper.data.FP.ty <- matrix(data=NA, nrow=(max.samples - 1), 7)

for( i in 1:(max.samples - 1) ){
  med.data.FP.ty[i,1] <- i+1
  med.data.FP.ty[i,2:7] <- apply(output.summaryFP.ty[[i]], 2, median)
  upper.data.FP.ty[i,1] <- i+1
  upper.data.FP.ty[i,2:7] <- apply(output.summaryFP.ty[[i]], 2, function(x) quantile(x, probs=c(0.025, 0.5, 0.95))[3])
}

