# difference is largely orthogonal from effect however measured
# the majority of diff FP are expressed at low levels
# this takes about 6 hours on a mobile i9 with 32 G of RAM
# data saved as data/output.summary.yeast.Rda

# change this to run the simulation if wanted
# depends on TP-FP-yeast-setup.r for datasets
run.this = FALSE

# TP: true positives are those found in the subset that are also 
# found in the whole dataset 
# intersect(whole,test)
# rate is TP/whole

# FP: false positives are those found in the subset that are not
# found in the whole dataset
# diff of whole 
# TP.effect <- setdiff(test,whole)

if(!exists('output.summary.yeast')){  
  load(paste(my.path,'data/output.summary.yeast.Rda', sep=''))
}

max.samples <- 20

if(run.this == TRUE){
output.summary2 <- list()
output.summary.yeast <- list()

# original tests done at 100 replicates
test.size <- 100
registerDoMC(4) # this is the MC backend. 
# 4 cores seems to be the performance limit
# beyond that there is throttling of each thread 
# tested on mobile i9 CPU with 32G of RAM

output.summary.yeast <- foreach(j = 2:max.samples)%dopar%{
    print(j)
	n.samples = j

	summary.stats <- matrix(data=NA, ncol=12, nrow=test.size)
	
	colnames(summary.stats) <- c('eff.TP', 'eff.FP', 'diff.TP', 'diff.FP', 'eff.diff.TP', 'eff.diff.FP', 'ol.TP', 'ol.FP', 'ol.diff.TP','ol.diff.FP', 'ol.eff.diff.TP', 'ol.eff.diff.FP')
	
	for(i in 1:test.size){
		test.col  <- c(sample(1:43,n.samples), sample(44:84,n.samples))
		test.conds <- c(rep("WT", n.samples), rep("SN",n.samples))
		x.test <- aldex.clr(d.good[,test.col], test.conds, verbose=F)
		x.e.test <- aldex.effect(x.test, CI=T, verbose=F)
		
		test.diff <- rownames(x.e.test)[which(abs(x.e.test$diff.btw) > 1)]
		
		test.ol <- rownames(x.e.test)[which(x.e.test$overlap < 0.1)]

		test.eff <- rownames(x.e.test)[which(abs(x.e.test$effect) > 1)]
		

		test.diff.eff <- intersect(test.diff, test.eff) 
		test.diff.ol <- intersect(test.diff, test.ol) 
		test.ol.eff.diff <- intersect(test.eff, intersect(test.ol, test.diff)) 

		summary.stats[i,1] <- length(intersect(yeast.eff, test.eff))
		summary.stats[i,2] <- length(setdiff(test.eff, yeast.eff))

		summary.stats[i,3] <- length(intersect(yeast.diff, test.diff))
		summary.stats[i,4] <- length(setdiff(test.diff, yeast.diff))

		summary.stats[i,5] <- length(intersect(yeast.diff.eff, test.diff.eff))
		summary.stats[i,6] <- length(setdiff(test.diff.eff, yeast.diff.eff))

		summary.stats[i,7] <- length(intersect(yeast.ol, test.ol))
		summary.stats[i,8] <- length(setdiff(test.ol, yeast.ol))

		summary.stats[i,9] <- length(intersect(yeast.diff.ol, test.diff.ol))
		summary.stats[i,10] <- length(setdiff(test.diff.ol, yeast.diff.ol))
		
		summary.stats[i,11] <- length(intersect(yeast.diff.eff, test.ol.eff.diff))
		summary.stats[i,12] <- length(setdiff(test.ol.eff.diff, yeast.diff.eff))
	}  
    output.summary2[[j]] <- summary.stats
}

save(output.summary.yeast, file=paste(my.path,'data/output.summary.yeast.Rda', sep=''))

} # this is the end of the run.this if statement

