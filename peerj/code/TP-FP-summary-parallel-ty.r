# difference is largely orthogonal from effect however measured
# the majority of diff FP are expressed at low levels
# this takes about 2 hours on a mobile i9 with 32 G of RAM
# data saved as data/output.summary.ty.Rda

# change this to run the simulation if wanted
# depends on TP-FP-ty-setup.r for datasets
run.this = FALSE

# TP: true positives are those found in the subset that are also 
# found in the whole dataset 
# intersect(whole,test)
# rate is TP/whole

# FP: false positives are those found in the subset that are not
# found in the whole dataset
# diff of whole 
# TP.effect <- setdiff(test,whole)

if(!exists('output.summary.ty')){  
  load(paste(my.path,'data/output.summary.ty.Rda', sep=''))
}

if(run.this == TRUE){
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
save(output.summary.ty, file=paste(my.path,'data/output.summary.ty.Rda', sep=''))
} # end run.this if statement




