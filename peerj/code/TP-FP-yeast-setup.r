# generate the data for Figure 1
# generate the single iteration TP/FP dataset for Figure 2
# generate the reference TP/FP dataset for Figures 2, 3 and 4

# variable name for yeast data is called 'd.good'

if(!exists('d.good')){  
  load(paste(my.path,'data/d.good.Rdata', sep=''))
}

# run this only the first time if the data do not exist
# saves time for re-runs if already completed

if(!exists('test.eff')){

	conds <- c(rep("WT",43),rep("SN", 41))

	x <- aldex.clr(d.good, conds)
	x.e <- aldex.effect(x, CI=T)
	x.t <- aldex.ttest(x)

	# reference TP rownames
	yeast.ol <- rownames(x.e)[which(x.e$overlap < 0.05)] 
	yeast.diff <- rownames(x.e)[which(abs(x.e$diff.btw) > 1)]
	yeast.eff <- rownames(x.e)[which(abs(x.e$effect) > 1)]

	yeast.diff.eff <- intersect(yeast.eff, yeast.diff)
	yeast.diff.ol <- intersect(yeast.ol, yeast.diff)


	# generate a random dataset and collect false positive features
	n.samples=5

	test.col  <- c(sample(1:43,n.samples), sample(44:84,n.samples))
	test.conds <- c(rep("WT", n.samples), rep("SN",n.samples))
	x.test <- aldex.clr(d.good[,test.col], test.conds, verbose=F)
	x.e.test <- aldex.effect(x.test, verbose=F)

	test.diff <- rownames(x.e.test)[which(abs(x.e.test$diff.btw) > 1)]
	test.ol <- rownames(x.e.test)[which(x.e.test$overlap < 0.1)]
	test.eff <- rownames(x.e.test)[which(abs(x.e.test$effect) > 1)]
} # end test for test.eff

