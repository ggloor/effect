# this is quick
# under an hour
# depends on TP-FP-yeast-setup.r
if(!exists('output.summary.edgeR')){  
  load(paste(my.path,'data/output.summary.edgeR.Rda', sep=''))
}

run.this=FALSE
# takes less than an hour

if(run.this == TRUE){

group <- factor(c(rep("WT",43),rep("SN", 41)))

y <- DGEList(counts=d.good, group=group)

keep <- filterByExpr(y)

y <- y[keep,,keep.lib.sizes=FALSE]

# normalize library sizes
# here we are scaling the counts in each sample
# choosing a midpoint in each sample
y <- calcNormFactors(y)

# add in the experimental design
design <- model.matrix(~group)

# fit a Negative Binomial to the data
y <- estimateDisp(y,design)

# Log likelihood test (kinda a t-test)
fit <- glmFit(y,design)
lrt <- glmLRT(fit,coef=2)

tt <- topTags(lrt, n=nrow(d.good))

diff.p <- rownames(tt)[which(tt[[1]]$FDR < 0.1)]
diff.FC <- union(rownames(tt)[which(tt[[1]]$logFC > 1)], 
  rownames(tt)[which(tt[[1]]$logFC < -1)])
diff.p.FC <- intersect(diff.p,diff.FC)

output.summary2 <- list()
output.summary.edgeR <- list()

# original tests done at 100 replicates
test.size <- 100
max.samples <- 20
registerDoMC(4) # this is the MC backend. 

output.summary.edgeR <- foreach(j = 2:max.samples)%dopar%{
    print(j)
	n.samples = j

	summary.stats <- matrix(data=NA, ncol=6, nrow=test.size)
	
	colnames(summary.stats) <- c('p.TP', 'p.FP', 'FC.TP', 'FC.FP', 'pFC.TP', 'pFC.FP')
	
	for(i in 1:test.size){
		test.col  <- c(sample(1:43,n.samples), sample(44:84,n.samples))
		group <- factor( c(rep("WT", n.samples), rep("SN",n.samples)) )
		y <- DGEList(counts=d.good[,test.col], group=group)

		keep <- filterByExpr(y)

		y <- y[keep,,keep.lib.sizes=FALSE]

		y <- calcNormFactors(y)

		design <- model.matrix(~group)

		y <- estimateDisp(y,design)

		# Log likelihood test (kinda a t-test)
		fit <- glmFit(y,design)
		lrt <- glmLRT(fit,coef=2)
		tt <- topTags(lrt, n=nrow(d.good))
		
		test.diff.p <- rownames(tt)[which(tt[[1]]$FDR < 0.1)]
		test.diff.FC <- union(rownames(tt)[which(tt[[1]]$logFC > 1)], 
  		  rownames(tt)[which(tt[[1]]$logFC < -1)])
		test.diff.p.FC <- intersect(test.diff.p,test.diff.FC)

		summary.stats[i,1] <- length(intersect(diff.p, test.diff.p))
		summary.stats[i,2] <- length(setdiff(test.diff.p, diff.p))
		summary.stats[i,3] <- length(intersect(diff.FC, test.diff.FC))
		summary.stats[i,4] <- length(setdiff(test.diff.FC, diff.FC))
		
		summary.stats[i,5] <- length(intersect(test.diff.p.FC, diff.p.FC))
		summary.stats[i,6] <- length(setdiff(test.diff.p.FC, diff.p.FC))
		}
	output.summary2[[j]] <- summary.stats
}
save(output.summary.ty, file=paste(my.path,'data/output.summary.edgeR.Rda', sep=''))

}

