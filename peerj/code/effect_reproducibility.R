# get the clean RNA-seq table
load("d.good.Rdata")
library(ALDEx2)

## THIS IS THE REFERENCE SET
conds <- c(rep("WT", length(grep("WT", colnames(d.good)))), rep("SNF", length(grep("SNF", colnames(d.good)))) )
#list to contain the reference set of

ref.set.yeast <- vector("list", 100)
# run 100 times to get an estimate of the most reproducibly different features
for(i in 1:100){
    if(i > 3) load("ref.set.yeast.RData")

    x <- aldex.clr(d.good, conds=conds)
    x.e <- aldex.effect(x)
    x.t <- aldex.ttest(x)
    x.all <- data.frame(x.e,x.t)
    ref.set.yeast[[i]] <- x.all
    save(ref.set.yeast, file="ref.set.yeast.RData")
}

## THIS IS THE TEST SET
## saved as effect_reproducibility_test.R
#########
# determine the features found at each number of random replicates
# code below was run on server
#########
#
test.set.yeast <- lapply(test.set.yeast <- vector(mode = 'list',40), function(x) x <- vector(mode='list',100))

for(i in 2:40){
for(j in 1:100){
    if(j > 3) load("test.set.yeast.RData")
    # subset d.good by random sampling
    d.sample <- cbind(d.good[,sample( colnames(d.good[,1:43]), i)], d.good[,sample( colnames(d.good[,44:84]), i)])
    conds.i <- c(rep("WT", i), rep("SNF", i))

    x <- aldex.clr(d.sample, conds=conds.i)
    x.e <- aldex.effect(x)
    x.t <- aldex.ttest(x)
    x.all <- data.frame(x.e,x.t)

    test.set.yeast[[i]][[j]] <- x.all
    save(test.set.yeast, file="test.set.yeast.RData")
}
}

