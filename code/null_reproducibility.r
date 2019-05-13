# get the clean RNA-seq table
source("setup.r")

null.set.effect <- lapply(null.set.effect <- vector(mode = 'list',6), function(x) x <- vector(mode='list',100))

seq.test <- c(2,3,5,10,15,20)

for(i in 1:6){
    for(j in 1:100){
        # subset d.good by random sampling
        d.sample <- cbind(d.good[,sample( WT.g$good, seq.test[i]*2)])
        conds.i <- c(rep("C", seq.test[i]), rep("E", seq.test[i]))

        x <- aldex.clr(d.sample, conds=conds.i)
        x.e <- aldex.effect(x, conds.i)
        null.set.effect[[i]][[j]] <- x.e
    }
        save(null.set.effect, file="null.set.effect.RData")
}

