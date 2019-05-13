# get the clean RNA-seq table
source("code/setup.r")
sign.set.effect <- lapply(sign.set.effect <- vector(mode = 'list',7), function(x) x <- vector(mode='list',100))

seq.test <- c(2,3,5,10,15,20,40)

for(i in 1:7){
    print(seq.test[i])
    for(j in 1:100){
        print(j)
        # subset d.good by random sampling
        d.sample <- cbind(d.good[,sample( WT.g$good, seq.test[i])], d.good[,sample( SNF.g$good, seq.test[i])])
        conds.i <- c(rep("C", seq.test[i]), rep("E", seq.test[i]))

        x <- aldex.clr(d.sample, conds=conds.i)
        x.e <- aldex.effect(x, CI=TRUE)
        sign.set.effect[[i]][[j]] <- x.e
    }
save(sign.set.effect, file="data/sign.set.effect.RData")
}

