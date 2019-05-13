# get the clean RNA-seq table
load("data/tiyani_pup_vs_ys.Rdata")
library(ALDEx2)
library(zCompositions)
library(CoDaSeq)
null.set.ty.effect <- lapply(null.set.ty.effect <- vector(mode = 'list',9), function(x) x <- vector(mode='list',100))
null.set.ty.single.effect <- lapply(null.set.ty.effect <- vector(mode = 'list',9), function(x) x <- vector(mode='list',100))

seq.test <- c(2,3,5,10,15,20,30,50,100)

for(i in 1:9){
    for(j in 1:100){
    print(c(i,j))
        # subset d.good by random sampling
        d.samp <- cbind(sample(d[,162:373], seq.test[i]*2))

        conds.i <- c(rep("C", seq.test[i]), rep("E", seq.test[i]))

        # test the actual data
        d.sample <- d.samp[rowSums(d.samp) > 0,]
        d.s.n0 <- cmultRepl(t(d.sample), method="CZM", label=0)
        d.s.clr <- apply(d.s.n0, 1, function(x) log(x) - mean(log(x)))
        null.actual <- codaSeq.effect(d.s.clr, conds.i)
        null.set.ty.single.effect[[i]][[j]] <- null.actual

        x <- aldex.clr(d.sample, conds=conds.i)
        x.e <- aldex.effect(x)
        null.set.ty.effect[[i]][[j]] <- x.e
    }
save(null.set.ty.effect, file="data/null.set.ty.effect.RData")
save(null.set.ty.single.effect, file="data/null.set.ty.single.effect.RData")
}
