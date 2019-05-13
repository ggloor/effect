# compare 8-12yo to ys of tiyani dataset
#
# addys=TRUE
# min.prop=0.001
# min.occurrence=0.1
# source("tiyani/chunk/libraries_data.R")
# # but these are not filtered, just need the parameters to run
#
# d.dirty <- data.frame(pup, ys, stringsAsFactors=F)
# d <-  codaSeq.filter(d.dirty, min.prop=0, min.occurrence=0, samples.by.row=FALSE)

# 1:161 are pupils
# 162:373 are ys
#  effect_reproducibility_ty_ref.R
library(ALDEx2)
load("tiyani_pup_vs_ys.Rdata")

# THIS RAN SUCCESSFULLY ON SERVER
 conds <- c(rep("Pup", 161), rep("YS", 212))
 # #
 # list to contain the reference set of
 ref.set.ty <- vector("list")

 # this was run 100 times to get an estimate of the most reprodicibly different features
 for(i in 1:100){
     if(i > 3) load("ref.set.ty.RData")
     x <- aldex.clr(d, conds=conds)
     x.e <- aldex.effect(x)
     x.t <- aldex.ttest(x)
     x.all <- data.frame(x.e,x.t)

     ref.set.ty[[i]] <- x.all
     save(ref.set.ty, file="ref.set.ty.RData")
 }

#########
# determine the features found at each number of random replicates
# code below was run on server
#########

library(ALDEx2)
load("data/tiyani_pup_vs_ys.Rdata") # variable is 'd'

sizes <- c(2,3,4,5,8,10, 16,20,32,40,64,80,100,128,150)

test.set.ty <- lapply(test.set.ty <- vector(mode = 'list',length(sizes)), function(x) x <- vector(mode='list',100))
names(test.set.ty) <- sizes
# 1:161 are pupils
# 162:373 are ys


for(i in 1:length(sizes)){
for(j in 1:100){
    if(j > 3) load("test.set.ty.RData")
    # subset by random sampling
    d.sample <- cbind(sample(d[,1:161], sizes[i]), sample(d[,162:373], sizes[i]))
    conds.i <- c(rep("Pup", sizes[i]), rep("YS", sizes[i]))

    x <- aldex.clr(d.sample, conds=conds.i)
    x.e <- aldex.effect(x)
    x.t <- aldex.ttest(x,)
    x.all <- data.frame(x.e,x.t)

    test.set.ty[[i]][[j]] <- x.all
    save(test.set.ty, file="test.set.ty.RData")
}
}


#####
# code below is for single (not expected value) effect measure and t-test
# run on laptop
#####

load("data/tiyani_pup_vs_ys.Rdata")
library(ALDEx2)
library(zCompositions)
library(CoDaSeq)

sizes <- c(2,3,4,5,8,10, 16,20,32,40,64,80,100,128,150)

test.set.single.ty <- lapply(test.set.single.ty <- vector(mode = 'list',length(sizes)), function(x) x <- vector(mode='list',100))
names(test.set.single.ty) <- sizes
# 1:161 are pupils
# 162:373 are ys


for(i in 1:length(sizes)){
for(j in 1:100){
    # subset by random sampling
    d.sample <- cbind(sample(d[,1:161], sizes[i]), sample(d[,162:373], sizes[i]))

    conds.i <- c(rep("C", sizes[i]), rep("E", sizes[i]))

    d.sample <- d.sample[rowSums(d.sample) > 0,]
    d.n0 <- cmultRepl(t(d.sample), label=0, method="CZM")
    d.clr <- apply(d.n0, 1, function(x) log2(x) - mean(log2(x)))

    p <- apply(d.clr, 1, function(x) as.numeric(t.test(x[1:sizes[i]], x[(sizes[i]+1):(2*sizes[i])])[3]) )
    q <- p.adjust(p)
    effect <- codaSeq.effect(d.clr, conds.i)
    pqe <- data.frame(p,q,effect)

    test.set.single.ty[[i]][[j]] <- pqe

}
    save(test.set.single.ty, file="test.set.single.ty.RData")
}
