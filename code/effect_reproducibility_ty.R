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
     x.e <- aldex.effect(x, conds)
     x.t <- aldex.ttest(x, conds)
     x.all <- data.frame(x.e,x.t)

     ref.set.ty[[i]] <- x.all
     save(ref.set.ty, file="ref.set.ty.RData")
 }

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
