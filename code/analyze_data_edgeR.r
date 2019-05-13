source("code/plots.r")
load("data/ref.set.edgeR.RData")
load("data/ref.set.edgeR.et.RData")
load("data/x.all.Rdata")
load("data/test.set.edgeR.Rdata")
load("data/test.set.edgeR.et.Rdata")


tp.edgeR <- ref.set.edgeR
#mp.edgeR <- setdiff(Reduce(union, ref.set.edgeR), Reduce(intersect, ref.set.edgeR))
tn.edgeR <- setdiff(rownames(x.all), c(tp.edgeR, mp.edgeR))

tp.edgeR.et <- ref.set.edgeR
mp.edgeR.et <- mp.edgeR
tn.edgeR.et <- setdiff(rownames(x.all), c(tp.edgeR.et, mp.edgeR.et))

par(mfrow=c(2,2))
analyze_data(tp.edgeR,mp.edgeR,tn.edgeR,test.set.edgeR,main="ep1", 5000, 40)
sens_spec(tp.edgeR,mp.edgeR,tn.edgeR,test.set.edgeR,main="ep1", 40)

analyze_data(tp.edgeR.et,mp.edgeR.et,tn.edgeR.et,test.set.edgeR.et,main="ep1", 5000, 40)
sens_spec(tp.edgeR.et,mp.edgeR.et,tn.edgeR.et,test.set.edgeR.et,main="ep1", 40)

