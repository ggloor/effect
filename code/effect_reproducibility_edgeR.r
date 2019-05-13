# get the clean RNA-seq table
source("code/setup.r")

## THIS IS THE REFERENCE SET
####
# Need to add in edgeR for comparison with Barton group paper
# also, ALDEx2 has essentially no FP, just FN
# while point estimate methods have both
# edgeR
library(edgeR)

 group <- factor(c(rep("C", length(WT.g$good)), rep("E", length(SNF.g$good))) )
 y <- DGEList(counts=d.good,group=group)
 y <- calcNormFactors(y)
 design <- model.matrix(~group)
 y <- estimateDisp(y,design)
 fit <- glmQLFit(y,design)
 qlf <- glmQLFTest(fit,coef=2)
 sum.tags <- data.frame(topTags(qlf, n=nrow(d.good)))
 ref.set.edgeR <- rownames(sum.tags[sum.tags$FDR < 0.05,])

 save(ref.set.edgeR, file="data/ref.set.edgeR.RData")

et.ref <- exactTest(y)
ref.set.edgeR.et <- data.frame(topTags(et.ref, n=nrow(d.good)))
save(ref.set.edgeR.et, file="data/ref.set.edgeR.et.RData")



## THIS IS THE TEST SET
test.set.edgeR <- lapply(test.set.edgeR <- vector(mode = 'list',40), function(x) x <- vector(mode='list',100))
test.set.edgeR.et <- lapply(test.set.edgeR.et <- vector(mode = 'list',40), function(x) x <- vector(mode='list',100))


### add in exactTest for comparison with Schurch
for(i in 2:40){

    for(j in 1:100){

    d.sample <- cbind(d.good[,sample( WT.g$good, i)], d.good[,sample( SNF.g$good, i)])
    group.i <- factor(c(rep("C", i), rep("E", i)))


    y <- DGEList(counts=d.sample,group=group.i)
    y <- calcNormFactors(y)
    design <- model.matrix(~group.i)
    y <- estimateDisp(y,design)
    fit <- glmQLFit(y,design)
    qlf <- glmQLFTest(fit,coef=2)
    sum.tags <- data.frame(topTags(qlf, n=nrow(d.good)))
    test.set.edgeR[[i]][[j]] <- rownames(sum.tags[sum.tags$FDR < 0.05,])
    et <- exactTest(y)
    sum.tags.et <- data.frame(topTags(et, n=nrow(d.good)))
    test.set.edgeR.et[[i]][[j]] <- rownames(sum.tags.et[sum.tags.et$FDR < 0.05,])
    }
    save(test.set.edgeR.et, file="data/test.set.edgeR.et.Rdata")
    save(test.set.edgeR, file="data/test.set.edgeR.Rdata")
}

