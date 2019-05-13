# depends on setup.r

##########

# get a list of TP and FP for each N
get.tpfp <- function(ref.set, test.set, EFF, DIFF){
    ret.list <- list()

    ####
    # by effect size
    # this is all that can be a TP
    ret.list$ref.gt.cut <- lapply(ref.set, function(x) {rownames(x)[abs(x$effect) > EFF & abs(x$diff.btw) > DIFF]})

    # everything that can be a TP
    ret.list$any.ref.gt.cut <- unique(unlist(ret.list$ref.gt.cut))

    # only TP in all reference sets
    ret.list$always.ref.gt.cut <- Reduce(intersect, ret.list$ref.gt.cut)

    # should exclude the setdiff as potential TP or FP since they are by definition undefined

    # test data
    # get the ids of features that are found
    ret.list$test.gt.cut <- lapply(test.set, function(x) {lapply(x, function(y) {rownames(y)[abs(y$effect) > EFF & abs(y$diff.btw) > DIFF]} ) } )

    # features in reference and in test
    TP.2  <- lapply(ret.list$test.gt.cut, function(x) {lapply(x, function(y) {length(intersect(y, ret.list$always.ref.gt.cut))})} )
    ret.list$TP.always.ref.gt.cut  <- lapply(TP.2, unlist)

    TP.3  <- lapply(ret.list$test.gt.cut, function(x) {lapply(x, function(y) {length(intersect(y, ret.list$any.ref.gt.cut))})} )
    ret.list$TP.any.ref.gt.cut  <- lapply(TP.3, unlist)

    ## features in test not in reference
    FP.2 <- lapply(ret.list$test.gt.cut, function(x) {lapply(x, function(y) {length(setdiff(y, ret.list$any.ref.gt.cut))})} )
    ret.list$FP.any.ref.gt.cut  <- lapply(FP.2, unlist)

    ###########
    # now do the same thing for p values - I will pull out BH values less than 0.1
    ret.list$ref.p.cut <- lapply(ref.set, function(x) {rownames(x)[abs(x$we.eBH) <0.1 & abs(x$diff.btw) > DIFF]})
    # everything that can be a TP by q value
    ret.list$any.ref.p.cut <- unique(unlist(ret.list$ref.p.cut))

    # only TP in all reference sets by q value
    ret.list$always.ref.p.cut <- Reduce(intersect, ret.list$ref.p.cut)

    # test data by p value
    # get the ids of features that are found
    ret.list$test.p.cut <- lapply(test.set, function(x) {lapply(x, function(y) {rownames(y)[abs(y$we.eBH) < 0.1 & abs(y$diff.btw) > DIFF]} ) } )

    # features in reference and in test by q
    TP.q2  <- lapply(ret.list$test.p.cut, function(x) {lapply(x, function(y) {length(intersect(y, ret.list$always.ref.p.cut))})} )
    ret.list$TP.always.ref.p.cut  <- lapply(TP.q2, unlist)

    TP.q3  <- lapply(ret.list$test.p.cut, function(x) {lapply(x, function(y) {length(intersect(y, ret.list$any.ref.p.cut))})} )
    ret.list$TP.any.ref.p.cut  <- lapply(TP.q3, unlist)

    ## features in test not in reference
    FP.q2 <- lapply(ret.list$test.p.cut, function(x) {lapply(x, function(y) {length(setdiff(y, ret.list$any.ref.p.cut))})} )
    ret.list$FP.any.ref.p.cut  <- lapply(FP.q2, unlist)


    return(ret.list)
}

# general plotting function for the
plot.ret.list <- function(ret.list, main="", leg.text = c("TPR", "FPR")){

    n.samples <- max(as.numeric(names(ret.list$TP.always.ref.p.cut)))

    for(i in 1:length(ret.list$TP.always.ref.gt.cut)){

    if(i == 1){
        plot(1, type="n", xlab="", ylab="", xlim=c(2, n.samples), ylim=c(0, 1), main=main)
    }
        x.num <- as.numeric(names(ret.list$TP.always.ref.gt.cut[i]))
        TPR <- ret.list$TP.always.ref.gt.cut[[i]]/length(ret.list$always.ref.gt.cut)
        quant <- quantile(TPR, probs=c(0.025,0.975))
        points(x.num,  mean(TPR), pch=19, col="dodgerblue")
        segments(x.num, quant[1], x.num, quant[2], lwd=2, col="dodgerblue")

        FPR <- ret.list$FP.any.ref.gt.cut[[i]]/(ret.list$FP.any.ref.gt.cut[[i]] + ret.list$TP.any.ref.gt.cut[[i]])
        quant.FP <- quantile(FPR, probs=c(0.025,0.975))
        points(x.num,  mean(FPR), pch=19, col="magenta")
        segments(x.num, quant.FP[1], x.num, quant.FP[2], lwd=2, col="magenta")
    }
    abline(h=0.8, lty=2, col="grey")
    abline(h=0.1, lty=2, col="grey")
    #abline(v=15, lty=3, col="grey")
    text(n.samples-5,0.9, labels=length(ret.list$always.ref.gt.cut))

    legend(30, 0.6, legend=leg.text, pch=19, col=c("dodgerblue","magenta"))
}

plot.ret.list.q <- function(ret.list, main="", leg.text = c("TPR", "FPR")){

    n.samples <- max(as.numeric(names(ret.list$TP.always.ref.p.cut)))

    for(i in 1:length(ret.list$TP.always.ref.p.cut)){

    if(i == 1){
        plot(1, type="n", xlab="", ylab="", xlim=c(2, n.samples), ylim=c(0, 1), main=main)
    }
        x.num <- as.numeric(names(ret.list$TP.always.ref.p.cut[i]))
        TPR <- ret.list$TP.always.ref.p.cut[[i]]/length(ret.list$always.ref.p.cut)
        quant <- quantile(TPR, probs=c(0.025,0.975), na.rm=T)
        points(x.num,  mean(TPR), pch=19, col="dodgerblue")
        segments(x.num, quant[1], x.num, quant[2], lwd=2, col="dodgerblue")

        FPR <- ret.list$FP.any.ref.p.cut[[i]]/(ret.list$FP.any.ref.p.cut[[i]] + ret.list$TP.any.ref.p.cut[[i]])
        quant.FP <- quantile(FPR, probs=c(0.025,0.975), na.rm=T)
        points(x.num,  mean(FPR), pch=19, col="magenta")
        segments(x.num, quant.FP[1], x.num, quant.FP[2], lwd=2, col="magenta")
    }
    abline(h=0.8, lty=2, col="grey")
    abline(h=0.1, lty=2, col="grey")
    #abline(v=15, lty=3, col="grey")
    text(n.samples-5,0.9, labels=length(ret.list$always.ref.p.cut))

    legend(30, 0.6, legend=leg.text, pch=19, col=c("dodgerblue","magenta"))
}


plot.FP.density <- function(ret.list, ref.set, main="", pos=c(1,2,9,19), leg=F){
    library(spatstat)

    FP.2 <- unique(unlist(ret.list$test.gt.cut[[pos[1]]][c(1:100)]))
    d.2 <- density(abs(ref.set[[1]][setdiff(FP.2, ret.list$any.ref.gt.cut),"effect"]))

    FP.5 <- unique(unlist(ret.list$test.gt.cut[[pos[2]]][c(1:100)]))
    d.5 <- density(abs(ref.set[[1]][setdiff(FP.5, ret.list$any.ref.gt.cut),"effect"]))

    FP.10 <- unique(unlist(ret.list$test.gt.cut[[pos[3]]][c(1:100)]))
    d.10 <- density(abs(ref.set[[1]][setdiff(FP.10, ret.list$any.ref.gt.cut),"effect"]))

    FP.20 <- unique(unlist(ret.list$test.gt.cut[[pos[4]]][c(1:100)]))
    d.20 <- density(abs(ref.set[[1]][setdiff(FP.20, ret.list$any.ref.gt.cut),"effect"]))

    x.min <- quantile( ewcdf(d.2$x, d.2$y), probs=0.025)
    x.max <- quantile( ewcdf(d.20$x, d.20$y), probs=0.975)

    plot(d.20, main=main ,xlab="true effect", xlim=c(x.min,x.max))
    polygon(d.2,col=rgb(1,0,0,0.2), border="red")
    polygon(d.5, col=rgb(1,.5,0.1,0.2), border="orange")
    polygon(d.10, col=rgb(0.1,0,1,0.2), border="blue")
    polygon(d.20, col=rgb(0.5,0,1,0.2), border="magenta")

    leg.names <- paste("N=", names(ret.list$test.gt.cut)[pos], sep="")

    if(leg==T) legend(0,max(d.20$y), legend=leg.names, col=c("red","orange","blue","magenta"), pch=19)

}

