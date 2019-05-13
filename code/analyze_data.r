source("code/plots.r")

##########
load("data/ref.set.RData")
load("data/x.all.Rdata")
load("data/test.set.eff1.Rdata")
load("data/test.set.eff2.Rdata")
load("data/test.set.welch.Rdata")
load("data/test.set.wilcox.Rdata")

# backfill this at some point. missing first bootstrap
ref.set[[1]][[1]] <- NULL
ref.set[[2]][[1]] <- NULL
ref.set[[3]][[1]] <- NULL
ref.set[[4]][[1]] <- NULL

tp.e1 <- Reduce(intersect, ref.set$eff1)
mp.e1 <- setdiff(Reduce(union, ref.set$eff1), Reduce(intersect, ref.set$eff1))
tn.e1 <- setdiff(rownames(x.all), c(tp.e1, mp.e1))


tp.e2 <- Reduce(intersect, ref.set$eff2)
mp.e2 <- setdiff(Reduce(union, ref.set$eff2), Reduce(intersect, ref.set$eff2))
tn.e2 <- setdiff(rownames(x.all), c(tp.e2, mp.e2))

tp.we <- Reduce(intersect, ref.set$ewelch)
mp.we <- setdiff(Reduce(union, ref.set$ewelch), Reduce(intersect, ref.set$ewelch))
tn.we <- setdiff(rownames(x.all), c(tp.we, mp.we))

tp.wi <- Reduce(intersect, ref.set$ewilcox)
mp.wi <- setdiff(Reduce(union, ref.set$ewilcox), Reduce(intersect, ref.set$ewilcox))
tn.wi <- setdiff(rownames(x.all), c(tp.wi, mp.wi))

par(mfrow=c(2,2))
analyze_data(tp.e1,mp.e1,tn.e1,test.set.eff1,main="ep1", 6000, 40)
analyze_data(tp.e2,mp.e2,tn.e2,test.set.eff2,main="ep2", 6000, 40)
analyze_data(tp.we,mp.we,tn.we,test.set.welch,main="welch's", 6000, 40)
analyze_data(tp.wi,mp.wi,tn.wi,test.set.wilcox,main="wilcox", 6000, 40)


# #### this needs work to get the proportions correct
# analyze_data.prop <- function(tp,mp,tn,data.set) {
#     plot(1, type="n", xlab="", ylab="", xlim=c(1, 3), ylim=c(0, 1))
#
#     for(i in 2:3){
#     # this needs to be a for loop or nested lapply when the full dataset is collected
#     TP.dist <- sapply(data.set[[i]], function(x) length(intersect(x,tp))) #/ length(tp)
#     MP.dist <- sapply(data.set[[i]], function(x) length(intersect(x,mp))) #/ length(mp)
#     FP.dist <- sapply(data.set[[i]], function(x) length(intersect(x,tn))) #/ length(tn)
#     FN.dist <- sapply(data.set[[i]], function(x) length( setdiff(c(tp,mp),  (intersect(x,c(tp,mp))) )) )
#     TN.dist <- length(c(tp, mp, tn)) - (TP.dist+MP.dist+FP.dist+FN.dist)
#
#     points(i,median(TP.dist/ length(tp)), ylim=c(0,1), pch=19)
#     segments(i,quantile(TP.dist/ length(tp), probs=c(0.025, 0.975)[1]), i, quantile(TP.dist/ length(tp), probs=c(0.025, 0.975)[2]), lwd=2, col=rgb(0,0,0,0.3))
#     points(i,median(MP.dist/ length(mp)),  pch=19, col="blue")
#     segments(i,quantile(MP.dist/ length(mp), probs=c(0.025, 0.975)[1]), i, quantile(MP.dist/ length(mp), probs=c(0.025, 0.975)[2]), lwd=2, col=rgb(0,0,1,0.3))
#     points(i,median(FP.dist/length(tn)),  pch=19, col="red")
#     segments(i,quantile(FP.dist/length(tn), probs=c(0.025, 0.975)[1]), i, quantile(FP.dist/length(tn), probs=c(0.025, 0.975)[2]), lwd=2, col=rgb(1,0,0,0.3))
#     points(i,median(TN.dist/length(tn)),  pch=19, col="green")
#     segments(i,quantile(TN.dist/length(tn), probs=c(0.025, 0.975)[1]), i, quantile(TN.dist/length(tn), probs=c(0.025, 0.975)[2]), lwd=2, col=rgb(0,1,0,0.3))
#     points(i,median(FN.dist),  pch=19, col="magenta")
#     segments(i,quantile(FN.dist, probs=c(0.025, 0.975)[1]), i, quantile(FN.dist, probs=c(0.025, 0.975)[2]), lwd=2, col=rgb(1,0,1,0.3))
#     }
#     legend(1,1, legend=c("TP", "MP","TN","FP","FN"), pch=19, col=c("black","blue","green","red","magenta"))
#     abline(h=0,lty=2, col="grey")
# }
#
