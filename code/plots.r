fdr.plot <- function(tp, tn, data.set, main="", limit, legx, legy){
    plot(1, type="n", xlab="", ylab="", xlim=c(1, limit), ylim=c(0, 1))
    med.mat <- matrix(data=NA, nrow=limit, ncol=5)
    mtext(side=3,line=0.2,font=1, cex=1,main)
    mtext(side=2,line=2.5,font=1, cex=0.8,"Proportion")
    mtext(side=1,line=2.5,font=1, cex=0.8, "N samples")

   # this needs to be a for loop or nested lapply when the full dataset is collected
   for(i in 2:limit){
        TP.dist <- sapply(data.set[[i]], function(x) length(intersect(x,tp))) #/ length(tp)
        FP.dist <- sapply(data.set[[i]], function(x) length(intersect(x,tn))) #/ length(tn)

        FN.dist <- sapply(data.set[[i]], function(x) length( setdiff(tp,  (intersect(x,tp)) )) )

        TN.dist <- length(c(tp, tn)) - (TP.dist+FP.dist+FN.dist)

        tpr.dist <- (TP.dist) / (TP.dist + FN.dist)
        fpr.dist <- (FP.dist) / (FP.dist + TN.dist)
        fnr.dist <- (FN.dist) / (TP.dist + FN.dist)
        fdr.dist <- (FP.dist) / (TP.dist + FP.dist )

        acc.dist <- (TP.dist + TN.dist) / (TP.dist + FP.dist + TN.dist + FN.dist)

        segments(i,quantile(tpr.dist, probs=c(0.025, 0.975)[1], na.rm=T), i, quantile(tpr.dist, probs=c(0.025, 0.975)[2], na.rm=T), lwd=2, col=rgb(0,0,1,0.3))
        segments(i,quantile(fpr.dist, probs=c(0.025, 0.975)[1], na.rm=T), i, quantile(fpr.dist, probs=c(0.025, 0.975)[2], na.rm=T), lwd=2, col=rgb(1,0,0,0.3))
        segments(i,quantile(fdr.dist, probs=c(0.025, 0.975)[1], na.rm=T), i, quantile(fdr.dist, probs=c(0.025, 0.975)[2], na.rm=T), lwd=2, col=rgb(0,0,0,0.3))

        segments(i,quantile(acc.dist, probs=c(0.025, 0.975)[1], na.rm=T), i, quantile(acc.dist, probs=c(0.025, 0.975)[2], na.rm=T), lwd=2, col=rgb(0.8,0.4,0.1,0.3))

        med.mat[i,] <- c(median(tpr.dist), median(fnr.dist), median(fpr.dist),median(fdr.dist),median(acc.dist))
    }
    points(2:limit,med.mat[2:limit,1],  pch=21, type="b", cex=0.5, col="blue",bg="white") # tpr
    points(2:limit,med.mat[2:limit,3],  pch=21, type="b", cex=0.5, col="red",bg="white") # fpr
    points(2:limit,med.mat[2:limit,4],  pch=21, type="b", cex=0.5   , col="black",bg="white") # fdr
    points(2:limit,med.mat[2:limit,5],  pch=21, type="b", cex=0.5   , col="orange",bg="white") # fdr

    legend(legx, legy, legend=c("tpr","fpr", "fdr", "acc"), pch=21, col=c("blue","red", "black","orange"))
    abline(h=0,lty=2, col="grey")


}


analyze_data <- function(tp,mp,tn,data.set, main="", size, limit) {
    med.mat <- matrix(data=NA, nrow=limit, ncol=5)

    plot(1, type="n", xlab="", ylab="", xlim=c(1, limit), ylim=c(0, size), main=main)

    for(i in 2:limit){
        # this needs to be a for loop or nested lapply when the full dataset is collected
        TP.dist <- sapply(data.set[[i]], function(x) length(intersect(x,tp))) #/ length(tp)
        MP.dist <- sapply(data.set[[i]], function(x) length(intersect(x,mp))) #/ length(mp)
        FP.dist <- sapply(data.set[[i]], function(x) length(intersect(x,tn))) #/ length(tn)

        FN.dist <- sapply(data.set[[i]], function(x) length( setdiff(c(tp,mp),  (intersect(x,c(tp,mp))) )) )

        TN.dist <- length(c(tp, mp, tn)) - (TP.dist+MP.dist+FP.dist+FN.dist)

        segments(i,quantile(TP.dist, probs=c(0.025, 0.975)[1]), i, quantile(TP.dist, probs=c(0.025, 0.975)[2]), lwd=2, col=rgb(0,0,0,0.3))
        segments(i,quantile(MP.dist, probs=c(0.025, 0.975)[1]), i, quantile(MP.dist, probs=c(0.025, 0.975)[2]), lwd=2, col=rgb(0,0,1,0.3))
        segments(i,quantile(FP.dist, probs=c(0.025, 0.975)[1]), i, quantile(FP.dist, probs=c(0.025, 0.975)[2]), lwd=2, col=rgb(1,0,0,0.3))
        segments(i,quantile(TN.dist, probs=c(0.025, 0.975)[1]), i, quantile(TN.dist, probs=c(0.025, 0.975)[2]), lwd=2, col=rgb(0,1,0,0.3))
        segments(i,quantile(FN.dist, probs=c(0.025, 0.975)[1]), i, quantile(FN.dist, probs=c(0.025, 0.975)[2]), lwd=2, col=rgb(1,0,1,0.3))

        med.mat[i,] <- c(median(TP.dist), median(MP.dist), median(FP.dist),median(TN.dist),median(FN.dist))

    }
    points(2:limit,med.mat[2:limit,1],  pch=21, type="b", col="black",bg="white")
    points(2:limit,med.mat[2:limit,2],  pch=21, type="b", col="blue",bg="white")
    points(2:limit,med.mat[2:limit,3],  pch=21, type="b", col="red",bg="white")
    points(2:limit,med.mat[2:limit,4],  pch=21, type="b", col="green",bg="white")
    points(2:limit,med.mat[2:limit,5],  pch=21, type="b", col="magenta",bg="white")



    legend(1,(0.95 * size), legend=c("TP", "MP","TN","FP","FN"), pch=21, col=c("black","blue","green","red","magenta"))
    abline(h=0,lty=2, col="grey")

}

sens_spec <- function(tp,mp,tn,data.set, main="", limit){
    med.mat <- matrix(data=NA, nrow=limit, ncol=5)

    plot(1, type="n", xlab="", ylab="", xlim=c(1, limit), ylim=c(0, 1), main=main)

    for(i in 2:limit){
        # this needs to be a for loop or nested lapply when the full dataset is collected
        TP.dist <- sapply(data.set[[i]], function(x) length(intersect(x,tp))) #/ length(tp)
        MP.dist <- sapply(data.set[[i]], function(x) length(intersect(x,mp))) #/ length(mp)
        FP.dist <- sapply(data.set[[i]], function(x) length(intersect(x,tn))) #/ length(tn)

        FN.dist <- sapply(data.set[[i]], function(x) length( setdiff(c(tp,mp),  (intersect(x,c(tp,mp))) )) )

        TN.dist <- length(c(tp, mp, tn)) - (TP.dist+MP.dist+FP.dist+FN.dist)

        tpr.dist <- (TP.dist) / (TP.dist + FN.dist)
        fpr.dist <- (FP.dist) / (FP.dist + TN.dist)
        fnr.dist <- (FN.dist) / (TP.dist + FN.dist)
        fdr.dist <- (FP.dist) / (TP.dist + FP.dist + 0.1)

        segments(i,quantile(tpr.dist, probs=c(0.025, 0.975)[1]), i, quantile(tpr.dist, probs=c(0.025, 0.975)[2], na.rm=T), lwd=2, col=rgb(0,0,0,0.3))
        segments(i,quantile(fnr.dist, probs=c(0.025, 0.975)[1]), i, quantile(fnr.dist, probs=c(0.025, 0.975)[2], na.rm=T), lwd=2, col=rgb(0,0,1,0.3))
        segments(i,quantile(fpr.dist, probs=c(0.025, 0.975)[1]), i, quantile(fpr.dist, probs=c(0.025, 0.975)[2], na.rm=T), lwd=2, col=rgb(1,0,0,0.3))
        segments(i,quantile(fdr.dist, probs=c(0.025, 0.975)[1]), i, quantile(fdr.dist, probs=c(0.025, 0.975)[2], na.rm=T), lwd=2, col=rgb(0,1,0,0.3))

        med.mat[i,] <- c(median(tpr.dist), median(fnr.dist), median(fpr.dist),median(fdr.dist),median(fdr.dist))
    }
    points(2:limit,med.mat[2:limit,1],  pch=21, type="b", col="black",bg="white")
    points(2:limit,med.mat[2:limit,2],  pch=21, type="b", col="blue",bg="white")
    points(2:limit,med.mat[2:limit,3],  pch=21, type="b", col="red",bg="white")
    points(2:limit,med.mat[2:limit,4],  pch=21, type="b", col="green",bg="white")

    legend(1,1, legend=c("tpr", "fnr","fpr", "fdr"), pch=21, col=c("black","blue", "red", "green"))
    abline(h=0,lty=2, col="grey")

}


