# Data for Figure 1 for the paper
### takes about 1/2 hour at 100000 iterations
### takes a minute or less at 1000 iterations
library(CoDaSeq)

cohens.d <- function(dataset, size){
    apply(dataset, 1, function(x) (mean(x[1:size]) - mean(x[seq(from=size+1, to=size*2)])) / sqrt((var(x[1:size+1]) + var(x[seq(from=size+1, to=size*2)]))/2))
}
list.null <- vector()
list.null$rnorm.null.effect <- matrix(data=NA, nrow=39, ncol=8)
list.null$runif.null.effect <- matrix(data=NA, nrow=39, ncol=8)
list.null$rbeta.null.effect <- matrix(data=NA, nrow=39, ncol=8)
list.null$rcauchy.null.effect <- matrix(data=NA, nrow=39, ncol=8)

list.null$rnorm.cohen.null.effect <- matrix(data=NA, nrow=39, ncol=8)
list.null$runif.cohen.null.effect <- matrix(data=NA, nrow=39, ncol=8)
list.null$rbeta.cohen.null.effect <- matrix(data=NA, nrow=39, ncol=8)
list.null$rcauchy.cohen.null.effect <- matrix(data=NA, nrow=39, ncol=8)

for(i in 2:40){
size <- i
tests <- 100000

test.mat.rnorm <- matrix(data=rnorm(size*2*tests), nrow=tests, ncol=size*2)
test.mat.runif <- matrix(data=runif(size*2*tests), nrow=tests, ncol=size*2)
test.mat.rbeta <- matrix(data=rbeta(size*2*tests, 1,5), nrow=tests, ncol=size*2)
test.mat.rcauchy <- matrix(data=rcauchy(size*2*tests), nrow=tests, ncol=size*2)

conds <- c(rep("A",size), rep("B",size))

list.null$rnorm.null.effect[i-1,] <- c(i, quantile(codaSeq.effect(test.mat.rnorm, conds), probs=c(0.005,0.025,0.05, 0.5, 0.95,0.975,0.995)))
list.null$runif.null.effect[i-1,] <- c(i, quantile(codaSeq.effect(test.mat.runif, conds), probs=c(0.005,0.025,0.05, 0.5, 0.95,0.975,0.995)))
list.null$rbeta.null.effect[i-1,] <- c(i, quantile(codaSeq.effect(test.mat.rbeta, conds), probs=c(0.005,0.025,0.05, 0.5, 0.95,0.975,0.995)))
list.null$rcauchy.null.effect[i-1,] <- c(i, quantile(codaSeq.effect(test.mat.rcauchy, conds), probs=c(0.005,0.025,0.05, 0.5, 0.95,0.975,0.995)))
list.null$rnorm.cohen.null.effect[i-1,] <- c(i, quantile(cohens.d(test.mat.rnorm, size), probs=c(0.005,0.025,0.05, 0.5, 0.95,0.975,0.995)))
list.null$runif.cohen.null.effect[i-1,] <- c(i, quantile(cohens.d(test.mat.runif, size), probs=c(0.005,0.025,0.05, 0.5, 0.95,0.975,0.995)))
list.null$rbeta.cohen.null.effect[i-1,] <- c(i, quantile(cohens.d(test.mat.rbeta, size), probs=c(0.005,0.025,0.05, 0.5, 0.95,0.975,0.995)))
list.null$rcauchy.cohen.null.effect[i-1,] <- c(i, quantile(cohens.d(test.mat.rcauchy, size), probs=c(0.005,0.025,0.05, 0.5, 0.95,0.975,0.995)))
}

save(list.null, file="data/list.null.Rdata")


#### True difference of 1
# single-side overlap of distributions of 1/6
# this moves the location by 1 sd unit in a Normal distribution

# quantile(rnorm(100000), 1/3) = 0.96 or approximately 1
# quantile(runif(100000), 1/3) = .333
# quantile(rbeta(100000, 1,5), 1/3) = 0.078
# quantile(rcauchy(100000), 1/3) = 1.83
list.one$rnorm.diff1.effect <- matrix(data=NA, nrow=39, ncol=8)
list.one$runif.diff1.effect <- matrix(data=NA, nrow=39, ncol=8)
list.one$rbeta.diff1.effect <- matrix(data=NA, nrow=39, ncol=8)
list.one$rcauchy.diff1.effect <- matrix(data=NA, nrow=39, ncol=8)
list.one$rnorm.cohen1.effect <- matrix(data=NA, nrow=39, ncol=8)
list.one$runif.cohen1.effect <- matrix(data=NA, nrow=39, ncol=8)
list.one$rbeta.cohen1.effect <- matrix(data=NA, nrow=39, ncol=8)
list.one$rcauchy.cohen1.effect <- matrix(data=NA, nrow=39, ncol=8)

for(i in 2:40){
size <- i
tests <- 100000

n1 <- matrix(data=rnorm(size*tests), nrow=tests, ncol=size)
n2 <- matrix(data=rnorm(size*tests, mean=1), nrow=tests, ncol=size)
test.mat.rnorm <- cbind(n2,n1)

c1 <- matrix(data=rcauchy(size*tests), nrow=tests, ncol=size)
c2 <- matrix(data=rcauchy(size*tests, location=-2.2), nrow=tests, ncol=size)
test.mat.rcauchy <- cbind(c1,c2)

r1 <- matrix(data=runif(size*tests, min=0, max=1), nrow=tests, ncol=size)
r2 <- matrix(data=runif(size*tests, min=.333, max=1.333), nrow=tests, ncol=size)
test.mat.runif <- cbind(r2,r1)

b1 <- matrix(data=rbeta(size*tests, 1,5), nrow=tests, ncol=size)
b2 <- matrix(data=rbeta(size*tests, 1,5)-0.116, nrow=tests, ncol=size)
test.mat.rbeta <- cbind(b1,b2)

conds <- c(rep("A",size), rep("B",size))

list.one$rnorm.diff1.effect[i-1,] <- c(i, quantile(codaSeq.effect(test.mat.rnorm, conds), probs=c(0.005,0.025,0.05, 0.5, 0.95,0.975,0.995)))
list.one$runif.diff1.effect[i-1,] <- c(i, quantile(codaSeq.effect(test.mat.runif, conds), probs=c(0.005,0.025,0.05, 0.5, 0.95,0.975,0.995)))
list.one$rbeta.diff1.effect[i-1,] <- c(i, quantile(codaSeq.effect(test.mat.rbeta, conds), probs=c(0.005,0.025,0.05, 0.5, 0.95,0.975,0.995)))
list.one$rcauchy.diff1.effect[i-1,] <- c(i, quantile(codaSeq.effect(test.mat.rcauchy, conds), probs=c(0.005,0.025,0.05, 0.5, 0.95,0.975,0.995)))

list.one$rnorm.cohen1.effect[i-1,] <- c(i, quantile(cohens.d(test.mat.rnorm, size), probs=c(0.005,0.025,0.05, 0.5, 0.95,0.975,0.995)))
list.one$runif.cohen1.effect[i-1,] <- c(i, quantile(cohens.d(test.mat.runif, size), probs=c(0.005,0.025,0.05, 0.5, 0.95,0.975,0.995)))
list.one$rbeta.cohen1.effect[i-1,] <- c(i, quantile(cohens.d(test.mat.rbeta, size), probs=c(0.005,0.025,0.05, 0.5, 0.95,0.975,0.995)))
list.one$rcauchy.cohen1.effect[i-1,] <- c(i, quantile(cohens.d(test.mat.rcauchy, size), probs=c(0.005,0.025,0.05, 0.5, 0.95,0.975,0.995)))

}

save(list.one, file="data/list.one.Rdata")
