#######
# DIFFERENCE
#######

######
# how many variables are needed to converge on the real difference of 1?
# for normally distributed, cauchy and random uniform
# the mean difference is identical to the difference of means
# the Median of the vector difference is slightly less precise
# than the mean, but is robuts to any distribution
######
# two functions to generate 100 random instances of different distibutions
# for plotting

b.data <- function(fun, size, disn){
    data <- vector()
        for(i in 1:1000){
        a <- disn(size, 0, 1)
        b <- disn(size, 1, 1)
        ab <- b-a
        data[i] <- fun(ab) -1
    }
    return(data)
}
b.data.ru <- function(fun, size, disn){
    data <- vector()
        for(i in 1:1000){
        a <- disn(size, 0, 1)
        b <- disn(size, 1, 2)
        ab <- b-a
        data[i] <- fun(ab) - 1
    }
    return(data)
}

par(mfrow=c(1,3))
# plot of residuals from the true difference
boxplot(list(b.data(mean,10,rnorm), b.data(median,10,rnorm),
   b.data(median,100,rnorm), b.data(median,100,rnorm),
   b.data(median,1000,rnorm), b.data(median,1000,rnorm)),ylim=c(-2,2),
   main="Normal distribution", names=c("10", "10", "100", "100",
   "1000", "1000"), col=c("red", "blue", "red","blue","red","blue"),
   border=c("red", "blue", "red","blue","red","blue"))
abline(h=0, lty=2, col="grey")
legend("topright",legend=c("Mean", "Median"), pch=18, col=c("red","blue"))

boxplot(list(b.data(mean,10,rcauchy), b.data(median,10,rcauchy),
   b.data(mean,100,rcauchy), b.data(median,100,rcauchy),
   b.data(mean,1000,rcauchy), b.data(median,1000,rcauchy)), ylim=c(-5,5),
   main="Cauchy distribution", names=c("10", "10", "100", "100",
   "1000", "1000"), col=c("red", "blue", "red","blue","red","blue"),
   border=c("red", "blue", "red","blue","red","blue"))
abline(h=0, lty=2, col="grey")

legend("topright",legend=c("Mean", "Median"), pch=18, col=c("red","blue"))
boxplot(list(b.data.ru(mean,10,runif), b.data.ru(median,10,runif),
   b.data.ru(mean,100,runif), b.data.ru(median,100,runif),
   b.data.ru(mean,1000,runif), b.data.ru(median,1000,runif)), ylim=c(-1,1),
   main="Uniform distribution", names=c("10", "10", "100", "100",
   "1000", "1000"), col=c("red", "blue", "red","blue","red","blue"),
   border=c("red", "blue", "red","blue","red","blue"))
abline(h=0, lty=2, col="grey")
legend("topright",legend=c("Mean", "Median"), pch=18, col=c("red","blue"))

# MAD vs Med(maximum absolute deviation)
# this is vectorized and much faster
mmad <- function(x,y){
  pmax(abs(x - sample(x)), abs(y - sample(y)))
}

### variance by sd (valid normal) or
sda <- sd(a)
sdb <- sd(b)
sdx <- sd(x)
sdy <- sd(y)
ma <- mmad(a)
mb <- mmad(b)
mx <- mmad(x)
my <- mmad(y)

cohen_ab <- (mean(a) - mean(b))/sqrt((sda^2 + sdb^2)/2)
cohen_xy <- (mean(x) - mean(y))/sqrt((sdx^2 + sdy^2)/2)

effect_ab <- (a - b) / mmad(a,b)
effect_xy <- (x - y) / mmad(x,y)

mad_ab <- (mean(a)-mean(b)) / ((mad(a) + mad(b))/2)
mad_xy <- (mean(x)-mean(y)) / ((mad(x) + mad(y))/2)


################
## DISPERSION
################

########
# efficiency of the dispersion measure
# efficiency of mad ==37% as expected
# efficiency of mmad = 52%
########

### first estimate the scaling factor
# the number of samples that are needed to achieve an variance precision of the normal variance
# multiple tests suggest a factor of  1.418339 +/- 9.585138e-05 is the expected scale
ntest =100000
dist.size = 1000

scale.mat <-  matrix(data=NA, nrow=ntest, ncol=2)

colnames(scale.mat) = c("mmad", "sd")

for(i in 1:ntest){
a <- rnorm(dist.size, 0, 1)
scale.mat[i,1] <- median(mmad(a,a))
scale.mat[i,2] <- sd(a)
}
# scale factor
mean(scale.mat[,1]/scale.mat[,2])
# standard error
sd(scale.mat[,1]/scale.mat[,2])/ sqrt(nrow(scale.mat))

### then estimate efficiency

eff <- matrix(data=NA, nrow=100, ncol=2)
colnames(eff) <- c("eff_mmad", "eff_mad")
for(j in 1:100){
ntest =1000
dist.size = 1000

diff.mat <-  matrix(data=NA, nrow=ntest, ncol=3)

colnames(diff.mat) = c("mmad", "sd", "mad")

for(i in 1:ntest){
a <- rnorm(dist.size, 0, 1)
diff.mat[i,1] <- median(mmad(a,a)) / 1.4183
diff.mat[i,2] <- sd(a)
diff.mat[i,3] <- mad(a)
}

boxplot(diff.mat)
abline(h=1, lty=2, col="grey")

eff[j,1] <- 1/ (var(diff.mat[,1])/var(diff.mat[,2]))
eff[j,2] <- 1/ (var(diff.mat[,3])/var(diff.mat[,2]))
}
# expected eff of mad
mean(eff[,2])
# expected eff of mmad
mean(eff[,2])

########
# BREAKDOWN
# breakdown of mad is 50% as expected
# breakdown of mmad is 20% :-(, can push to 35% if use first
# quartile rather than median
# it is the maximum that is the problem here
########
dist.size = 1000

brk <- matrix(data=NA, ncol=6, nrow=dist.size)
colnames(brk) <- c("orig", "contam", "sd_orig", "sd_contam", "mad_orig", "mad_contam")

for(i in 1:dist.size){
c <- rnorm(dist.size, 0, 1)
a <- rep(1, dist.size)
b <- a
b[1:i] <- c[1:i]

brk[i,1] <- median(mmad(a,a)) / 1.4184
brk[i,2] <- median(mmad(b,b)) / 1.4184
brk[i,3] <- sd(a)
brk[i,4] <- sd(b)
brk[i,5] <- mad(a)
brk[i,6] <- mad(b)
}

par(mfrow=c(1,2))
plot(brk[,2] - brk[1], ylab ="contam - orig", xlab="N contam", main="mmad", log="y")
abline(v=2000)
plot(brk[,4] - brk[3], ylab ="contam - orig", xlab="N contam", main="SD", log="y")
abline(v=1)
#plot(brk[,6] - brk[5], ylab ="contam - orig", xlab="N contam", main="mad", log="y")
#abline(v=5000)


########
ntest =1000
dist.size = 1000

test.dist <- c(normal, uniform, bimodal.normal, bimodal.uniform)

out <- matrix(data=NA, nrow=ntest, ncol=3)
colnames(out) = c("mmad", "sd", "mad")

norm.dist <- c(1,1, 1,1, 1,1)
bimod.dist <- c(1,1, 2,4, 4,1)
cauchy.dist <- c(1,1, 1,1, 1,1)

dist.t <- cbind( norm.dist, bimod.dist, cauchy.dist)

col = 2 #dist to test
adjust <- c(1,0,4,0,9,0) # increment to mean and sd for comparison set

 for(i in 1:ntest){
    # lets mix a few distributions
    y <- dist.t[,col]
    z <- y + adjust

    abc <- cbind(rcauchy(dist.size, y[1], y[2]),
      rnorm(dist.size, y[3] ,y[4]), rnorm(dist.size, y[5],y[6]))

    def <- cbind(rcauchy(dist.size, z[1], z[2]),
      rnorm(dist.size, z[3] ,z[4]), rnorm(dist.size, z[5],z[6]))

    abc.c <- c(abc)
    def.c <- c(def)

    #hist(abc.c, breaks=99, ylim=c(0,120))
    #hist(def.c, breaks=99, add=T, col=rgb(0,0,0,0.2))
    out[i,1] <- median( (abc.c - def.c)/ (mmad(abc.c,def.c)) )
    out[i,2] <- (mean(abc.c) -mean(def.c)) / sqrt((var(abc.c) + var(def.c))/2)
    out[i,3] <- (mean(abc.c) -mean(def.c)) / ((mad(abc.c) + mad(def.c))/2)
}
par(mfrow=c(1,3))
hist(abc.c, breaks=99, ylim=c(0,120), xlim=c(min(abc.c), max(def.c)), col=rgb(1,0,0,0.2))
hist(def.c, breaks=99, add=T, col=rgb(0,0,0,0.2))

hist(out[,1]/out[,2], breaks=99)
hist(out[,1]/out[,3], breaks=99)
median(out[,1]/out[,2])
mean(out[,1]/out[,2])
median(out[,1]/out[,3])
mean(out[,1]/out[,3])
var(out[,1])/var(out[,2]) #
#######

##########
# Fe vs Cohen's d for Normal and Cauchy Distribution
##########
test.out <- matrix(data=NA, nrow=1000, ncol=4)
colnames(test.out) <- c("FeN", "CdN", "FeC", "CdC")

for(i in 1:1000){
    a <- rcauchy(1000, 3, 1)
    b <- rcauchy(1000, 1, 1)

    c <- rnorm(1000, 3, 1)
    d <- rnorm(1000, 1, 1)

    test.out[i,3] <- median( (a - b)/ (mmad(a,b)) )
    test.out[i,4] <- (mean(a) -mean(b)) / sqrt((var(a) + var(b))/2)
    test.out[i,1] <- median( (c - d)/ (mmad(c,d)) )
    test.out[i,2] <- (mean(c) -mean(d)) / sqrt((var(c) + var(d))/2)
}
par(mfrow=c(2,2))
hist(test.out[,2], main="Cd Normal", breaks=99, xlab="Difference")
hist(test.out[,1], main="Fe Normal", breaks=99, xlab="Difference")
hist(test.out[,4], main="Cd Cauchy", breaks=99, xlab="Difference")
hist(test.out[,3], main="Fe Cauchy", breaks=99, xlab="Difference")

######### Breakdown
ntest =1000
dist.size = 100

test.dist <- c(normal, uniform, bimodal.normal, bimodal.uniform)

out <- matrix(data=NA, nrow=ntest, ncol=3)
colnames(out) = c("mmad", "sd", "mad")

norm.dist <- c(1,1, 1,1, 1,1)
bimod.dist <- c(1,2, 2,4, 4,1)

dist.t <- cbind(bimod.dist, norm.dist)

col = 2 #dist to test
adjust <- c(3,0,3,0,3,0) # increment to mean and sd for comparison set

for(i in 1:ntest){
# lets mix a few distributions
y <- dist.t[,col]
z <- y + adjust

abc <- cbind(rnorm(dist.size, y[1], y[2]),
  rnorm(dist.size, y[3] ,y[4]), rnorm(dist.size, y[5],y[6]))

abc.c <- c(abc)
def.c <- abc.c
def.c[1:100] <- 100

out[i,1] <- mean( (abc.c - def.c)/(mmad(abc.c,def.c)) )
out[i,2] <- (mean(abc.c) -mean(def.c)) / ((sd(abc.c) + sd(def.c))/2)
out[i,3] <- (mean(abc.c) -mean(def.c)) / ((mad(abc.c) + mad(def.c))/2)
}


