load("data/ref.set.yeast.RData")
# effect > 2
list.gt.2 <- lapply(ref.set.yeast, function(x) {rownames(x)[abs(x$effect) >2]})
unique.gt.2 <- unique(unlist(list.gt.2))

# effect > 2 & diff > 1
list.gt.2.2 <- lapply(ref.set.yeast, function(x) {rownames(x)[abs(x$effect) >2 & abs(x$diff.btw) > 1]})
unique.gt.2.2 <- unique(unlist(list.gt.2.2))


# effect > 1
list.gt.1 <- lapply(ref.set.yeast, function(x) {rownames(x)[abs(x$effect) >1]})
unique.gt.1 <- unique(unlist(list.gt.1))

# effect > 1 & diff > 1
list.gt.1.2 <- lapply(ref.set.yeast, function(x) {rownames(x)[abs(x$effect) >1 & abs(x$diff.btw) > 1]})
unique.gt.1.2 <- unique(unlist(list.gt.1.2))


# we.eBH < 0.05
list.lt.05 <- lapply(ref.set.yeast, function(x) {rownames(x)[x$we.eBH <0.05]})
unique.lt.05 <- unique(unlist(list.lt.05))

# we.eBH < 0.05 & diff > 1
list.lt.05.2 <- lapply(ref.set.yeast, function(x) {rownames(x)[x$we.eBH <0.05  & abs(x$diff.btw) > 1]})
unique.lt.05.2 <- unique(unlist(list.lt.05.2))

par(mfrow=c(1,2))
# effect plot
plot(ref.set.yeast[[1]]$diff.win, ref.set.yeast[[1]]$diff.btw, pch=19, col=rgb(0,0,0,0.2))

points(ref.set.yeast[[1]][unique.lt.05,"diff.win"], ref.set.yeast[[1]][unique.lt.05,"diff.btw"], pch=19, col="dodgerblue3")

points(ref.set.yeast[[1]][unique.lt.05.2,"diff.win"], ref.set.yeast[[1]][unique.lt.05.2,"diff.btw"], pch=19, col="blue")

points(ref.set.yeast[[1]][unique.gt.1,"diff.win"], ref.set.yeast[[1]][unique.gt.1,"diff.btw"], pch=19, col="orange")

points(ref.set.yeast[[1]][unique.gt.1.2,"diff.win"], ref.set.yeast[[1]][unique.gt.1.2,"diff.btw"], pch=19, col="orange4")

points(ref.set.yeast[[1]][unique.gt.2,"diff.win"], ref.set.yeast[[1]][unique.gt.2,"diff.btw"], pch=19, col="red")

points(ref.set.yeast[[1]][unique.gt.2.2,"diff.win"], ref.set.yeast[[1]][unique.gt.2.2,"diff.btw"], pch=19, col="red4")

abline(h=c(-1,1), lty=2)
abline(0,-2, lty=3)
abline(0,2, lty=3)
abline(0,-1, lty=4)
abline(0,1, lty=4)

# volcano plot
plot(ref.set.yeast[[1]]$we.eBH, ref.set.yeast[[1]]$diff.btw, pch=19, col=rgb(0,0,0,0.2), log="x")

points(ref.set.yeast[[1]][unique.lt.05,"we.eBH"], ref.set.yeast[[1]][unique.lt.05,"diff.btw"], pch=19, col="dodgerblue3")

points(ref.set.yeast[[1]][unique.lt.05.2,"we.eBH"], ref.set.yeast[[1]][unique.lt.05.2,"diff.btw"], pch=19, col="blue")

points(ref.set.yeast[[1]][unique.gt.1,"we.eBH"], ref.set.yeast[[1]][unique.gt.1,"diff.btw"], pch=19, col="orange")
points(ref.set.yeast[[1]][unique.gt.1.2,"we.eBH"], ref.set.yeast[[1]][unique.gt.1.2,"diff.btw"], pch=19, col="orange4")

points(ref.set.yeast[[1]][unique.gt.2,"we.eBH"], ref.set.yeast[[1]][unique.gt.2,"diff.btw"], pch=19, col="red")

points(ref.set.yeast[[1]][unique.gt.2.2,"we.eBH"], ref.set.yeast[[1]][unique.gt.2.2,"diff.btw"], pch=19, col="red4")

abline(h=c(-1,1), lty=2)
abline(v=0.05, lty=3)

#######16S reference
load("ref.set.ty.RData")
# effect > 2
list.gt.2 <- lapply(ref.set.ty, function(x) {rownames(x)[abs(x$effect) >2]})
unique.gt.2 <- unique(unlist(list.gt.2))

# effect > 2 & diff > 1
list.gt.2.2 <- lapply(ref.set.ty, function(x) {rownames(x)[abs(x$effect) >2 & abs(x$diff.btw) > 1]})
unique.gt.2.2 <- unique(unlist(list.gt.2.2))


# effect > 1
list.gt.1 <- lapply(ref.set.ty, function(x) {rownames(x)[abs(x$effect) >1]})
unique.gt.1 <- unique(unlist(list.gt.1))

# effect > 1 & diff > 1
list.gt.1.2 <- lapply(ref.set.ty, function(x) {rownames(x)[abs(x$effect) >1 & abs(x$diff.btw) > 1]})
unique.gt.1.2 <- unique(unlist(list.gt.1.2))


# we.eBH < 0.05
list.lt.05 <- lapply(ref.set.ty, function(x) {rownames(x)[x$we.eBH <0.05]})
unique.lt.05 <- unique(unlist(list.lt.05))

# we.eBH < 0.05 & diff > 1
list.lt.05.2 <- lapply(ref.set.ty, function(x) {rownames(x)[x$we.eBH <0.05  & abs(x$diff.btw) > 1]})
unique.lt.05.2 <- unique(unlist(list.lt.05.2))

par(mfrow=c(1,2))
# effect plot
plot(ref.set.ty[[1]]$diff.win, ref.set.ty[[1]]$diff.btw, pch=19, col=rgb(0,0,0,0.2), xlim=c(0.2,6))

points(ref.set.ty[[1]][unique.lt.05,"diff.win"], ref.set.ty[[1]][unique.lt.05,"diff.btw"], pch=19, col="dodgerblue3")

points(ref.set.ty[[1]][unique.lt.05.2,"diff.win"], ref.set.ty[[1]][unique.lt.05.2,"diff.btw"], pch=19, col="blue")

points(ref.set.ty[[1]][unique.gt.1,"diff.win"], ref.set.ty[[1]][unique.gt.1,"diff.btw"], pch=19, col="orange")

points(ref.set.ty[[1]][unique.gt.1.2,"diff.win"], ref.set.ty[[1]][unique.gt.1.2,"diff.btw"], pch=19, col="orange4")

points(ref.set.ty[[1]][unique.gt.2,"diff.win"], ref.set.ty[[1]][unique.gt.2,"diff.btw"], pch=19, col="red")

points(ref.set.ty[[1]][unique.gt.2.2,"diff.win"], ref.set.ty[[1]][unique.gt.2.2,"diff.btw"], pch=19, col="red4")

abline(h=c(-1,1), lty=2)
abline(0,-2, lty=3)
abline(0,2, lty=3)
abline(0,-1, lty=4)
abline(0,1, lty=4)

# volcano plot
plot(ref.set.ty[[1]]$we.eBH, ref.set.ty[[1]]$diff.btw, pch=19, col=rgb(0,0,0,0.2), log="x")

points(ref.set.ty[[1]][unique.lt.05,"we.eBH"], ref.set.ty[[1]][unique.lt.05,"diff.btw"], pch=19, col="dodgerblue3")

points(ref.set.ty[[1]][unique.lt.05.2,"we.eBH"], ref.set.ty[[1]][unique.lt.05.2,"diff.btw"], pch=19, col="blue")

points(ref.set.ty[[1]][unique.gt.1,"we.eBH"], ref.set.ty[[1]][unique.gt.1,"diff.btw"], pch=19, col="orange")
points(ref.set.ty[[1]][unique.gt.1.2,"we.eBH"], ref.set.ty[[1]][unique.gt.1.2,"diff.btw"], pch=19, col="orange4")

points(ref.set.ty[[1]][unique.gt.2,"we.eBH"], ref.set.ty[[1]][unique.gt.2,"diff.btw"], pch=19, col="red")

points(ref.set.ty[[1]][unique.gt.2.2,"we.eBH"], ref.set.ty[[1]][unique.gt.2.2,"diff.btw"], pch=19, col="red4")

abline(h=c(-1,1), lty=2)
abline(v=0.05, lty=3)


