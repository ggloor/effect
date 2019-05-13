######
# selex
######

library(ALDEx2)
data(selex)
conds <- c(rep("NS", 7), rep("E", 7))
x <- aldex.clr(selex, conds)
x.e <- aldex.effect(x, CI=T)
x.t <- aldex.ttest(x, verbose=T)
x.all.S <- data.frame(x.e,x.t)

sgn <-  sign(x.all.S$effect.low) == sign(x.all.S$effect.high)
eff <- abs(x.all.S$effect) > 2

aldex.plot(x.all.S)
points(x.all.S$diff.win[eff], x.all.S$diff.btw[eff], pch=19, cex=0.5, col="cyan")
points(x.all.S$diff.win[sgn], x.all.S$diff.btw[sgn], col="blue")

mtext("SELEX", 3, line=1,cex=1.5)



######
# BARTON Tscome
######
# depends on effect/code/setup.r

conds <- c(rep("WT", length(WT.g$good)), rep("KO", length(SNF.g$good)))
x <- aldex.clr(d.good, conds)
x.e <- aldex.effect(x, CI=T)
x.t <- aldex.ttest(x, verbose=T)
x.all.Y <- data.frame(x.e,x.t)

sgn <-  sign(x.all.Y$effect.low) == sign(x.all.Y$effect.high)
eff <- abs(x.all.Y$effect) > 2

aldex.plot(x.all.Y)
points(x.all.Y$diff.win[eff], x.all.Y$diff.btw[eff], pch=19, cex=0.5, col="cyan")
points(x.all.Y$diff.win[sgn], x.all.Y$diff.btw[sgn], col="blue")

cols <- grep("rab.win", colnames(x.all.Y))

mtext("Transcriptome", 3, line=1,cex=1.5)

#######
# tiyani 16S
#######
load("data/tiyani_pup_vs_ys.Rdata")

conds <- c(rep("Pup", 161), rep("YS", 212))
x <- aldex.clr(d, conds=conds)
x.e <- aldex.effect(x, CI=T)
x.t <- aldex.ttest(x, verbose=T)
x.all.16 <- data.frame(x.e,x.t)

sgn <-  sign(x.all.16$effect.low) == sign(x.all.16$effect.high)
eff <- abs(x.all.16$effect) > 2

aldex.plot(x.all.16)
points(x.all.16$diff.win[eff], x.all.16$diff.btw[eff], pch=19, cex=0.5, col="cyan")
points(x.all.16$diff.win[sgn], x.all.16$diff.btw[sgn], col="blue")

cols <- grep("rab.win", colnames(x.all.16))

mtext("16S rRNA gene sequencing", 3, line=1,cex=1.5)
