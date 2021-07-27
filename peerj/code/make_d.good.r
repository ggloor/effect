####
# Loads the raw data for the transcriptome
####
# testing tp, fp, tn, fn as replicate number by ALDEx2 effect
library(CoDaSeq)
# read in the dataset
d.agg <- read.table("~/Documents/0_git/effect/data/barton_agg.tsv", sep="\t", header=T, row.names=1)

# get the outliers from each group. See codaSeq.outlier function
# get WT indices
WT <- grep("WT", rownames(d.agg))
# subset
WT.agg <- d.agg[WT,]

# filter, samples by row
wt.gt0 <- WT.agg[,colSums(WT.agg) > 0]

# estimate 0 values (zCompositions)
# samples by row
wt.agg.n0 <- cmultRepl(wt.gt0, method="CZM", label=0)

# clr transform
wt.agg.n0.clr <- t(apply(wt.agg.n0, 1, function(x) log(x) - mean(log(x))))

# make a list of names to keep. found in $good
WT.g <- codaSeq.outlier(wt.agg.n0.clr, plot.me=FALSE)

SNF <- grep("SNF", rownames(d.agg))
# subset
SNF.agg <- d.agg[SNF,]

# filter, samples by row
SNF.gt0 <- SNF.agg[,colSums(SNF.agg) > 0]

# estimate 0 values (zCompositions)
# samples by row
SNF.agg.n0 <- cmultRepl(SNF.gt0, method="CZM", label=0)

# clr transform
SNF.agg.n0.clr <- t(apply(SNF.agg.n0, 1, function(x) log(x) - mean(log(x))))

# make a list of names to keep. found in $good
SNF.g <- codaSeq.outlier(SNF.agg.n0.clr, plot.me=FALSE)

d.good <- rbind(d.agg[WT.g$good,], d.agg[SNF.g$good,])


d.good <- t(d.good)

d.good <- d.good[rowSums(d.good) > 0,]

save(d.good, file="data/d.good.Rdata")
