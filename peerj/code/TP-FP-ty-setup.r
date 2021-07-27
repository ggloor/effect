# variable name for 16S data is 'd'
# basically same script as for yeast

if(!exists('d')){  
  load(paste(my.path,'data/tiyani_pup_vs_ys.Rdata', sep=''))
}

if(!exists('test.ty.eff')){
	conds.ty <- c(rep("Pup", 161), rep("YS", 212))

	t <- aldex.clr(d, conds=conds.ty)
	t.e <- aldex.effect(t)

	# reference TP names
	ty.ol <- rownames(t.e)[which(t.e$overlap < 0.05)]
	ty.diff <- rownames(t.e)[which(abs(t.e$diff.btw) > 1)]
	ty.eff <- rownames(t.e)[which(abs(t.e$effect) > 1)]

	ty.diff.ol <- intersect(ty.ol, ty.diff)
	ty.diff.eff <- intersect(ty.eff, ty.diff)
	ty.ol.eff <- intersect(ty.eff, ty.ol)

	# an example FP dataset
	n.samples = 5
	test.col <- c(sample(1:161, n.samples), sample(162:373, n.samples))
	test.conds <- c(rep("Pup", n.samples), rep("YS", n.samples))
	test.data <- d[,test.col]

	t.s <- aldex.clr(test.data, test.conds)
	t.s.e <- aldex.effect(t.s)

	test.ty.ol <- rownames(t.s.e)[which(t.s.e$overlap < 0.1)]
	test.ty.diff <- rownames(t.s.e)[which(abs(t.s.e$diff.btw) > 1)]
	test.ty.eff <- rownames(t.s.e)[which(abs(t.s.e$effect) > 1)]
} # end test for test.ty.eff
