###
# Figure 1
###
# depends on TP-FP-yeast-setup.r

if (exists('x')){
	pdf('YDR171W_dist.pdf')
	aldex.plotFeature(x, 'YDR171W')
	abline(v=0, lty=2, col='grey', lwd=2)
	dev.off()
}
