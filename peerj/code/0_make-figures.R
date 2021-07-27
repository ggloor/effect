# This is an R script and you can call it inside or outside R
# 
# all code and data must be available inside a working directory
#
# Set the main path to the working directory
# 

my.path <- '~/Documents/0_git/effect/peerJ/'

# load up the package dependencies
# note that these are needed to regenerate the data
# see sessionInfo() output for all packages loaded

library(ALDEx2)
library(edgeR)
library(foreach)
library(doMC)


# script to make all figures in effect size submission
# much of the raw data is pre-computed because it takes
#   a friggin long time. The supplementary figure data
#   took over a week, but the vectorized effect size 
#   function in ALDEx2 and the use of parallel computing
#   will bring this down to under a day. 

####
# Main paper figures
####

# first get the raw data for Figure 1 and generate
# a single instance TP yeast dataset

source(paste(my.path, 'code/TP-FP-yeast-setup.r', sep=''))
source(paste(my.path, 'code/TP-FP-ty-setup.r', sep=''))

###
# Figure 1: file is YDR171W_dist.pdf
# source(paste(my.path, 'code/Fig1.r', sep=''))

###
# Figure 2: file is FP-plots.pdf

# get the data (or run the calculation if the data does not exist)
# for the FP only analysis of Figure 2

source(paste(my.path, 'code/FP-only-summary-parallel.r', sep=''))

source(paste(my.path, 'code/Fig2.r', sep=''))

###
# Figure 3: file is null.effect.pdf
source(paste(my.path, 'code/Fig3.r', sep=''))

###
# Figure 4: file is TPvsFP.pdf
source(paste(my.path, 'code/TP-FP-yeast-summary-parallel.r', sep=''))
source(paste(my.path, 'code/TP-FP-summary-parallel-ty.r', sep=''))
source(paste(my.path, 'code/TP-FP-summary-parallel-edgeR.r', sep=''))

source(paste(my.path, 'code/Fig4.r', sep=''))

####
# sessionInfo()
# 
# R version 4.0.4 (2021-02-15)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS Big Sur 10.16
# 
# Matrix products: default
# BLAS:   /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRblas.dylib
# LAPACK: /Library/Frameworks/R.framework/Versions/4.0/Resources/lib/libRlapack.dylib
# 
# locale:
# [1] en_CA.UTF-8/en_CA.UTF-8/en_CA.UTF-8/C/en_CA.UTF-8/en_CA.UTF-8
# 
# attached base packages:
# [1] parallel  stats     graphics  grDevices utils     datasets  methods  
# [8] base     
# 
# other attached packages:
#  [1] edgeR_3.30.3        limma_3.44.3        doMC_1.3.7         
#  [4] iterators_1.0.13    foreach_1.5.1       ALDEx2_1.23.3      
#  [7] zCompositions_1.3.4 truncnorm_1.0-8     NADA_1.6-1.1       
# [10] survival_3.2-7      MASS_7.3-53        
# 
# loaded via a namespace (and not attached):
#  [1] Rcpp_1.0.6                  XVector_0.28.0             
#  [3] GenomicRanges_1.40.0        BiocGenerics_0.34.0        
#  [5] splines_4.0.4               zlibbioc_1.34.0            
#  [7] IRanges_2.22.2              BiocParallel_1.22.0        
#  [9] lattice_0.20-41             GenomeInfoDb_1.24.2        
# [11] tools_4.0.4                 SummarizedExperiment_1.18.2
# [13] Rfast_2.0.1                 grid_4.0.4                 
# [15] Biobase_2.48.0              matrixStats_0.58.0         
# [17] multtest_2.44.0             Matrix_1.3-2               
# [19] GenomeInfoDbData_1.2.3      codetools_0.2-18           
# [21] S4Vectors_0.26.1            bitops_1.0-6               
# [23] RCurl_1.98-1.3              DelayedArray_0.14.1        
# [25] compiler_4.0.4              locfit_1.5-9.4             
# [27] RcppZiggurat_0.1.6          stats4_4.0.4        