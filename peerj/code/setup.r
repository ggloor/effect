# before you run this
# download the data from
# 10.6084/m9.figshare.8132216
# put it into a directory called data

# this sets up and generates all the data used for the figures
# the data generated by these scripts can be found at
#     Figshare
# but if you want to generate your own instances feel free to go
# ahead if you have some spare electrons. The time requried to run
# these setup scripts is given as comment beside their source calls
# these were on an i9 class moble intel chip

run.source=FALSE

if(run.source == TRUE) {
    source("code/F1_null_data.r") # about 1 hour
        # list.null.Rdata, list.one.Rdata
    source("code/make_d.good.r") # about 1 minute
        # d.good.Rdata
    source("code/null_reproducibility.ty.r") # about 4 hours
        # null.set.ty.effect.RData, null.set.ty.single.effect.RData
    source("code/null_reproducibility.r") # about 2 hours
        # null.set.effect.RData
    source("code/effect_reproducibility.R") # about a week
        # ref.set.yeast.Rdata, test.set.yeast.Rdata
    source("code/effect_reproducibility_ty.R") # about a day
        # ref.set.ty.Rdata, test.set.ty.Rdata
}



# ALDEx2
load(paste(my.path, "data/ref.set.yeast.Rdata", sep=''))
load(paste(my.path, "data/test.set.yeast.Rdata", sep=''))

names(test.set.yeast) <- c(1:40)
test.set.yeast[[1]] <- NULL

# edgeR glm
load(paste(my.path, "data/ref.set.edgeR.RData", sep=''))
load(paste(my.path, "data/test.set.edgeR.RData", sep=''))


# edgeR exact test
load(paste(my.path, "data/ref.set.edgeR.et.RData", sep=''))
load(paste(my.path, "data/test.set.edgeR.et.RData", sep=''))

# ALDEx2 OTU table from Bian study
load(paste(my.path, "data/ref.set.ty.Rdata", sep=''))
load(paste(my.path, "data/test.set.ty.Rdata", sep=''))

