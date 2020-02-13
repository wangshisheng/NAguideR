############################################################################################################################
#### This Scipt describes an example of running a dataset to impute values
#### based on KNN-CR, KNN-TN and KNN-EU
############################################################################################################################

source("Trunc_KNN/Imput_funcs.r")
source("Trunc_KNN/Simulation.r")

### Load libraries as some functions are dependent on them

library(MASS)
library(mvtnorm)
library(Matrix)
library(magrittr)

###########################################################################
##### Here we Simulate a dataset of size 50 Samples by 400 Metabolites
##### We will have a Complete Dataset and a Dataset with Missing Values
##### For this example we generate a Dataset with 15% total Missing where
##### 10% of the Data is missing below the Threshold and 5% missing above
##### the threshold. We also discard if there is missing greater than 75%.
############################################################################
set.seed(505)
CorrelationMatrix0.7 <- CorrMatrixNegFixed(20, 20, 0.7, 0.2)
DataSimul <- SimulatedData(50, 400, as.matrix(nearPD(CorrelationMatrix0.7)[[1]]), low = -5, high =5)

## Simulate 15% overall missing, 10% due to below LOD (MNAR), 5% MAR
## Only retain metabolites with <75% MVs
Missingness <- MissingData(DataSimul, 15, 5, 0.75)

CompleteData <- Missingness[[2]]
MissData <- Missingness[[1]]
dim(MissData) ## 50x390; After Screening we have 50 Samples and 390 Metabolites

sum(is.na(MissData)/length(MissData))  ## 13.5%, below 15% due to screening

LOD <- quantile(DataSimul, probs = 0.1)


##################################################################################
### We now do the different imputations and we provide the type of imputation and
### k neighbors
##################################################################################

(kNN_Corr_Imp <- imputeKNN(t(MissData), k=10 , distance = "correlation"))
(kNN_Euc_Imp <- KNNEuc(t(MissData), k=10))
(kNN_Trunc_Imp <- imputeKNN(t(MissData), k=10 , distance = "truncation", perc= 0.75))

###################################################################################
### We compute the RMSE since we have the CompleteData, MissingData and ImputedData
####################################################################################

RMSError <- ErrorsComputation(trunc=kNN_Trunc_Imp, corr=kNN_Corr_Imp, euc=kNN_Euc_Imp,
                              miss=MissData, complete = CompleteData)

names(RMSError) <- c("KNN-TN", "KNN-CR", "KNN-EU")
RMSError
##   KNN-TN   KNN-CR   KNN-EU
## 1.161180 1.345218 1.606502

######################################################################################
### We look at the Distribution of Metabolites based on the Imputation Method
######################################################################################


## This plots the first 20 Metabolites of the Dataset and overlays the imputed values
## from the different methods in different colors
## Reproduces Figure 6 in the manuscript

dim(MissData)
sum(is.na(MissData[,1:20]))  ## 190

png("Figure6.png", width = 7, height = 7, units = 'in', res = 600)

col.black <- rgb(0,0,0,alpha=20,maxColorValue=255)
col.black2 <- rgb(0,0,0,alpha=255,maxColorValue=255)

col.blue  <- rgb(0,0,255,alpha=200,maxColorValue=255)
col.red   <- rgb(255,0,0,alpha=200,maxColorValue=255)
col.green <- rgb(0,255,0,alpha=200,maxColorValue=255)

idx.na <- which(is.na(MissData[,1:20]))

plot(rep(1:20, each = 50)[-idx.na], CompleteData[,1:20][-idx.na],
         xlab = "Metabolite", ylab = "Intensity Values", xlim=c(0, 20.5),
         pch = 1, ylim = c(-8,8), col = col.black, cex = 0.9) ##, xaxt='n')

## Just use 'points' to add to a plot rather than the 'plot' command
points(rep(1:20, each = 50)[idx.na], CompleteData[,1:20][idx.na],
       pch = 8, col = col.black2, cex = 0.9)

## Need to make these colors more transparent so we can see all of them
## Shift these slightly to the right when plotting ...

points(rep(1:20, each = 50)[idx.na]+0.23,  t(kNN_Trunc_Imp[1:20,])[idx.na],
       pch=17,  ylim = c(-8,8), col = col.blue, cex = 0.9)  ## Trun
points(rep(1:20, each = 50)[idx.na]+0.46,  t(kNN_Corr_Imp[1:20,])[idx.na],
       pch=15,  ylim = c(-8,8),col = col.red, cex = 0.9)   ## Corr
points(rep(1:20, each = 50)[idx.na]+0.7,  t(kNN_Euc_Imp[1:20,])[idx.na],
       pch=18,  ylim = c(-8,8),col = col.green, cex = 0.9)  ## EUC

legend("topleft", c("Original-Observed", "Orginal-Missing", "KNN-TN", "KNN-CR", "KNN-EU"),
       pch = c(1, 8, 17, 15, 18), col = c(1, 1, 4, 2, 3),  bty = "n",
       text.col = c(1, 1, 4, 2, 3), pt.cex = 1)

## Color region below LOD light red
col.red2   <- rgb(255,0,0,alpha=25,maxColorValue=255)
lims <- par("usr")
polygon(x = c(lims[1], lims[2], lims[2], lims[1]), y = c(LOD, LOD, lims[3], lims[3]), col = col.red2,
        border = NA)

dev.off()

