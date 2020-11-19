#Procurement Goverance and  Value for Money
#Author Maulid Bwabo
#PhD Candidate at Jiangsu University
#Management School
#PRINCIPAL COMPONENTS ANALYSIS AND PLS-PM
library("plspm")
library("plsdepot")
library("corrplot")
#Working Directory
setwd("C:/Users/bwabo/OneDrive/Desktop/Review Paper T")
#Import Data
Governance=read.table("C:/Users/bwabo/OneDrive/Desktop/Public Money/Governace 3.csv",header = T,sep = ",", stringsAsFactors = FALSE )
Governance1= read.csv("C:/Users/bwabo/OneDrive/Desktop/Public Money/Governace 3.csv")
str(Governance1)
#path matrix
Transparency =rep(0,4)
Accountability=rep(0,4)
Legal =c(1,1,0,0)
Value =c(1,1,1,0)
#bind the matrix
Gov_path =rbind(Transparency,Accountability,Legal,Value)
#let see
Gov_path
#plot the matrix
innerplot(Gov_path)
#outer Model
# list of blocks (outer model)
Gov_blocks =list(1:9, 10:17, 27:34, 35:41)
# vector of reflective modes
Gov_modes =rep("A",4)
# apply plspm
str(Governance)
col(Governance)
Govpls = plspm(Governance1, Gov_path, Gov_blocks, modes = Gov_modes, scheme="centroid", scaled=FALSE)
summary(Govpls)
Govpls$path_coefs
#Inner model
Govpls$inner_model
Govpls$path_coefs

# PCA
getNormFactor = function(collection) {
  med   = mean(collection);
  eqs   = skewness(collection) / 100;
  dest  = 0.5 - 0.5 * (exp(eqs) - exp(-eqs)) / (exp(eqs) + exp(-eqs));
  return ((med - dest * med) / dest);
}
#
# Correlation
summary(Governance1);
M = cor(GovernaceP)
M
ncol(Governance1)
nrow(Governance1)
# Coerce to matrix, remove the first column which has characters
GovernaceP= subset(Governance1, select = - c(42, 43, 43))
#CPA analysis
help(prcomp)
GovernancePca = prcomp(GovernaceP, center = TRUE, scale. = TRUE)
GovernancePca
GovernancePca$rotation
GovernancePca$x
GovernancePca$sdev
GovernancePca$scale
GovernanceRotationAbs = abs(GovernancePca$rotation)
GovernanceRotationAbs
GovernanceRotationAbs
plot(GovernancePca, type = "l")

GovernancePca
str(GovernancePca)
summary(GovernancePca)
library(devtools)
install.packages("usethis")
#VQV issue
install.packages("ggbiplot")
library(ggbiplot)
install.packages("remotes")
remotes::install_github("vqv/ggbiplot")
library(ggbiplot)
require(ggplot2)
require(plyr)
require(scales)
require(grid)
ggbiplot(GovernancePca)
ggbiplot(GovernancePca, labels=rownames(GovernancePca))
Governace.group = c(rep("Transparency", 269), rep("Accountability",269), rep("Legal", 269),rep("Value",269))
ggbiplot(GovernancePca,ellipse=TRUE,  labels=rownames(GovernaceP), groups=Governace.group)
ggbiplot(mtcars.pca)
library(devtools)
install.packages("usethis")
install_github("vqv/ggbiplot", force = TRUE)
install.packages("remotes")
remotes::install_github("vqv/ggbiplot")
install_github("vqv/ggbiplot")
install.packages("ggbiplot")
library(devtools)
install_github("vqv/ggbiplot")
#Install Library
library(ggbiplot)
ggbiplot(GovernancePca)
names(GovernaceP)
GovernaceP

#Getting Principle Components
# 1: Transparency
GovernanceRotationAbs[order(-GovernanceRotationAbs[,"PC1"]),1]
Transparency_cols = c(1,2,3,4,5,6,7,8,9);
plot(nipals(GovernaceP[,Transparency_cols]), main = "Transparency indicators (circle of correlations)", cex.main = 1)

# 2: Accountability
GovernanceRotationAbs[order(-GovernanceRotationAbs[,"PC2"]),2]
Accountability_cols=c(10,11.12,13,14,15,17);
#impact_cols = c(13);
plot(nipals(GovernaceP[,Accountability_cols]), main = "Accountability indicators (circle of correlations)", cex.main = 1)

# 3: Legal
GovernanceRotationAbs[order(-GovernanceRotationAbs[,"PC3"]),3]
Legal_cols = c(27,28,29,30,31,32,33,34);
plot(nipals(GovernaceP[,Legal_cols]), main = "Legal indicators (circle of correlations)", cex.main = 1)

# 4: value for Money
GovernanceRotationAbs[order(-GovernanceRotationAbs[,"PC4"]),4]
Value_cols = c(35, 36,37,38,39,40,41);
plot(nipals(GovernaceP[,Value_cols]), main = "Value indicators (circle of correlations)", cex.main = 1)
# principal components
GovernaceP = princomp(GovernaceP);
summary(GovernaceP, loadings=TRUE)

# PCA of Influencer indicators with nipals
help("princomp")
infleuncer_pca = nipals(GovernaceP[,c(1:9,10,27:34)])
plot(infleuncer_pca, main = "Influencer indicators (circle of correlations)", cex.main = 1)
# PCA of Content indicators with nipals
content_pca = nipals(tweets[,7:11])
plot(content_pca, main = "Influencer indicators (circle of correlations)", cex.main = 1)

# Building inner model
Transparency =rep(0,4)
Accountability=rep(0,4)
Legal =c(1,1,0,0)
Value =c(1,1,1,0)
# Matrix created by row binding
GovInner = rbind(Transparency, Accountability, Legal, Value)
colnames(GovInner) = rownames(GovInner)

# plot the inner matrix
innerplot(GovInner)

# define list of indicators
GovOuter =list(1:9, 10:17, 27:34, 35:41)

# Tell variables are reflexive or formative
GovModes = rep("A",4)
GovPls = plspm(GovernaceP,GovInner,GovOuter, Gov_modes, maxiter=100)
# Goodness of fit (should be higher than 0.70)
GovPls$gof

# summarized results
summary(GovPls)
# Show results
GovPls
GovPls$path_coefs
GovPls$inner_model
GovPls$inner_summary
plot(GovPls)
GovPls$crossloadings
plot(GovPls, what="loadings")
plot(GovPls, what="weights")
# Unidimensionallity
tweetsPls$unidim

#  Alphas must be higher than 0.7 to be acceotable (rule of thumb)
GovPls$unidim[, 3, drop = FALSE]

# Loadings and communalities
# communality must be higher than 0.7 (comes form 0.7^2 = 50% of variance)
GovPls$outer_model

# Cross loadings
# Does parameter is useful to describe blocks?
GovPls$crossloadings
# Explanation of the blocks
GovPls$inner_model
GovPls$inner_summary
# Validation
# Bootstraping: Add some noise to the original data to make sure that the model correctly
# describes data.
bootPls = plspm(GovernaceP,GovInner, GovOuter, GovModes, boot.val = TRUE, br = 100)
plot(bootPls)
bootPls$crossloadings
plot(bootPls, what="loadings")
plot(bootPls, what="weights")
# Bootstraping results
bootPls$boot
