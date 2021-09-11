#Procurement Governance and  Value for Money
#Author Maulid Bwabo
#PhD Candidate at Jiangsu University
#Management School
#PRINCIPAL COMPONENTS ANALYSIS AND PLS-PM
install.packages("plspm")
devtools::install_github("gastonstat/plspm")
library("plspm")
library("plsdepot")
library("corrplot")
help(nipals)
#Working Directory
setwd("C:/Users/bwabo/OneDrive/Desktop/Review Paper T")
#Import Data
library(readr)
Governanace =read.table("C:/Users/bwabo/OneDrive/Desktop/Paper 4/Governanace.csv",header = T,sep = ",", stringsAsFactors = FALSE )
View(Governanace)
summary(Governanace)
Governanace$Experience = as.factor(Governanace$Experience)
Governanace$Age = as.factor(Governanace$Age)
Governanace$Gender = as.factor(Governanace$Gender)
mean=mean(Governanace$T1)
mean(Governanace$Age)
summary(Governanace$Gender)
str(Governanace)
head(Governanace)
#Descriptive Analysis
install.packages("reshape")
install.packages("pastecs")
library(reshape)
library(pastecs)
stat.desc(Governanace)
summfn = function(x)c(n=sum(!is.na(x)),mean=mean(x),sd=sd(x))
x= apply(Governanace,2,summfn)
t(x)
summary(Governanace$Experience)
summary(Governanace$Age)
summary(Governanace$Gender)
Governanace$Ex =ifelse(Governanace$Experience.Length < median(Governanace$Experience.Length),
                   "High", "Low"
)
#Round one
table(Governanace$Experience, Governanace$Age)
xtabs(~Governanace$Experience + Governanace$Age)
prop.table(table(Governanace$Experience,Governanace$Age))
#Gender
#Round two
table(Governanace$Experience, Governanace$Gender)
xtabs(~Governanace$Experience + Governanace$Gender)
prop.table(table(Governanace$Experience,Governanace$Gender))
round(prop.table(table(Governanace$Experience,Governanace$Gender), 1), 2)
#Round three
table(Governanace$Gender, Governanace$Experience)
xtabs(~Governanace$Gender + Governanace$Experience)
prop.table(table(Governanace$Gender,Governanace$Experience))
round(prop.table(table(Governanace$Gender,Governanace$Experience), 1), 2)
#Round two
table(Governanace$Age, Governanace$Experience)
xtabs(~Governanace$Age + Governanace$Experience)
prop.table(table(Governanace$Age, Governanace$Experience))
#Percentage by row
round(prop.table(table(Governanace$Experience,Governanace$Age), 1), 2)
round(prop.table(table(Governanace$Age,Governanace$Experience), 1), 2)
#summary tool package
install.packages("installr")
library(installr)
updateR()
install.packages("gmum.r", type="source")
require(summarytools)
install.packages("summarytools")
library(summarytools)
library(magick)
library(Rcpp)
#Frequency analysis
#Removing the missing data
freq(Governanace$Gender,
     report.nas = FALSE # remove NA information
)
freq(Governanace$Age,
     report.nas = FALSE # remove NA information
)
freq(Governanace$Experience,
     report.nas = FALSE # remove NA information
)
#Chi-square test for each group
ctable(
  x = Governanace$Experience,
  y = Governanace$Age,
  chisq = TRUE, # display results of Chi-square test of independence
  headings = FALSE # remove headings
)
ctable(
  x = Governanace$Gender,
  y = Governanace$Age,
  chisq = TRUE, # display results of Chi-square test of independence
  headings = FALSE # remove headings
)
#Continue
descr(Governanace,
      headings = FALSE, # remove headings
      stats = "common" # most common descriptive statistics
)
#
stby(
  data = Governanace,
  INDICES = Governanace$Experience, # by Species
  FUN = descr, # descriptive statistics
  stats = "common" # most common descr. stats
)
#
stby(
  data = Governanace,
  INDICES = Governanace$Age, # by Species
  FUN = descr, # descriptive statistics
  stats = "common" # most common descr. stats
)
#
stby(
  data = Governanace,
  INDICES = Governanace$Gender, # by Species
  FUN = descr, # descriptive statistics
  stats = "common" # most common descr. stats
)
#
dfSummary(Governanace)
install.packages("pander")
install.packages("rapportools")
library(summarytools)
library(Rcpp)
str(Governanace)
sstate = scale(Governanace,center=apply(Governanace,2,median),
                scale=apply(Governanace,2,mad))
#Conditional Process Analysis
# select data for Experience  (HIGH|LOW)
High = Governanace[Governanace$Experience =="High",]
Low = Governanace[Governanace$Experience=="Low",]
# High experience plspm
High_pls =plspm(High,Gov_path, Gov_blocks, modes = Gov_modes, scheme="centroid", scaled=FALSE)
summary(High_pls)
High_val=plspm(High,Gov_path, Gov_blocks, modes = Gov_modes, boot.val=TRUE,br=5000)
High_val
summary(High_val)
# plot path coefficients
plot(High_pls)
#Low experience plspm
Low_pls =plspm(Low,Gov_path, Gov_blocks, modes = Gov_modes, scheme="centroid", scaled=FALSE)
summary(Low_pls)
Low_val=plspm(Low,Gov_path, Gov_blocks, modes = Gov_modes, boot.val=TRUE,br=5000)
Low_val
summary(Low_val)
#Plot path coefficients
plot(Low_pls)
#Permutations
Governance1$Experience = as.factor(Governanace$Experience)
Gov_pls_perm =plspm.groups(Govpls, Governanace$Experience, method ="permutation")
Gov_pls_perm
#bootstrap t-test
Govpls_boot =plspm.groups(Govpls, Governanace$Experience, method ="bootstrap")
Govpls_boot
Govpls$inner_model
#Next analysis phase (PCA)
Governance1= read.csv("C:/Users/bwabo/OneDrive/Desktop/Public Money/Governace3.csv")
str(Governance1)
head(Governance1)
head(Governance1)
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
# Apply PLS-PM
str(Governance1)
col(Governance)
Govpls = plspm(Governance1, Gov_path, Gov_blocks, modes = Gov_modes, scheme="centroid", scaled=FALSE)
summary(Govpls)
Govpls$path_coefs
# Bootstrapping: Add some noise to the original data to make sure that the model correctly
# describes data.
bootgov = plspm(Governance1, Gov_path, Gov_blocks, modes = Gov_modes, boot.val = TRUE, br = 5000)
bootgov$boot$paths
bootgov$boot$total.efs
bootgov$boot$rsq
bootgov$boot$loadings

plot(bootgov)
bootgov$crossloadings
plot(bootgov, what="loadings")
plot(bootgov, what="weights")
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
str(Governance1);
M = cor(GovernaceP)
M
ncol(Governance1)
nrow(Governance1)
str(GovernaceP)
# Coerce to matrix, remove the first column which has characters
Governance1$Gender = as.factor(Governance1$Gender)
GovernaceP= subset(Governance1, select = - c(42, 43))
log.GovernanceP = log(GovernaceP[, 1:9])
log.GovernanceP = log(GovernaceP[, 10:17])
log.GovernanceP = log(GovernaceP[, 27:34])
log.GovernanceP = log(GovernaceP[, 35:41])
log.GovernanceP = log(GovernaceP[, 35:41])
Governance.Gender = GovernaceP[, 42]
#CPA analysis
log.GovernanceP = prcomp(GovernaceP, center = TRUE, scale. = TRUE)
log.GovernanceP
log.GovernanceP$rotation
log.GovernanceP$x
log.GovernanceP$sdev
log.GovernanceP$scale
GovernanceRotationAbs = abs(log.GovernanceP$rotation)
GovernanceRotationAbs
GovernanceRotationAbs
plot(log.GovernanceP, type = "l")
GovernancePca
str(log.GovernanceP)
summary(log.GovernanceP)
library(devtools)
install.packages("usethis")
library(usethis)
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
ggbiplot(log.GovernanceP)
ggbiplot(log.GovernanceP, labels=rownames(log.GovernanceP))
predict(log.GovernanceP,
        newdata=tail(GovernaceP, 2))
#Interpreting the Result
group = ggbiplot(log.GovernanceP, obs.scale = 1, var.scale = 1,
                 groups = Governance.Gender, ellipse = TRUE,
                 circle = TRUE)
group = group + scale_color_discrete(name = '')
group = group + theme(legend.direction = 'horizontal',
                      legend.position = 'top')
print(group)
ggbiplot(log.GovernanceP,ellipse=TRUE,choices=c(3,4), labels=rownames(GovernaceP), groups=Governance.Gender)
ggbiplot(log.GovernanceP,ellipse=TRUE,choices=c(5,6), labels=rownames(GovernaceP), groups=Governance.Gender)
ggbiplot(log.GovernanceP,ellipse=TRUE,choices=c(7,8), labels=rownames(GovernaceP), groups=Governance.Gender)
ggbiplot(log.GovernanceP,ellipse=TRUE,choices=c(8,9), labels=rownames(GovernaceP), groups=Governance.Gender)
#Graphical Parameter with ggbiplot
ggbiplot(log.GovernanceP,ellipse=TRUE,circle=TRUE, labels=rownames(GovernaceP), groups=Governance.Gender)
ggbiplot(log.GovernanceP,ellipse=TRUE,obs.scale = 1, var.scale = 1,  labels=rownames(GovernaceP), groups=Governance.Gender)
#Removing the arrows from graphs
ggbiplot(log.GovernanceP,ellipse=TRUE,obs.scale = 1, var.scale = 1,var.axes=FALSE,   labels=rownames(GovernaceP), groups=Governance.Gender)
#Customized ggbiplot
ggbiplot(log.GovernanceP,ellipse=TRUE,obs.scale = 1, var.scale = 1,  labels=rownames(GovernaceP), groups=Governance.Gender) +
  scale_colour_manual(name="Origin", values= c("forest green", "red3", "dark blue"))+
  ggtitle("PCA of Governance dataset")+
  theme_minimal()+
  theme(legend.position = "bottom")

#Installing ggbiplot
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
ggbiplot(log.GovernanceP)
names(log.GovernanceP)
log.GovernanceP

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
GovernaceP= princomp(GovernaceP);
summary(GovernaceP, loadings=TRUE)

# PCA of Influencer indicators with nipals
infleuncer_pca1 = nipals(GovernaceP[,c(1:9)])
plot(infleuncer_pca, main = "Influencer indicators (circle of correlations)", cex.main = 1)
infleuncer_pca2 = nipals(GovernaceP[,c(10:17)])
plot(infleuncer_pca2, main = "Influencer indicators (circle of correlations)", cex.main = 1)
infleuncer_pca3 = nipals(GovernaceP[,c(27:34)])
plot(infleuncer_pca3, main = "Influencer indicators (circle of correlations)", cex.main = 1)
infleuncer_pca4 = nipals(GovernaceP[,c(35:41)])
plot(infleuncer_pca4, main = "Influencer indicators (circle of correlations)", cex.main = 1)
# PCA of Content indicators with nipals
content_pca1 = nipals(GovernaceP[,1:9])
plot(content_pca1, main = "Influencer indicators (circle of correlations)", cex.main = 1)
content_pca2 = nipals(GovernaceP[,10:17])
plot(content_pca2, main = "Influencer indicators (circle of correlations)", cex.main = 1)
content_pca3 = nipals(GovernaceP[,27:34])
plot(content_pca3, main = "Influencer indicators (circle of correlations)", cex.main = 1)
content_pca4 = nipals(GovernaceP[,35:41])
plot(content_pca4, main = "Influencer indicators (circle of correlations)", cex.main = 1)
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

#  Alphas must be higher than 0.7 (rule of thumb)
GovPls$unidim[, 3, drop = FALSE]

# Loading and communalities
# communality must be higher than 0.7 (comes form 0.7^2 = 50% of variance)
GovPls$outer_model

# Cross loading
# Does parameter is useful to describe blocks?
GovPls$crossloadings
# Explanation of the blocks
GovPls$inner_model
GovPls$inner_summary
# Validation
# Bootstrapping: Add some noise to the original data to make sure that the model correctly
# describes data.
bootPls = plspm(GovernaceP,GovInner, GovOuter, GovModes, boot.val = TRUE, br = 100)
plot(bootPls)
bootPls$crossloadings
plot(bootPls, what="loadings")
plot(bootPls, what="weights")
# Bootstrapping results
bootPls$boot
#Clustering
head(Governance)
GovernanceT=Governance[,which(names(Governance) != "Gender")]
GovernaceT= subset(Governance, select = - c(42, 43))
head(GovernaceT)
GovernanceT=Governance[,which(names(Governance) != "Age")]
set.seed(278613)
GovernanceK=kmeans(x=GovernaceT,centers = 3)
GovernanceK
plot(GovernanceK,data = GovernaceT)
set.seed(278613)
GovernanceTZ=kmeans(GovernaceT [,1:17], centers = 3,nstart = 25)
GovernanceTZ
GovernanceTZ$size
GovernanceTZ$cluster
GovernanceTZ$withinss
GovernanceTZ$totss
GovernanceTZ$tot.withinss
GovernanceTZ$betweenss
GovernanceTZ$iter
GovernanceB=FitKMeans(GovernaceT,max.clusters = 20,nstart = 25, seed = 278613)
GovernanceB$Hartigan
GovernanceB$Clusters
PlotHartigan(GovernanceB)
#Gap Statistics
require(cluster)
theGov=clusGap(GovernaceT,FUNcluster = pam,K.max = 50)
GovDF=as.data.frame(theGov$Tab)
GovDF
#Log W curves
ggplot(GovDF,aes(x=1:nrow(GovDF)))+
  geom_line(aes(y=logW), color="blue")+
  geom_point(aes(y=logW), color="blue")+
  geom_line(aes(y=E.logW), color="green")+
  geom_point(aes(y=E.logW), color="green")+
  labs(x="Number of Clusters", y="Gap")
#Gap Curve
ggplot(GovDF,aes(x=1:nrow(GovDF)))+
  geom_line(aes(y=gap), color="red")+
  geom_point(aes(y=gap), color="red")+
  geom_errorbar(aes(ymin=gap-SE.sim, ymax=gap+SE.sim),color="red")+
  labs(x="Number of Clusters", y="Gap")
#Hierarchical Clustering
GovH=hclust(d=dist(GovernaceT))
plot(GovH)
#Linkage Method
Govh1=hclust(dist(GovernaceT),method = "single")
Govh2=hclust(dist(GovernaceT),method = "complete")
Govh3=hclust(dist(GovernaceT),method = "average")
Govh4=hclust(dist(GovernaceT),method = "centroid")
#
plot(Govh1,labels = FALSE,main = "Single")
plot(Govh2,labels = FALSE,main = "Complete")
plot(Govh3,labels = FALSE,main = "average")
plot(Govh4,labels = FALSE,main = "centroid")
#Plot the tree
plot(GovH)
#Split into three clusters
rect.hclust(GovH,k=3,border = "red")
#Split into 13 clusters
rect.hclust(GovH,k=13, border = "blue")
#Conditional Process Analysis
# select data for Experience  (HIGH|LOW)
High = Governance1[Governance1$Experience =="HIGH",]
Low = Governance1[Governance1$Experience=="LOW",]
# High experience plspm
High_pls =plspm(High,Gov_path, Gov_blocks, modes = Gov_modes, scheme="centroid", scaled=FALSE)
summary(High_pls)
High_val=plspm(High,Gov_path, Gov_blocks, modes = Gov_modes, boot.val=TRUE,br=5000)
High_val
summary(High_val)
#Low experience plspm
Low_pls =plspm(Low,Gov_path, Gov_blocks, modes = Gov_modes, scheme="centroid", scaled=FALSE)
summary(Low_pls)
Low_val=plspm(Low,Gov_path, Gov_blocks, modes = Gov_modes, boot.val=TRUE,br=5000)
Low_val
summary(Low_val)
#Permutations
str(Governance1)
Governance1$Experience = as.factor(Governance1$Experience)
Gov_pls_perm =plspm.groups(Govpls, Governance1$Experience, method ="permutation")
Govpls_boot =plspm.groups(Govpls, Governance1$Experience, method ="bootstrap")
#Process package for Moderation and Mediation
if(!require(devtools)) install.packages("devtools")
devtools::install_github("cardiomoon/processR")
#Segmentation tree in PLS-PM
library(genpathmox)
#example 2
Governanace =read.table("C:/Users/bwabo/OneDrive/Desktop/Paper 4/Governanace.csv",header = T,sep = ",", stringsAsFactors = FALSE )
View(Governanace)
summary(Governanace)
Governanace$Experience = as.factor(Governanace$Experience)
Governanace$Age = as.factor(Governanace$Age)
Governanace$Gender = as.factor(Governanace$Gender)
# select manifest variables
data.gov=Governanace[,1:41]
#define the inner model matrix
Transparency =rep(0,4)
Accountability=rep(0,4)
Legal =c(1,1,0,0)
Value =c(1,1,1,0)
#bind the matrix
Gov_path =rbind(Transparency,Accountability,Legal,Value)
# list of blocks (outer model)
Gov_blocks =list(1:9, 10:17, 27:34, 35:41)
# vector of reflective modes
Gov_modes =rep("A",4)
#re-ordering the segmentation into true scale
seg.gov= Governanace[,42:44]
View(seg.gov)
#Ordering
seg.gov$Experience = factor(seg.gov$Experience, ordered=TRUE)
seg.gov$Age=factor(seg.gov$Age,ordered = TRUE)
seg.gov$Age=factor(seg.gov$Age,bin.levels(seg.gov$Age,spl = 2))
seg.gov$Gender=factor(seg.gov$Gender,bin.levels(seg.gov$Gender,spl = 2))
seg.gov$Gender=factor(seg.gov$Gender,ordered = TRUE)
# Pathmox Analysis
gov.pathmox=pls.pathmox(data.gov, Gov_path, Gov_blocks, Gov_modes,SVAR=seg.gov,signif=0.05,
                         deep=2,size=0.2,n.node=20)
gov.pathmox$terminal
gov.pathmox$nodes
gov.pathmox$candidates
gov.pathmox$model
gov.pathmox$Fc.r
gov.pathmox$hybrid
#tree nodes
nodes.models=pls.treemodel(gov.pathmox)
nodes.models$loadings
nodes.models$path_sgnificance
nodes.models$predictive_power_R2
nodes.models$total_effects
#summary
summarize.mox(gov.pathmox)
citation("genpathmox")
citation("cluster")
citation("plspm")
