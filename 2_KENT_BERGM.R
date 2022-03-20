## LOLLARDS DATA
## KENT - BERGM (2)
## R script written by Jose Luis Estevez (Masaryk University)
## Date: March 20th 2022
########################################################################################################################

# LOADING
rm(list=ls())
load('Kent_data.RData')

# Required packages
library(sna);library(igraph);library(Bergm)

########################################################################################################################

# BERGM
# Removal of NAs in kinship and colocation
colocation[is.na(colocation)] <- 0
kinship[is.na(kinship)] <- 0

summary(ntw_objs$inculpations ~ edges + mutual +
          dgwdsp(decay=log(2),fixed=TRUE,type='OTP') +
          dgwesp(decay=log(2),fixed=TRUE,type='OTP') +
          edgecov(kinship) + edgecov(colocation) +
          nodeofactor('sex') + nodeifactor('sex') + nodematch('sex') +
          nodeifactor('witness') +
          nodeifactor('sentenced'))

# MODELLING
set.seed(0708)
model <- bergmM(ntw_objs$inculpations ~ edges + mutual + # bergmM to handle missing data
                  dgwdsp(decay=log(2),fixed=TRUE,type='OTP') +
                  dgwesp(decay=log(2),fixed=TRUE,type='OTP') +
                  edgecov(kinship) + edgecov(colocation) +
                  nodeofactor('sex') + nodeifactor('sex') + nodematch('sex') +
                   nodeifactor('witness') + nodeifactor('sentenced'),
                burn.in=1000,main.iters=5000,gamma=0.30,
                prior.mean=c(rep(0,11)),prior.sigma=diag(5,11))
summary(model) 

########################################################################################################################

# RESULTS 

# Extraction of posteriors
posteriors <- data.frame(model$Theta)
# Estimates
modelresults <- data.frame(coeff=apply(posteriors,2,mean))

# Name of effects estimated
rownames(modelresults) <- c('edges','mutual','gwdsp','gwesp','kinship','same setting',
                             'female (out)','female (in)','same sex','other deponent (in)','under trial (in)')

# Extraction of Bayesian p values
for(i in 1:nrow(modelresults)){
  if(modelresults[i,'coeff'] > 0){
    modelresults[i,'p'] <- sum(posteriors[,i] < 0)/length(posteriors[,i])
  }else{
    modelresults[i,'p'] <- sum(posteriors[,i] > 0)/length(posteriors[,i])
  }
}

# Significance symbols
sig <- function(x){
  ifelse(x < .001,'***',
         ifelse(x < .01,'**',
                ifelse(x < .05,'*',
                       ifelse(x < .1,'+',''))))
}

modelresults$sig <- sig(modelresults$p)
modelresults
write.table(modelresults,'results BERGM.csv',row.names=TRUE,sep=',')

########################################################################################################################

# GOODNESS OF FIT

jpeg(filename='BERGM_gof.jpeg',width=10,height=8,units='in',res=1000)
bgof(model)
dev.off()

########################################################################################################################

# Save image
save.image('Kent_BERGM.RData')