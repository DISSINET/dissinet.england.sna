## LOLLARDS DATA
## KENT - BERGM (2)
## R script written by Jose Luis Estevez (Masaryk University)
## Date: June 9th 2022
########################################################################################################################

# LOADING
rm(list=ls())
load('Kent_data.RData')

# Required packages
library(sna);library(igraph);library(Bergm)

########################################################################################################################

# BERGM
# Removal of NAs in kinship_mtx and same_settlement_mtx
same_settlement_mtx[is.na(same_settlement_mtx)] <- 0
kinship_mtx[is.na(kinship_mtx)] <- 0

summary(naming_ntw ~ edges + mutual +
          gwodegree(decay=log(2),fixed=TRUE)+gwidegree(decay=log(2),fixed=TRUE)+
          dgwdsp(decay=log(2),fixed=TRUE,type='OTP') +
          dgwesp(decay=log(2),fixed=TRUE,type='OTP') +
          edgecov(kinship_mtx) + edgecov(same_settlement_mtx) +
          nodeofactor('sex') + nodeifactor('sex') + nodematch('sex') +
          nodeofactor('witness') + nodeifactor('witness') +
          nodeofactor('impenitent') + nodeifactor('impenitent'))

# MODELLING
set.seed(0708)
model <- bergmM(naming_ntw ~ edges + mutual + 
                  dgwdsp(decay=log(2),fixed=TRUE,type='OTP') +
                  dgwesp(decay=log(2),fixed=TRUE,type='OTP') +
                  edgecov(kinship_mtx) + edgecov(same_settlement_mtx) +
                  nodeofactor('sex') + nodeifactor('sex') + nodematch('sex') +
                  nodeifactor('witness') + nodeifactor('impenitent'),
                burn.in=1000,main.iters=5000,gamma=0.3,
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
                             'female (out)','female (in)','same sex','witness (in)','impenitent heretic (in)')

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