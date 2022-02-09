## LOLLARDS DATA
## KENT - Descriptive analysis (2)
## R script written by Jose Luis Estevez (Masaryk University)
## Date: Feb 8th 2022
########################################################################################################################

# LOADING
rm(list=ls())
load('Kent_data.RData')

# Required packages
library('sna');library('igraph');library('influenceR');library(statnet);library(Bergm)

########################################################################################################################

# DESCRIPTIVE OF THE NETWORKS
ntw_descript <- as.data.frame(matrix(NA,nrow=12,ncol=4,
                                     dimnames = list(c('den','ave deg','max outdeg','max indeg','recip','trans','iso',
                                                       'assort_sex','assort_def','assort_witn','assort_sent','assort_deg'),
                                                     c('network','inculpation','colocation','kinship'))))

for(i in seq_along(ntw_objs)){
  ntw_descript[1,i] <- gden(ntw_objs[[i]])*100
  ntw_descript[2,i] <- mean(sna::degree(ntw_objs[[i]],cmode='outdegree'))
  ntw_descript[3,i] <- max(sna::degree(ntw_objs[[i]],cmode='outdegree'))
  ntw_descript[4,i] <- max(sna::degree(ntw_objs[[i]],cmode='indegree'))
  ntw_descript[5,i] <- grecip(ntw_objs[[i]],measure='edgewise')*100
  ntw_descript[6,i] <- transitivity(igraph_objs[[i]])*100
  ntw_descript[7,i] <- sum(components(igraph_objs[[i]])$csize == 1)
  ntw_descript[8,i] <- assortativity.nominal(igraph_objs[[i]],as.factor(V(igraph_objs[[i]])$sex))
  ntw_descript[9,i] <- assortativity.nominal(igraph_objs[[i]],as.factor(V(igraph_objs[[i]])$defendant))
  ntw_descript[10,i] <- assortativity.nominal(igraph_objs[[i]],as.factor(V(igraph_objs[[i]])$witness))
  ntw_descript[11,i] <- assortativity.nominal(igraph_objs[[i]],as.factor(V(igraph_objs[[i]])$sentenced))
  ntw_descript[12,i] <- assortativity.degree(igraph_objs[[i]])
}

round(ntw_descript,2)
write.table(ntw_descript,'ntw_descript.csv',row.names=TRUE,sep=',')

########################################################################################################################

# MATRIX OVERLAP
# Jaccard indices
Jaccard <- function(matrix1,matrix2){
  shared_ties <- matrix1*matrix2
  diff_ties <- 1*((matrix1+matrix2)==1)
  denominator <- sum(shared_ties,na.rm=TRUE)+sum(diff_ties,na.rm=TRUE)
  outcome <- ifelse(denominator==0,0,sum(shared_ties,na.rm=TRUE)/denominator)
  return(outcome)
}

biv_descript <- as.data.frame(matrix(NA,nrow=4,ncol=4,
                                     dimnames = list( c('network','inculpation','colocation','kinship'),
                                                     c('network','inculpation','colocation','kinship'))))
mtx <- list(network,inculpations,colocation,kinship)
names(mtx) <- rownames(biv_descript)

for(i in rownames(biv_descript)){
  for(j in colnames(biv_descript)){
    biv_descript[i,j] <- Jaccard(mtx[[i]],mtx[[j]])*100
  }
}

round(biv_descript,1)
write.table(biv_descript,'biv_descript.csv',row.names=TRUE,sep=',')

########################################################################################################################

# BERGM
# Removal of NAs in kinship and colocation
colocation[is.na(colocation)] <- 0
kinship[is.na(kinship)] <- 0

summary(ntw_objs$network ~ edges + mutual +
          gwodegree(decay=log(2),fixed=TRUE) +
          gwidegree(decay=log(2),fixed=TRUE) +
          dgwdsp(decay=log(2),fixed=TRUE,type='OTP') +
          dgwesp(decay=log(2),fixed=TRUE,type='OTP') +
          isolates() +
          edgecov(kinship) + edgecov(colocation) +
          nodeofactor('sex') + nodeifactor('sex') + nodematch('sex') +
          nodeofactor('witness') + 
          nodeifactor('sentenced'))

summary(ntw_objs$inculpations ~ edges + mutual +
          gwodegree(decay=log(2),fixed=TRUE) +
          gwidegree(decay=log(2),fixed=TRUE) +
          dgwdsp(decay=log(2),fixed=TRUE,type='OTP') +
          dgwesp(decay=log(2),fixed=TRUE,type='OTP') +
          isolates() +
          edgecov(kinship) + edgecov(colocation) +
          nodeofactor('sex') + nodeifactor('sex') + nodematch('sex') +
          nodeofactor('witness') + 
          nodeifactor('sentenced'))

# MODELLING
set.seed(0708)
model1 <- bergm(ntw_objs$network ~ edges + mutual +
                  gwodegree(decay=log(2),fixed=TRUE) +
                  gwidegree(decay=log(2),fixed=TRUE) +
                  dgwdsp(decay=log(2),fixed=TRUE,type='OTP') +
                  dgwesp(decay=log(2),fixed=TRUE,type='OTP') +
                  isolates() +
                  edgecov(kinship) + edgecov(colocation) +
                  nodeofactor('sex') + nodeifactor('sex') + nodematch('sex') +
                  nodeofactor('witness') + 
                  nodeifactor('sentenced'),
                burn.in=1000,main.iters=5000,gamma=0.30,
                prior.mean=c(rep(0,14)),prior.sigma=diag(5,14))
summary(model1)  

set.seed(0708)
model2 <- bergm(ntw_objs$inculpations ~ edges + mutual +
                  gwodegree(decay=log(2),fixed=TRUE) +
                  gwidegree(decay=log(2),fixed=TRUE) +
                  dgwdsp(decay=log(2),fixed=TRUE,type='OTP') +
                  dgwesp(decay=log(2),fixed=TRUE,type='OTP') +
                  isolates() +
                  edgecov(kinship) + edgecov(colocation) +
                  nodeofactor('sex') + nodeifactor('sex') + nodematch('sex') +
                  nodeofactor('witness') + 
                  nodeifactor('sentenced'),
                burn.in=1000,main.iters=5000,gamma=0.30,
                prior.mean=c(rep(0,14)),prior.sigma=diag(5,14))
summary(model2)  

########################################################################################################################

# RESULTS (MODELS 1 AND 2)
# FUNCTION TO ADD SIGNIFICANT LEVELS
sig <- function(x){
  ifelse(x < .001,'***',
         ifelse(x < .01,'**',
                ifelse(x < .05,'*',
                       ifelse(x < .1,'+',''))))
}

# MODEL 1 (SOCIAL NETWORK)
# Extraction of posteriors
posteriors <- data.frame(model1$Theta)

# Estimates
model1results <- data.frame(coeff=apply(posteriors,2,mean))

# Name of effects estimated
rownames(model1results) <- c('edges','mutual','gwodeg','gwideg','gwdsp','gwesp','isolates','kinship','same setting',
                             'female (out)','female (in)','same sex','witness (out)','sentenced (in)')

# Extraction of Bayesian p values
for(i in 1:nrow(model1results)){
  if(model1results[i,'coeff'] > 0){
    model1results[i,'p'] <- sum(posteriors[,i] < 0)/length(posteriors[,i])
  }else{
    model1results[i,'p'] <- sum(posteriors[,i] > 0)/length(posteriors[,i])
  }
}

# Significance symbols
model1results$sig <- sig(model1results$p)
model1results
write.table(model1results,'results model 1.csv',row.names=TRUE,sep=',')

# MODEL 2 (INCULPATIONS)
posteriors <- data.frame(model2$Theta)

# Estimates
model2results <- data.frame(coeff=apply(posteriors,2,mean))

# Name of effects estimated
rownames(model2results) <- c('edges','mutual','gwodeg','gwideg','gwdsp','gwesp','isolates','kinship','same setting',
                             'female (out)','female (in)','same sex','witness (out)','sentenced (in)')

# Extraction of Bayesian p values
for(i in 1:nrow(model2results)){
  if(model2results[i,'coeff'] > 0){
    model2results[i,'p'] <- sum(posteriors[,i] < 0)/length(posteriors[,i])
  }else{
    model2results[i,'p'] <- sum(posteriors[,i] > 0)/length(posteriors[,i])
  }
}

# Significance symbols
model2results$sig <- sig(model2results$p)
model2results
write.table(model2results,'results model 2.csv',row.names=TRUE,sep=',')

########################################################################################################################

# GOODNESS OF FIT
jpeg(filename='GOF model 1.jpeg',width=10,height=8,units='in',res=500)
bgof(model1)
dev.off()

jpeg(filename='GOF model 2.jpeg',width=10,height=8,units='in',res=500)
bgof(model2)
dev.off()

########################################################################################################################

# Save image
save.image('Kent_ERGM.RData')