## LOLLARDS DATA
## KENT - SAOMs (3)
## R script written by Jose Luis Estevez (Masaryk University)
## Date: Feb 8th 2022
########################################################################################################################

# LOADING
rm(list=ls())
load('Kent_data.RData')

# Required packages
library(igraph);library(RSiena);library(ggpubr)

########################################################################################################################

# let's change one tie for RSiena to work
network0 <- network
inculpations0 <- inculpations
illicit_speech0 <- illicit_speech

set.seed(0708)
network0[network0 == 1][sample(1:length(network0[network0 == 1]),1)] <- 0
network0[network0 == 0][sample(1:length(network0[network0 == 0]),1)] <- 1

set.seed(0708)
inculpations0[inculpations0 == 1][sample(1:length(inculpations0[inculpations0 == 1]),1)] <- 0
inculpations0[inculpations0 == 0][sample(1:length(inculpations0[inculpations0 == 0]),1)] <- 1

set.seed(0708)
illicit_speech0[illicit_speech0 == 1][sample(1:length(illicit_speech0[illicit_speech0 == 1]),1)] <- 0
illicit_speech0[illicit_speech0 == 0][sample(1:length(illicit_speech0[illicit_speech0 == 0]),1)] <- 1

########################################################################################################################

# SIENA OBJECT CREATION
siena_network <- sienaDataCreate(
  network = sienaNet(array(c(network0,network),dim=c(nrow(network),ncol(network),2))), # the dependent network
  kinship = coDyadCovar(kinship),
  colocation = coDyadCovar(colocation),
  sex = coCovar(ifelse(V(igraph_objs$network)$sex == 'Woman',1,0),centered=TRUE),
  witness = coCovar(ifelse(V(igraph_objs$network)$witness == 'Yes',1,0),centered=TRUE),
  sentenced = coCovar(ifelse(V(igraph_objs$network)$sentenced == 'Yes',1,0),centered=TRUE)
)

siena_inculpations <- sienaDataCreate(
  inculpations = sienaNet(array(c(inculpations0,inculpations),dim=c(nrow(inculpations),ncol(inculpations),2))), # the dependent inculpations
  kinship = coDyadCovar(kinship),
  colocation = coDyadCovar(colocation),
  sex = coCovar(ifelse(V(igraph_objs$inculpations)$sex == 'Woman',1,0),centered=TRUE),
  witness = coCovar(ifelse(V(igraph_objs$inculpations)$witness == 'Yes',1,0),centered=TRUE),
  sentenced = coCovar(ifelse(V(igraph_objs$inculpations)$sentenced == 'Yes',1,0),centered=TRUE)
)

siena_illicit_speech <- sienaDataCreate(
  illicit_speech = sienaNet(array(c(illicit_speech0,illicit_speech),dim=c(nrow(illicit_speech),ncol(illicit_speech),2))), # the dependent inculpations
  kinship = coDyadCovar(kinship),
  colocation = coDyadCovar(colocation),
  sex = coCovar(ifelse(V(igraph_objs$illicit_speech)$sex == 'Woman',1,0),centered=TRUE),
  witness = coCovar(ifelse(V(igraph_objs$illicit_speech)$witness == 'Yes',1,0),centered=TRUE),
  sentenced = coCovar(ifelse(V(igraph_objs$illicit_speech)$sentenced == 'Yes',1,0),centered=TRUE)
)

########################################################################################################################

# SIENA EFFECTS
sienaEffnetwork <- getEffects(siena_network)
sienaEffnetwork <- includeEffects(sienaEffnetwork,outActSqrt,inPopSqrt,gwespFF,name='network')
sienaEffnetwork <- includeInteraction(sienaEffnetwork,recip,gwespFF,parameter=69)
sienaEffnetwork <- includeEffects(sienaEffnetwork,X,interaction1 = 'kinship')
sienaEffnetwork <- includeEffects(sienaEffnetwork,X,interaction1 = 'colocation')
sienaEffnetwork <- includeEffects(sienaEffnetwork,altX,egoX,sameX,interaction1 = 'sex')
sienaEffnetwork <- includeEffects(sienaEffnetwork,egoX,interaction1 = 'witness')
sienaEffnetwork <- includeEffects(sienaEffnetwork,altX,interaction1 = 'sentenced')
sienaEffnetwork <- setEffect(sienaEffnetwork,Rate,initialValue=10,name='network',fix=TRUE,type="rate")

sienaEffinculpations <- getEffects(siena_inculpations)
sienaEffinculpations <- includeEffects(sienaEffinculpations,outActSqrt,inPopSqrt,gwespFF,name='inculpations')
sienaEffinculpations <- includeInteraction(sienaEffinculpations,recip,gwespFF,parameter=69)
sienaEffinculpations <- includeEffects(sienaEffinculpations,X,interaction1 = 'kinship')
sienaEffinculpations <- includeEffects(sienaEffinculpations,X,interaction1 = 'colocation')
sienaEffinculpations <- includeEffects(sienaEffinculpations,altX,egoX,sameX,interaction1 = 'sex')
sienaEffinculpations <- includeEffects(sienaEffinculpations,egoX,interaction1 = 'witness')
sienaEffinculpations <- includeEffects(sienaEffinculpations,altX,interaction1 = 'sentenced')
sienaEffinculpations <- setEffect(sienaEffinculpations,Rate,initialValue=10,name='inculpations',fix=TRUE,type="rate")

sienaEffillicit_speech <- getEffects(siena_illicit_speech)
sienaEffillicit_speech <- includeEffects(sienaEffillicit_speech,outActSqrt,inPopSqrt,gwespFF,name='illicit_speech')
sienaEffillicit_speech <- includeInteraction(sienaEffillicit_speech,recip,gwespFF,parameter=69)
sienaEffillicit_speech <- includeEffects(sienaEffillicit_speech,X,interaction1 = 'kinship')
sienaEffillicit_speech <- includeEffects(sienaEffillicit_speech,X,interaction1 = 'colocation')
sienaEffillicit_speech <- includeEffects(sienaEffillicit_speech,altX,egoX,sameX,interaction1 = 'sex')
sienaEffillicit_speech <- includeEffects(sienaEffillicit_speech,egoX,interaction1 = 'witness')
sienaEffillicit_speech <- includeEffects(sienaEffillicit_speech,altX,interaction1 = 'sentenced')
sienaEffillicit_speech <- setEffect(sienaEffillicit_speech,Rate,initialValue=10,name='illicit_speech',fix=TRUE,type="rate")

########################################################################################################################

# ALGORITHM
algorithm1 <- sienaAlgorithmCreate(projname='siena_network',useStdInits=FALSE,cond=FALSE,nsub=4,n3=5000,seed=0708)
algorithm2 <- sienaAlgorithmCreate(projname='siena_inculpations',useStdInits=FALSE,cond=FALSE,nsub=4,n3=5000,seed=0708)
algorithm3 <- sienaAlgorithmCreate(projname='siena_illicit_speech',useStdInits=FALSE,cond=FALSE,nsub=4,n3=5000,seed=0708)

# Function for running siena07 until convergence has been reached
siena07RunToConvergence <- function(alg, data, eff, ans0, modelName, ...){
  numr <- 0
  ans <- siena07(alg, data = data, eff = eff, prevAns = ans0, returnDeps = TRUE, ...)
  
  repeat{
    numr <- numr + 1
    tconv.max <- ans$tconv.max
    tratio.max <- max( abs( ans$tstat[eff$type[eff$include] != "rate"] ) )
    if (tconv.max > 100) {
      print(ans)
      cat("WARNING: Extreme divergence. Terminating run.\n")
      return("WARNING: Extreme divergence. Terminating run")
    }
    else if (tconv.max < 0.20 & tratio.max < 0.10) {
      print(ans)
      cat(paste0("Maximum Absolute Value Amongst Convergence t-Ratios: ", tratio.max, "\n"))
      cat(paste0("Model Has Converged After ", numr, " iterations. \n"))
      return(ans)
    }
    else {
      print(ans)
      cat("WARNING: Convergence Inadequate.\n")
      cat(paste0("Overall maximum convergence ratio: ", tconv.max, "\n"))
      cat(paste0("Iteration Number: ", numr), "\n")
      ans <- siena07(alg, data = data, eff = eff, prevAns = ans, returnDeps = TRUE, ...)
    }
  }
}

########################################################################################################################

# MODELS
network_SAOM <- siena07RunToConvergence(dat=siena_network,eff=sienaEffnetwork,alg=algorithm1,
                                        ans0=NULL,modelName='model1.ans',batch=FALSE,verbose=FALSE,
                                        useCluster=TRUE,nbrNodes=3,returnChains=FALSE,returnDataFrame=TRUE)
inculpations_SAOM <- siena07RunToConvergence(dat=siena_inculpations,eff=sienaEffinculpations,alg=algorithm2,
                                             ans0=NULL,modelName='model2.ans',batch=FALSE,verbose=FALSE,
                                             useCluster=TRUE,nbrNodes=3,returnChains=FALSE,returnDataFrame=TRUE)
illicit_speech_SAOM <- siena07RunToConvergence(dat=siena_illicit_speech,eff=sienaEffillicit_speech,alg=algorithm3,
                                             ans0=NULL,modelName='model3.ans',batch=FALSE,verbose=FALSE,
                                             useCluster=TRUE,nbrNodes=3,returnChains=FALSE,returnDataFrame=TRUE)

########################################################################################################################

# RESULTS
# Tabulation of results
model1_results <- data.frame(effects=network_SAOM$effects$effectName,
                             est=network_SAOM$theta,
                             se=network_SAOM$se)
model2_results <- data.frame(effects=inculpations_SAOM$effects$effectName,
                             est=inculpations_SAOM$theta,
                             se=inculpations_SAOM$se)
model3_results <- data.frame(effects=illicit_speech_SAOM$effects$effectName,
                             est=illicit_speech_SAOM$theta,
                             se=illicit_speech_SAOM$se)

# p-values (two-tailed tests)
for(i in 2:nrow(model1_results)){
  model1_results[i,'p'] <- round(2*(1-pnorm(abs(network_SAOM$theta[i])/network_SAOM$se[i])),3) 
}
for(i in 2:nrow(model2_results)){
  model2_results[i,'p'] <- round(2*(1-pnorm(abs(inculpations_SAOM$theta[i])/inculpations_SAOM$se[i])),3) 
}
for(i in 2:nrow(model3_results)){
  model3_results[i,'p'] <- round(2*(1-pnorm(abs(illicit_speech_SAOM$theta[i])/illicit_speech_SAOM$se[i])),3) 
}

# Significance symbols
sig <- function(x){
  ifelse(x < .001,'***',
         ifelse(x < .01,'**',
                ifelse(x < .05,'*',
                       ifelse(x < .1,'+',''))))
}

model1_results$sig <- sig(model1_results$p)
model2_results$sig <- sig(model2_results$p)
model3_results$sig <- sig(model3_results$p)

model1_results
model2_results
model3_results

write.table(model1_results[c(1:3,6,5,4,14,7,8,11,10,9,12,13),],'results SAOM model 1.csv',row.names=FALSE,sep=',')
write.table(model2_results[c(1:3,6,5,4,14,7,8,11,10,9,12,13),],'results SAOM model 2.csv',row.names=FALSE,sep=',')
write.table(model3_results[c(1:3,6,5,4,14,7,8,11,10,9,12,13),],'results SAOM model 3.csv',row.names=FALSE,sep=',')

########################################################################################################################

# GOODNESS OF FIT
algorithm_GOF1 <- sienaAlgorithmCreate(projname='gof1',useStdInits=FALSE,cond=FALSE,nsub=0,n3=1000,simOnly=TRUE,seed=0708) 
algorithm_GOF2 <- sienaAlgorithmCreate(projname='gof2',useStdInits=FALSE,cond=FALSE,nsub=0,n3=1000,simOnly=TRUE,seed=0708) 
algorithm_GOF3 <- sienaAlgorithmCreate(projname='gof3',useStdInits=FALSE,cond=FALSE,nsub=0,n3=1000,simOnly=TRUE,seed=0708) 

siena_GOF1 <- siena07(algorithm_GOF1,dat=siena_network,eff=sienaEffnetwork,
                      batch=TRUE,verbose=TRUE,returnDeps=TRUE,useCluster=TRUE,nbrNodes=3,prevAns=network_SAOM)
siena_GOF2 <- siena07(algorithm_GOF2,dat=siena_inculpations,eff=sienaEffinculpations,
                      batch=TRUE,verbose=TRUE,returnDeps=TRUE,useCluster=TRUE,nbrNodes=3,prevAns=inculpations_SAOM)
siena_GOF3 <- siena07(algorithm_GOF3,dat=siena_illicit_speech,eff=sienaEffillicit_speech,
                      batch=TRUE,verbose=TRUE,returnDeps=TRUE,useCluster=TRUE,nbrNodes=3,prevAns=illicit_speech_SAOM)

# Function to obtain the geodesic distribution
igraphNetworkExtraction <- function(i,data,sims,period,groupName,varName){
  dimsOfDepVar<- attr(data[[groupName]]$depvars[[varName]], 'netdims')
  missings <- is.na(data[[groupName]]$depvars[[varName]][,,period]) |
    is.na(data[[groupName]]$depvars[[varName]][,,period+1])
  if (is.null(i)) {
    # sienaGOF wants the observation:
    original <- data[[groupName]]$depvars[[varName]][,,period+1]
    original[missings] <- 0
    returnValue <- graph.adjacency(original)
  } else {
    missings <- graph.adjacency(missings)
    #sienaGOF wants the i-th simulation:
    returnValue <- graph.difference(graph.edgelist(sims[[i]][[groupName]][[varName]][[period]][,1:2]),missings)
  }
  returnValue
}

GeodesicDistribution <- function(i,data,sims,period,groupName,varName,levls=c(1:5,Inf),cumulative=TRUE,...){
  x <- networkExtraction(i, data, sims, period, groupName, varName)
  a <- sna::geodist(x)$gdist
  if(cumulative){
    gdi <- sapply(levls, function(i){ sum(a<=i) })
  } else {
    gdi <- sapply(levls, function(i){ sum(a==i) })
  }
  names(gdi) <- as.character(levls)
  gdi
}

# Obtention of goodness of fit statistics
model1GOF <-  model2GOF <-  model3GOF <- vector('list',4)

model1GOF[[1]] <- sienaGOF(siena_GOF1,IndegreeDistribution,verbose=TRUE,varName='network',cumulative=FALSE)
model1GOF[[2]] <- sienaGOF(siena_GOF1,OutdegreeDistribution,verbose=TRUE,varName='network',cumulative=FALSE)
model1GOF[[3]] <- sienaGOF(siena_GOF1,TriadCensus,verbose=TRUE,varName='network')
model1GOF[[4]] <- sienaGOF(siena_GOF1,GeodesicDistribution,verbose=TRUE,varName='network',cumulative=FALSE)

model2GOF[[1]] <- sienaGOF(siena_GOF2,IndegreeDistribution,verbose=TRUE,varName='inculpations',cumulative=FALSE)
model2GOF[[2]] <- sienaGOF(siena_GOF2,OutdegreeDistribution,verbose=TRUE,varName='inculpations',cumulative=FALSE)
model2GOF[[3]] <- sienaGOF(siena_GOF2,TriadCensus,verbose=TRUE,varName='inculpations')
model2GOF[[4]] <- sienaGOF(siena_GOF2,GeodesicDistribution,verbose=TRUE,varName='inculpations',cumulative=FALSE)

model3GOF[[1]] <- sienaGOF(siena_GOF3,IndegreeDistribution,verbose=TRUE,varName='illicit_speech',cumulative=FALSE)
model3GOF[[2]] <- sienaGOF(siena_GOF3,OutdegreeDistribution,verbose=TRUE,varName='illicit_speech',cumulative=FALSE)
model3GOF[[3]] <- sienaGOF(siena_GOF3,TriadCensus,verbose=TRUE,varName='illicit_speech')
model3GOF[[4]] <- sienaGOF(siena_GOF3,GeodesicDistribution,verbose=TRUE,varName='illicit_speech',cumulative=FALSE)

# GOF visualisation
for(i in 1:4){
  model1GOF[[i]] <- plot(model1GOF[[i]],scale=TRUE,center=TRUE)
  model2GOF[[i]] <- plot(model2GOF[[i]],scale=TRUE,center=TRUE)
  model3GOF[[i]] <- plot(model3GOF[[i]],scale=TRUE,center=TRUE)
}

jpeg(filename='GOFmodel1.jpeg',width=13,height=7,units='in',res=500)
ggarrange(model1GOF[[1]],model1GOF[[2]],model1GOF[[3]],model1GOF[[4]],
          labels=c('A','B','C','D'),
          ncol=2,nrow=2)
dev.off()

jpeg(filename='GOFmodel2.jpeg',width=13,height=7,units='in',res=500)
ggarrange(model2GOF[[1]],model2GOF[[2]],model2GOF[[3]],model2GOF[[4]],
          labels=c('A','B','C','D'),
          ncol=2,nrow=2)
dev.off()

jpeg(filename='GOFmodel3.jpeg',width=13,height=7,units='in',res=500)
ggarrange(model3GOF[[1]],model3GOF[[2]],model3GOF[[3]],model3GOF[[4]],
          labels=c('A','B','C','D'),
          ncol=2,nrow=2)
dev.off()

########################################################################################################################

# Save image
save.image('Kent_SAOM.RData')