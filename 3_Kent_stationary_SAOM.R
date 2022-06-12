## LOLLARDS DATA
## KENT - Stationary SAOM (3)
## R script written by Jose Luis Estevez (Masaryk University)
## Date: June 9th 2022
########################################################################################################################

# LOADING
rm(list=ls())
load('Kent_data.RData')

# Required packages
library(igraph);library(RSiena);library(ggpubr)

########################################################################################################################

# let's change one tie for RSiena to work
naming_mtx0 <- naming_mtx

set.seed(0708)
naming_mtx0[!is.na(naming_mtx0) & naming_mtx0 == 1][sample(1:length(naming_mtx0[!is.na(naming_mtx0) & naming_mtx0 == 1]),1)] <- 0
naming_mtx0[!is.na(naming_mtx0) & naming_mtx0 == 0][sample(1:length(naming_mtx0[!is.na(naming_mtx0) & naming_mtx0 == 0]),1)] <- 1

########################################################################################################################

# SIENA OBJECT CREATION
siena_naming <- sienaDataCreate(
  naming = sienaNet(array(c(naming_mtx0,naming_mtx),dim=c(nrow(naming_mtx),ncol(naming_mtx),2))),
  kinship = coDyadCovar(kinship_mtx),
  same_settlement = coDyadCovar(same_settlement_mtx),
  sex = coCovar(ifelse(defendants_att$sex == 'f',1,0),centered=TRUE),
  witness = coCovar(defendants_att$witness_against_impenitents,centered=TRUE),
  impenitent = coCovar(defendants_att$impenitent,centered=TRUE)
)

########################################################################################################################

# SIENA EFFECTS

sienaEff_naming <- getEffects(siena_naming)
sienaEff_naming <- includeEffects(sienaEff_naming,gwespFF,name='naming')
sienaEff_naming <- includeInteraction(sienaEff_naming,recip,gwespFF,parameter=69)
sienaEff_naming <- includeEffects(sienaEff_naming,X,interaction1 = 'kinship')
sienaEff_naming <- includeEffects(sienaEff_naming,X,interaction1 = 'same_settlement')
sienaEff_naming <- includeEffects(sienaEff_naming,altX,egoX,sameX,interaction1 = 'sex')
sienaEff_naming <- includeEffects(sienaEff_naming,altX,interaction1 = 'witness')
sienaEff_naming <- includeEffects(sienaEff_naming,altX,interaction1 = 'impenitent')
sienaEff_naming <- setEffect(sienaEff_naming,Rate,initialValue=5,name='naming',fix=TRUE,type="rate")

########################################################################################################################

# ALGORITHM

algorithm <- sienaAlgorithmCreate(projname='siena_naming',useStdInits=FALSE,cond=FALSE,nsub=4,n3=5000,seed=0708)

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

# MODEL
naming_SAOM <- siena07RunToConvergence(dat=siena_naming,eff=sienaEff_naming,alg=algorithm,
                                       ans0=NULL,modelName='model.ans',batch=FALSE,verbose=FALSE,
                                       useCluster=TRUE,nbrNodes=3,returnChains=FALSE,returnDataFrame=TRUE)

########################################################################################################################

# RESULTS
# Tabulation of results
model_results <-  data.frame(effects=naming_SAOM$effects$effectName,
                             est=naming_SAOM$theta,
                             se=naming_SAOM$se)

# p-values (two-tailed tests)
for(i in 2:nrow(model_results)){
  model_results[i,'p'] <- round(2*(1-pnorm(abs(naming_SAOM$theta[i])/naming_SAOM$se[i])),3) 
}

# Significance symbols
sig <- function(x){
  ifelse(x < .001,'***',
         ifelse(x < .01,'**',
                ifelse(x < .05,'*',
                       ifelse(x < .1,'+',''))))
}

model_results$sig <- sig(model_results$p)
model_results

write.table(model_results,'results SAOM.csv',row.names=FALSE,sep=',')

########################################################################################################################

# GOODNESS OF FIT
algorithm_GOF <- sienaAlgorithmCreate(projname='gof',useStdInits=FALSE,cond=FALSE,nsub=0,n3=1000,simOnly=TRUE,seed=0708) 

siena_GOF <- siena07(algorithm_GOF,dat=siena_naming,eff=sienaEff_naming,
                      batch=TRUE,verbose=TRUE,returnDeps=TRUE,useCluster=TRUE,nbrNodes=3,prevAns=naming_SAOM)

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
modelGOF <- vector('list',4)

modelGOF[[1]] <- sienaGOF(siena_GOF,IndegreeDistribution,verbose=TRUE,varName='naming',cumulative=FALSE)
modelGOF[[2]] <- sienaGOF(siena_GOF,OutdegreeDistribution,verbose=TRUE,varName='naming',cumulative=FALSE)
modelGOF[[3]] <- sienaGOF(siena_GOF,TriadCensus,verbose=TRUE,varName='naming')
modelGOF[[4]] <- sienaGOF(siena_GOF,GeodesicDistribution,verbose=TRUE,varName='naming',cumulative=FALSE)

# GOF visualisation
for(i in 1:4){
  modelGOF[[i]] <- plot(modelGOF[[i]],scale=TRUE,center=TRUE)
}

jpeg(filename='SAOM_gof.jpeg',width=13,height=7,units='in',res=1000)
ggarrange(modelGOF[[1]],modelGOF[[2]],modelGOF[[3]],modelGOF[[4]],
          labels=c('A','B','C','D'),
          ncol=2,nrow=2)
dev.off()

########################################################################################################################

# Save image
save.image('Kent_SAOM.RData')
