## LOLLARDS DATA
## KENT - Exploratory and descriptive analyses (1)
## R script written by Jose Luis Estevez (Masaryk University)
## Date: March 19th 2022
########################################################################################################################

# DATA LOADING
rm(list=ls())
ties <- readxl::read_excel('Kent Coding AKADB2+edges 2.1.xlsx',sheet='edges') # info on ties
persons <- readxl::read_excel('Kent Persons.xlsx',sheet='Persons') # info on individuals
persons <- persons[!(persons$label == 'William Warham'),] # Let's remove the Inquisitor out of the sample

# Required packages
library(sna);library(igraph);library(ggplot2);library(isnar)

########################################################################################################################

# FIRST EXPLORATION

class(ties);class(persons) 
dim(ties);dim(persons) # 502 ties, 78 individuals mentioned

# The 53 defendants + plus 25 subjects (some of them, their identity is unknown)
defendants <- persons[!is.na(persons$deponent) & persons$deponent == 1,]
others <- persons[is.na(persons$deponent) | persons$deponent != 1,]
# Some of them, we do not even know their names
others[is.na(others$family_name) | others$family_name == '(unknown)' | others$name == '(unknown)',
       c('id','label','name','family_name')]

# Proportion of info available
sum(!is.na(defendants$origin_or_residence)) # Residence of 51 of the 53 defendants
sum(!is.na(others$origin_or_residence)) # Residence of 9 of the 25 remaining subjects 
sum(!is.na(defendants$occupation_type)) # Occupation of 11 defendants 
sum(!is.na(others$occupation_type)) # Occupation of 3 of the 25 remaining subjects 
sum(!is.na(defendants$age_tens)) # Age of 16 defendants
sum(!is.na(others$age_tens)) # Age of 1 of the 25 remaining subjects

persons[is.na(persons$sex),c('name','family_name','sex')] # there are 5 people with sex missing (all them male)
persons$sex[is.na(persons$sex)] <- 'm' # I correct this

# Information about ties
names(ties) # ('from', 'to', type of tie 'arsubtype1', whether directed or undirected, and who reported those ties 'deponent')
ties <- ties[,c('from','to','arsubtype1','arsubtype2','arsubtype3','arsubtype4','direction','deponent')]

'%!in%' <- function(x,y)!('%in%'(x,y)) # Function 'not in'
ties_knownsource <- ties[ties$deponent %in% defendants$id,]
ties_unknownsource <- ties[ties$deponent %!in% defendants$id,]

unique(ties_knownsource$arsubtype1) # type of ties reported (no kinship or collocation)
ties_knownsource <- ties_knownsource[ties_knownsource$arsubtype1 != 'work',] # remove working together as a tie
ties_knownsource <- ties_knownsource[ties_knownsource$arsubtype1 != 'transfer',] # remove transfer as a tie
threats <- which(!is.na(ties_knownsource$arsubtype3) & ties_knownsource$arsubtype3 == 'threatening')
ties_knownsource <- ties_knownsource[-threats,] # remove threats as a tie

# Reverse the direction of ties capturing 'hearing'
ties_knownsource$target <- ties_knownsource$source <- NA

for(i in 1:nrow(ties_knownsource)){
  if(!is.na(ties_knownsource$arsubtype3[i]) & ties_knownsource$arsubtype3[i] == 'hearing'){
    ties_knownsource$source[i] <- ties_knownsource$to[i]
    ties_knownsource$target[i] <- ties_knownsource$from[i]
  }else{
    ties_knownsource$source[i] <- ties_knownsource$from[i]
    ties_knownsource$target[i] <- ties_knownsource$to[i]
  }
}

ties_knownsource$from <- ties_knownsource$source
ties_knownsource$to <- ties_knownsource$target

########################################################################################################################

# EXTRACTION OF TIES AND INCULPATIONS

# let's divide ties between mutual (being together, having a meal, etc.) and unidirectional (hearing, saying, etc.)
mutuals <- ties_knownsource[ties_knownsource$arsubtype1 != 'flow' | (ties_knownsource$arsubtype1 == 'flow' & ties_knownsource$arsubtype3 == 'conversation'),]
unidirect <- ties_knownsource[ties_knownsource$arsubtype1 == 'flow' & ties_knownsource$arsubtype3 != 'conversation',]
unique(unidirect$arsubtype3) 

# Obtaining social network
ntw1 <- graph_from_edgelist(as.matrix(mutuals[,c('from','to')]),directed=FALSE) # mutual ties
ntw1 <- 1*(as.matrix(get.adjacency(ntw1)) != 0)
ntw2 <- graph_from_edgelist(as.matrix(unidirect[,c('from','to')]),directed=TRUE) # unidirectional ties
ntw2 <- 1*(as.matrix(get.adjacency(ntw2)) != 0)

for(i in rownames(ntw2)){
  for(j in colnames(ntw2)){
    if(ntw2[i,j] == 1){
      ntw1[i,j] <- ntw2[i,j]
    }
  }
}

network <- ntw1
sum(network)

# Obtaining inculpations (including self-inculpations)
inculpations <- tidyr::gather(ties_knownsource[,c('from','to','deponent')],
                              key='direction',value='target',-c('deponent'))
inculpations <- graph_from_edgelist(as.matrix(inculpations[,c('deponent','target')]),directed=TRUE)
inculpations <- 1*(as.matrix(get.adjacency(inculpations)) != 0)
sum(inculpations) # 85
sum(inculpations) - sum(diag(inculpations)) # 70 inculpations plus 15 self-inculpations
diag(inculpations) <- 0
# People whose deposition is missing could not inculpate anybody, so NA
inculpations[rowSums(inculpations) == 0,] <- NA

# Explicit illicit speech only
illicit_speech <- ties_knownsource[ties_knownsource$arsubtype1 %in% c('flow','reading'),]
mutuals <- illicit_speech[illicit_speech$arsubtype1 != 'flow' | (illicit_speech$arsubtype1 == 'flow' & illicit_speech$arsubtype3 == 'conversation'),]
unidirect <- illicit_speech[illicit_speech$arsubtype1 == 'flow' & illicit_speech$arsubtype3 != 'conversation',]

# Obtaining the illicit speech network
illicit1 <- graph_from_edgelist(as.matrix(mutuals[,c('from','to')]),directed=FALSE) # mutual ties
illicit1 <- 1*(as.matrix(get.adjacency(illicit1)) != 0)
illicit2 <- graph_from_edgelist(as.matrix(unidirect[,c('from','to')]),directed=TRUE) # unidirectional ties
illicit2 <- 1*(as.matrix(get.adjacency(illicit2)) != 0)

illicit_speech <- network*0

for(i in rownames(illicit1)){
  for(j in colnames(illicit1)){
    if(illicit1[i,j] == 1){
      illicit_speech[i,j] <- illicit1[i,j]
    }
  }
}

for(i in rownames(illicit2)){
  for(j in colnames(illicit2)){
    if(illicit2[i,j] == 1){
      illicit_speech[i,j] <- illicit2[i,j]
    }
  }
}

sum(illicit_speech)

########################################################################################################################

# CREATION OF COLOCATION (SAME RESIDENCE) AND KINSHIP MATRICES

# Colocation
unique(persons$origin_or_residence) # 17 different locations
summary(as.factor(persons$origin_or_residence)) # Only in 12, at least two subjects
colocation <- 1*outer(persons$origin_or_residence, persons$origin_or_residence,'==')
rownames(colocation) <- colnames(colocation) <- persons$id
diag(colocation) <- 0

# Kinship
persons$kinship <- persons$family_name
persons$kinship[persons$kinship == '(unknown)'] <- NA
persons$kinship[!is.na(persons$kinship) & persons$kinship == 'Raynold'] <- 'Reignold' # different spelling obut same surname
unique(persons$kinship) # 41 different family names
summary(as.factor(persons$kinship)) # 18 family names at least shared by two subjects
# Since Agnes Ive is sister of Robert Hilles (see Tanner, p. xviii), I related these two families
persons$kinship[!is.na(persons$kinship) & persons$kinship == 'Ive'] <- 'Hilles' 
kinship <- 1*outer(persons$kinship, persons$kinship,'==')
rownames(kinship) <- colnames(kinship) <- persons$id
diag(kinship) <- 0

# Number of colocation and kinship ties
sum(colocation,na.rm = TRUE)/2 # 151
sum(kinship,na.rm = TRUE)/2 # 57

########################################################################################################################

# ENLARGE NETWORK, INCULPATIONS, AND ILLICIT SPEECH TO 78 SUBJECTS

mtx <- kinship*0
mtx[is.na(mtx)] <- 0

for(i in rownames(network)){
  for(j in colnames(network)){
    mtx[i,j] <- network[i,j]
  }
}

network <- mtx # network

mtx <- mtx*0

for(i in rownames(inculpations)){
  for(j in colnames(inculpations)){
    mtx[i,j] <- inculpations[i,j]
  }
}

inculpations <- mtx # inculpations
# Those whose deposition is missing, to NA
inculpations[is.na(rowSums(inculpations)) | rowSums(inculpations) == 0,] <- NA

mtx <- mtx*0

for(i in rownames(illicit_speech)){
  for(j in colnames(illicit_speech)){
    mtx[i,j] <- illicit_speech[i,j]
  }
}

illicit_speech <- mtx # illicit speech
rm(mtx)

########################################################################################################################

# CREATION OF 'NETWORK' OBJECTS

ntw_objs <- list()

ntw_objs[['network']] <- network(network,directed=TRUE)
ntw_objs[['inculpations']] <- network(inculpations,directed=TRUE)
ntw_objs[['illicit_speech']] <- network(illicit_speech,directed=TRUE)
ntw_objs[['colocation']] <- network(colocation,directed=FALSE)
ntw_objs[['kinship']] <- network(kinship,directed=FALSE)

# Addition of attributes (name, sex, defendant (53), witness (15), and sentenced to dead (5))
persons$fullnames <- paste(persons$family_name,persons$name,sep=', ')
persons$fullnames[persons$fullnames %in% c('(unknown), (unknown)', 'NA, NA')] <- '(unknown)' # full names
persons$witness <- 1*(persons$id %in% unique(ties$deponent)) # The 15 witnesses

for(i in seq_along(ntw_objs)){
  network::set.vertex.attribute(ntw_objs[[i]],'vertex.names',persons$fullnames)
  network::set.vertex.attribute(ntw_objs[[i]],'sex',ifelse(persons$sex=='m','Man','Woman'))
  network::set.vertex.attribute(ntw_objs[[i]],'defendant',ifelse(!is.na(persons$deponent) & persons$deponent==1,'Yes','No'))
  network::set.vertex.attribute(ntw_objs[[i]],'witness',ifelse(persons$witness==1,'Yes','No'))
  network::set.vertex.attribute(ntw_objs[[i]],'sentenced',ifelse(persons$defendant==1,'Yes','No'))
  network::set.vertex.attribute(ntw_objs[[i]],'kinship',persons$kinship)
  network::set.vertex.attribute(ntw_objs[[i]],'colocation',persons$origin_or_residence)
  
}

# CREATION OF 'IGRAPH' OBJECTS

igraph_objs <- list()

igraph_objs[['network']] <- graph_from_adjacency_matrix(network,mode='directed')
igraph_objs[['inculpations']]<- graph_from_adjacency_matrix(inculpations,mode='directed')
igraph_objs[['illicit_speech']]<- graph_from_adjacency_matrix(illicit_speech,mode='directed')
igraph_objs[['colocation']] <- graph_from_adjacency_matrix(colocation,mode='undirected')
igraph_objs[['kinship']] <- graph_from_adjacency_matrix(kinship,mode='undirected')

# Addition of attributes (name, sex, defendant (53), witness (15), and sentenced to dead (5))
for(i in seq_along(igraph_objs)){
  V(igraph_objs[[i]])$name <- persons$fullnames
  V(igraph_objs[[i]])$sex <- ifelse(persons$sex=='m','Man','Woman')
  V(igraph_objs[[i]])$defendant <- ifelse(!is.na(persons$deponent) & persons$deponent==1,'Yes','No')
  V(igraph_objs[[i]])$witness <- ifelse(persons$witness==1,'Yes','No')
  V(igraph_objs[[i]])$sentenced <- ifelse(persons$defendant==1,'Yes','No')
  V(igraph_objs[[i]])$kinship <- persons$kinship
  V(igraph_objs[[i]])$colocation <- persons$origin_or_residence
}

########################################################################################################################

# REDUCED TO ONLY THE 53 DEFENDANTS

# Let's reduce the network to only defendants
# In network objects
for(i in seq_along(ntw_objs)){
  network::delete.vertices(ntw_objs[[i]],vid=which(ntw_objs[[i]] %v% 'defendant' == 'No'))
}
ntw_objs # Checking

# In igraph objects
for(i in seq_along(igraph_objs)){
  igraph_objs[[i]] <- delete_vertices(igraph_objs[[i]],V(igraph_objs[[i]])$defendant == 'No')
}
igraph_objs # Checking

# In matrices
deponents <- persons[!is.na(persons$deponent) & persons$deponent == 1,]$id
illicit_speech <- illicit_speech[rownames(illicit_speech) %in% deponents,colnames(illicit_speech) %in% deponents]
inculpations <- inculpations[rownames(inculpations) %in% deponents,colnames(inculpations) %in% deponents]
kinship <- kinship[rownames(kinship) %in% deponents,colnames(kinship) %in% deponents]
colocation <- colocation[rownames(colocation) %in% deponents,colnames(colocation) %in% deponents]
network <- network[rownames(network) %in% deponents,colnames(network) %in% deponents]

########################################################################################################################

# VISUALISATIONS

# Creation of a common layout for all the network visuals
# Sum of all ties
alltogether <- array(NA,dim = c(53,53,3),dimnames=list(rownames(network),colnames(network),1:3))
alltogether[,,1] <- network 
alltogether[,,2] <- colocation
alltogether[,,3] <- kinship
alltogether <- apply(alltogether,c(1,2),max,na.rm=TRUE)

alltogether <- graph_from_adjacency_matrix(alltogether,mode='undirected')
set.seed(1234)
lyt <- layout_with_fr(alltogether)

jpeg(filename='Kinship ties and inculpations.jpeg',width=18,height=18,units='in',res=1000)
par(mfrow=c(1,2))
plot(igraph_objs$kinship,
     vertex.size = 5,
     vertex.label.color = 'black',
     edge.color = "black",
     edge.width = 1,
     layout = lyt,
     mark.groups = cluster_louvain(igraph_objs$colocation),
     main = "Kinship ties and same village (groups)")
plot(igraph_objs$inculpations,
     vertex.color = ifelse(V(igraph_objs$inculpations)$sentenced == 'Yes', "firebrick3",
                           ifelse(V(igraph_objs$inculpations)$witness == 'Yes',"dodgerblue","grey")),
     vertex.size = 5,
     vertex.label.color = 'black',
     edge.color = "darkred",
     edge.width = 1,
     edge.arrow.size = 0.5,
     layout = lyt,
     main = "Inculpations")
legend("bottomright",bty="o",legend=c('Under trial','Deponents','Other suspects'),
       fill=c('firebrick3','dodgerblue','grey'))
dev.off()

########################################################################################################################

# DESCRIPTIVE OF THE NETWORKS

ntw_descript <- as.data.frame(matrix(NA,nrow=14,ncol=5,
                                     dimnames = list(c('N','ties','den','ave deg','max outdeg','max indeg','recip',
                                                       'trans','iso','deg centr','EI (sex)','EI (family name)',
                                                       'EI (village)','EI (deponent)'),
                                                     c('network','inculpation','illicit speech','colocation','kinship'))))

for(i in seq_along(ntw_objs)){
  ntw_descript[1,i] <- vcount(igraph_objs[[i]])
  ntw_descript[2,i] <- ecount(igraph_objs[[i]])
  ntw_descript[3,i] <- edge_density(igraph_objs[[i]])*100
  ntw_descript[4,i] <- mean(sna::degree(ntw_objs[[i]],cmode='outdegree'))
  ntw_descript[5,i] <- max(sna::degree(ntw_objs[[i]],cmode='outdegree'))
  ntw_descript[6,i] <- max(sna::degree(ntw_objs[[i]],cmode='indegree'))
  ntw_descript[7,i] <- reciprocity(igraph_objs[[i]])*100
  ntw_descript[8,i] <- transitivity(igraph_objs[[i]])*100
  ntw_descript[9,i] <- sum(components(igraph_objs[[i]])$csize == 1)
  ntw_descript[10,i] <- centr_degree(igraph_objs[[i]],mode='total')$centralization
  ntw_descript[11,i] <- ei(igraph_objs[[i]],'sex')
  ntw_descript[12,i] <- ei(igraph_objs[[i]],'kinship')
  ntw_descript[13,i] <- ei(igraph_objs[[i]],'colocation')
  ntw_descript[14,i] <- ei(igraph_objs[[i]],'witness')
}

round(ntw_descript[,c('inculpation','kinship','colocation')],2)
write.table(ntw_descript[,c('inculpation','kinship','colocation')],'ntw_descript.csv',row.names=TRUE,sep=',')

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

biv_descript <- as.data.frame(matrix(NA,nrow=5,ncol=5,
                                     dimnames = list( c('network','inculpation','illicit speech','colocation','kinship'),
                                                      c('network','inculpation','illicit speech','colocation','kinship'))))
mtx <- list(network,inculpations,illicit_speech,colocation,kinship)
names(mtx) <- rownames(biv_descript)

for(i in rownames(biv_descript)){
  for(j in colnames(biv_descript)){
    biv_descript[i,j] <- Jaccard(mtx[[i]],mtx[[j]])*100
  }
}

round(biv_descript[c('inculpation','kinship','colocation'),c('inculpation','kinship','colocation')],1)
write.table(biv_descript[c('inculpation','kinship','colocation'),c('inculpation','kinship','colocation')],
            'biv_descript.csv',row.names=TRUE,sep=',')

########################################################################################################################

# Removal of unnecessary objects
rm(alltogether);rm(others);rm(defendants);rm(ties);rm(ties_knownsource);rm(ties_unknownsource);rm(i);rm(j)
rm(unidirect);rm(mutuals);rm(threats);rm(ntw1);rm(ntw2);rm(illicit1);rm(illicit2);rm(mtx);rm(Jaccard)

# Save image
save.image('Kent_data.RData')