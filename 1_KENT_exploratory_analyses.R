## LOLLARDS DATA
## KENT - Exploratory analysis (1)
## R script written by Jose Luis Estevez (Masaryk University)
## Date: Feb 7th 2022
########################################################################################################################

# LOADING
rm(list=ls())

ties <- readxl::read_excel('Kent Coding AKADB2+edges 2.1.xlsx',sheet='edges') # info on ties
persons <- readxl::read_excel('Kent Persons.xlsx',sheet='Persons') # info on individuals

# Required packages
library(sna);library(igraph);library(ggplot2);library(ggpubr);library(ggraph)

########################################################################################################################

# EXPLORATION
class(ties);class(persons) 
dim(ties);dim(persons) # 502 ties, 79 individuals mentioned

# The 53 defendants + plus 26 subjects (some of them, their identity is unknown)
defendants <- persons[!is.na(persons$deponent) & persons$deponent == 1,]
others <- persons[is.na(persons$deponent) | persons$deponent != 1,]
# Some of them, we do not even know their names
others[is.na(others$family_name) | others$family_name == '(unknown)' | others$name == '(unknown)',
       c('id','label','name','family_name')]

# Proportion of info available
sum(!is.na(defendants$origin_or_residence)) # Residence of 51 of the 53 defendants
sum(!is.na(others$origin_or_residence)) # Residence of 9 of the 26 remaining subjects (34.6%)
sum(!is.na(defendants$occupation_type)) # Occupation of 11 defendants 
sum(!is.na(others$occupation_type)) # Occupation of 4 of the 26 remaining subjects (15.4%)
sum(!is.na(defendants$age_tens)) # Age of 16 defendants
sum(!is.na(others$age_tens)) # Age of 1 of the 26 remaining subjects

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

# ENLARGE NETWORK, INCULPATIONS, AND ILLICIT SPEECH TO 79 SUBJECTS
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
}

########################################################################################################################

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
}

########################################################################################################################

# VISUALISATIONS
# Creation of a common layout for all the network visuals
# Sum of all ties
alltogether <- array(NA,dim = c(79,79,3),dimnames=list(rownames(network),colnames(network),1:3))
alltogether[,,1] <- network 
alltogether[,,2] <- colocation
alltogether[,,3] <- kinship
alltogether <- apply(alltogether,c(1,2),max,na.rm=TRUE)

alltogether <- graph_from_adjacency_matrix(alltogether,mode='undirected')
set.seed(1234)
lyt <- layout_with_fr(alltogether)

jpeg(filename='Inculpations.jpeg',width=15,height=15,units='in',res=500)
plot(igraph_objs$inculpations,
     vertex.color = ifelse(V(igraph_objs$inculpations)$sex == 'Woman', "tomato", "grey"),
     vertex.shape = ifelse(V(igraph_objs$inculpations)$sentenced == 'Yes', "square", "circle"),
     vertex.size = 5,
     vertex.label.color = 'black',
     edge.color = "black",
     edge.width = 1,
     edge.arrow.size = 0.5,
     layout = lyt,
     mark.groups = cluster_louvain(igraph_objs$colocation),
     main = "Inculpations")
dev.off()

jpeg(filename='Network.jpeg',width=15,height=15,units='in',res=500)
plot(igraph_objs$network,
     vertex.color = ifelse(V(igraph_objs$network)$sex == 'Woman', "tomato", "grey"),
     vertex.shape = ifelse(V(igraph_objs$network)$sentenced == 'Yes', "square", "circle"),
     vertex.size = 5,
     vertex.label.size = 1,
     vertex.label.color = 'black',
     edge.color = "black",
     edge.width = 1,
     edge.arrow.size = 0.5,
     layout = lyt,
     mark.groups = cluster_louvain(igraph_objs$colocation),
     main = "Social Network reported")
dev.off()

jpeg(filename='Illicit_speech.jpeg',width=15,height=15,units='in',res=500)
plot(igraph_objs$illicit_speech,
     vertex.color = ifelse(V(igraph_objs$network)$sex == 'Woman', "tomato", "grey"),
     vertex.shape = ifelse(V(igraph_objs$network)$sentenced == 'Yes', "square", "circle"),
     vertex.size = 5,
     vertex.label.size = 1,
     vertex.label.color = 'black',
     edge.color = "black",
     edge.width = 1,
     edge.arrow.size = 0.5,
     layout = lyt,
     mark.groups = cluster_louvain(igraph_objs$colocation),
     main = "Illicit speech")
dev.off()

jpeg(filename='Kinship.jpeg',width=15,height=15,units='in',res=500)
plot(igraph_objs$kinship,
     vertex.color = ifelse(V(igraph_objs$kinship)$sex == 'Woman', "tomato", "grey"),
     vertex.shape = ifelse(V(igraph_objs$kinship)$sentenced == 'Yes', "square", "circle"),
     vertex.size = 5,
     vertex.label.color = 'black',
     edge.color = "black",
     edge.width = 1,
     layout = lyt,
     mark.groups = cluster_louvain(igraph_objs$colocation),
     main = "Kinship ties")
dev.off()

########################################################################################################################

# REDUCED TO ONLY THE 53 DEFENDANTS

# Eliminate non-defendants from the igraph objects
igraph_objs_red <- igraph_objs

for(i in seq_along(igraph_objs_red)){
  igraph_objs_red[[i]] <- delete_vertices(igraph_objs_red[[i]],V(igraph_objs_red[[i]])$defendant == 'No')
}

lyt_red <- lyt[V(igraph_objs$network)$defendant == 'Yes',] # layout without non-defendants

jpeg(filename='Inculpations_red.jpeg',width=15,height=15,units='in',res=500)
plot(igraph_objs_red$inculpations,
     vertex.color = ifelse(V(igraph_objs_red$inculpations)$sex == 'Woman', "tomato", "grey"),
     vertex.shape = ifelse(V(igraph_objs_red$inculpations)$sentenced == 'Yes', "square", "circle"),
     vertex.size = 5,
     vertex.label.color = 'black',
     edge.color = "black",
     edge.width = 1,
     edge.arrow.size = 0.5,
     layout = lyt_red,
     mark.groups = cluster_louvain(igraph_objs_red$colocation),
     main = "Inculpations (reduced)")
dev.off()

jpeg(filename='Network_red.jpeg',width=15,height=15,units='in',res=500)
plot(igraph_objs_red$network,
     vertex.color = ifelse(V(igraph_objs_red$network)$sex == 'Woman', "tomato", "grey"),
     vertex.shape = ifelse(V(igraph_objs_red$network)$sentenced == 'Yes', "square", "circle"),
     vertex.size = 5,
     vertex.label.color = 'black',
     edge.color = "black",
     edge.width = 1,
     edge.arrow.size = 0.5,
     layout = lyt_red,
     mark.groups = cluster_louvain(igraph_objs_red$colocation),
     main = "Social Network reported (reduced)")
dev.off()

jpeg(filename='Illicit_speech_red.jpeg',width=15,height=15,units='in',res=500)
plot(igraph_objs_red$illicit_speech,
     vertex.color = ifelse(V(igraph_objs_red$network)$sex == 'Woman', "tomato", "grey"),
     vertex.shape = ifelse(V(igraph_objs_red$network)$sentenced == 'Yes', "square", "circle"),
     vertex.size = 5,
     vertex.label.color = 'black',
     edge.color = "black",
     edge.width = 1,
     edge.arrow.size = 0.5,
     layout = lyt_red,
     mark.groups = cluster_louvain(igraph_objs_red$colocation),
     main = "Illicit speech (reduced)")
dev.off()

jpeg(filename='Kinship_red.jpeg',width=15,height=15,units='in',res=500)
plot(igraph_objs_red$kinship,
     vertex.color = ifelse(V(igraph_objs_red$kinship)$sex == 'Woman', "tomato", "grey"),
     vertex.shape = ifelse(V(igraph_objs_red$kinship)$sentenced == 'Yes', "square", "circle"),
     vertex.size = 5,
     vertex.label.color = 'black',
     edge.color = "black",
     edge.width = 1,
     layout = lyt_red,
     mark.groups = cluster_louvain(igraph_objs_red$colocation),
     main = "Kinship ties (reduced)")
dev.off()

########################################################################################################################

# EXTRACION OF THE PART OF THE NETWORK REPORTED BY EACH DEPONENT OR WITNESS

# Dissidence network by reporter
# Get the IDs of those who reported others (witnesses)
reporters <- unique(ties_knownsource$deponent)
reporters <- reporters[order(reporters)]

# Split the data by reporter
subntw <- vector('list',length=length(reporters))
names(subntw) <- reporters

for(i in names(subntw)){
  subntw[[i]] <- ties_knownsource[ties_knownsource$deponent == i,c('from','to')]
}

# Remove dublicated ties
for(i in seq_along(subntw)){
  subntw[[i]] <- subntw[[i]][!duplicated(subntw[[i]]),]
}

subgraph <- subntw
for(i in seq_along(subgraph)){
  subgraph[[i]] <- graph_from_edgelist(as.matrix(subgraph[[i]]),directed=FALSE)
  subgraph[[i]] <- 1*(as.matrix(get.adjacency(subgraph[[i]])) != 0)
}

# Large matrices
subgraph_enl <- subntw
for(i in seq_along(subgraph_enl)){
  subgraph_enl[[i]] <- network*0
}

for(x in seq_along(subgraph)){
  for(i in rownames(subgraph[[x]])){
    for(j in colnames(subgraph[[x]])){
      subgraph_enl[[x]][i,j] <- subgraph[[x]][i,j]
    }
  }
}

for(i in names(subgraph_enl)){
  subgraph_enl[[i]] <- graph_from_adjacency_matrix(subgraph_enl[[i]],mode='undirected')
  V(subgraph_enl[[i]])$name <- persons$fullnames
  V(subgraph_enl[[i]])$reporter <- 1*(persons$id == i)
}

# Visualisation
subgraph_plot <- subgraph_enl 
for(i in seq_along(subgraph_plot)){
  subgraph_plot[[i]] <- ggraph(subgraph_plot[[i]],layout=lyt)+
    geom_edge_link(colour='blue',width=.5,alpha=1)+
    geom_node_point(aes(colour=as.factor(reporter),fill=as.factor(reporter)),size=5)+
    scale_colour_manual(values=c('0'="gray",'1'="tomato"))+
    scale_fill_manual(values=c('0'="gray",'1'="tomato"))+
    geom_node_text(aes(label=name),size=2.5,repel=TRUE)
}

jpeg(filename='Sub_networks.jpeg',width=30,height=20,units='in',res=500)
ggarrange(subgraph_plot[[1]],subgraph_plot[[2]],subgraph_plot[[3]],subgraph_plot[[4]],subgraph_plot[[5]],
          subgraph_plot[[6]],subgraph_plot[[7]],subgraph_plot[[8]],subgraph_plot[[9]],subgraph_plot[[10]],
          subgraph_plot[[11]],subgraph_plot[[12]],subgraph_plot[[13]],subgraph_plot[[14]],subgraph_plot[[15]],
          labels=V(subgraph_enl$P01)$name[match(names(subgraph_plot),persons$id)],
          common.legend = TRUE,
          nrow=3,ncol=5)
dev.off()

########################################################################################################################

# Removal of unnecessary objects
rm(alltogether);rm(others);rm(defendants);rm(subgraph);rm(subgraph_plot);rm(ties);rm(ties_knownsource)
rm(ties_unknownsource);rm(i);rm(j);rm(reporters);rm(x);rm(subntw);rm(igraph_objs_red)
rm(unidirect);rm(mutuals);rm(threats);rm(ntw1);rm(ntw2);rm(illicit1);rm(illicit2)

# Save image
save.image('Kent_data.RData')