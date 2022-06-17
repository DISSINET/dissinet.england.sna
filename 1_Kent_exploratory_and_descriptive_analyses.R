## LOLLARDS DATA
## KENT - Exploratory and descriptive analyses (1)
## R script written by Jose Luis Estevez (Masaryk University)
## Date: June 15th 2022
########################################################################################################################

# Required packages
library(igraph);library(sna);library(data.table):library(isnar)

# DATA LOADING
rm(list=ls())

# Information about individuals (attributes)
persons <- readxl::read_excel('Kent Persons.xlsx',sheet='Persons') 
persons <- persons[!(persons$label == 'William Warham'),] # Let's remove the Inquisitor out of the sample
persons <- as.data.table(persons)

# Information on on incriminations and other types of ties (like kinship)
ties <- readxl::read_excel('Kent Coding AKADB2+edges 2.2 (June 2022).xlsx',
                   sheet='edges')
ties <- as.data.table(ties)

# Information on crimes (charges) and penances (punishments)
crimes_and_penances <- readxl::read_excel('Kent crime and punishment data.xlsx',sheet='Data')

# NUMBER OF INDIVIDUALS AND TIES AVAILABLE
dim(persons);dim(ties) # 79 individuals, 570 ties

########################################################################################################################

# FIRST EXPLORATION

# In the record, Tanner identified 53 defendants (but there are more individuals in the data: up to 79)
# Are there differences between these 53 and the rest? (note in the data these defendants are referred to as deponents)
persons <- persons[,tanners_defendant := ifelse(is.na(persons$defendant) | persons$defendant == '0',0,1)] # Tanner's defendants

# For example, how much do we know about defendants compared to non-defendats
persons[,.(perc_sex_known = sum(!is.na(sex))/length(sex)*100, # sex
           perc_age_known = sum(!is.na(age_tens))/length(age_tens)*100, # age (approx.)
           perc_settlement_known = sum(!is.na(origin_or_residence))/length(origin_or_residence)*100, # settlement
           perc_job_known = sum(!is.na(occupation_type))/length(occupation_type)*100), # occupation
        keyby=tanners_defendant] # by whether Tanner categorized them or not as defendants
# We see that we have far more information about defendants than about non-defendants

defendants_id <- persons[persons$tanners_defendant == 1,]$id # the defendants' IDs

########################################################################################################################

# SELECTION OF TIES: NAMING (who named whom in trial) and KINSHIP

# Information about ties
names(ties) # ('from', 'to', type of tie 'arsubtype1', whether directed or undirected, and who reported those ties 'deponent')
ties <- ties[,c('from','to','arsubtype1','arsubtype2','arsubtype3','arsubtype4','direction','deponent_id')]
names(ties)[8] <- 'deponent'

kinship <- ties[ties$arsubtype1 == 'social relation',] # Kinship ties for later

# Naming ties
# There is one tie whose source (the deponent) is unknown
ties <- ties[ties$deponent %in% defendants_id,] # We only keep ties whose deponent we know

# Now, we will create ties: who names whom (no matter is as the source (from) or as the target (to))
naming1 <- ties[,.(deponent,from)]
naming2 <- ties[,.(deponent,to)]
naming1 <- setnames(naming1,c('deponent','from'),c('deponent','to'))

naming_ties <- rbind(naming1,naming2) # all naming ties
rm(naming1);rm(naming2)
naming_ties <- naming_ties[!duplicated(naming_ties),] # let's remove duplicated 
naming_ties <- naming_ties[!(naming_ties$deponent == naming_ties$to),] # and let's remove loops (78 ties)

# Finally, let's remove when people named others beyond the defendants (53)
naming_ties <- naming_ties[naming_ties$to %in% defendants_id,] # Eventually, that makes 65 ties in total

# Manual check
# William Baker (P01) never named Thomas Harwode (P34) or Joan Harwode (P35)
naming_ties <- naming_ties[!(naming_ties$deponent == 'P01' & naming_ties$to == 'P34'),] 
naming_ties <- naming_ties[!(naming_ties$deponent == 'P01' & naming_ties$to == 'P35'),] 
# William Riche (P55) never named Christopher Grebill (P31) or John Grebill Jr (P32)
naming_ties <- naming_ties[!(naming_ties$deponent == 'P55' & naming_ties$to == 'P31'),] 
naming_ties <- naming_ties[!(naming_ties$deponent == 'P55' & naming_ties$to == 'P32'),] 

########################################################################################################################

# VISUALISATION OF THE NAMING NETWORK

# Let's make this a graph for visualisation purposes
naming_graph <- graph_from_edgelist(as.matrix.data.frame(naming_ties),directed=TRUE)

# Let's add nodes (the 53 defendants)
'%!in%' <- function(x,y)!('%in%'(x,y)) # Function 'not in'
naming_graph <- igraph::add.vertices(naming_graph, 
                                     length(defendants_id[defendants_id %!in% V(naming_graph)$name]),
                                     name= defendants_id[defendants_id %!in% V(naming_graph)$name])

# Add attributes to the plot
defendants_att <- persons[persons$id %in% defendants_id,]
defendants_att <- defendants_att[match(V(naming_graph)$name,defendants_att$id),]

V(naming_graph)$fullname <- defendants_att$label # full name
V(naming_graph)$sex <- defendants_att$sex # sex
V(naming_graph)$impenitent <- defendants_att$impenitent # whether the person is an impenitent heretic
defendants_att$witness_against_impenitents <- ifelse(!is.na(defendants_att$witness_against_impenitents) & 
                                                       defendants_att$witness_against_impenitents == 1,1,0)
V(naming_graph)$witness <- defendants_att$witness_against_impenitents # Whether a witness
V(naming_graph)$settlement <- defendants_att$origin_or_residence # Place of origin or residence

# VISUALISATION

# Let's safe the layout 
set.seed(0708)
graph_layout <- layout_with_fr(naming_graph)

jpeg(filename='Naming ties.jpeg',width=15,height=15,units='in',res=1000)
plot(naming_graph,
     vertex.color = ifelse(V(naming_graph)$impenitent == 1, "#FF89AA",
                           ifelse(V(naming_graph)$witness == 1,"#9ADDEE",'#F4EED2')),
     vertex.size = 8,
     vertex.label= V(naming_graph)$fullname, vertex.label.cex = 1, vertex.label.color = 'black',
     edge.color = 'tomato', edge.width = 2, edge.arrow.size = 0.75,
     layout = graph_layout,
     main = "Naming ties")
legend("bottomleft",pch=21,
       legend=c('Impenitent heretic','Witness','Defendant'),
       pt.bg=c('#FF89AA','#9ADDEE','#F4EED2'),
       pt.cex=2, cex=1.75, bty="o", ncol=1)
dev.off()

########################################################################################################################

# VISUALISATION: NETWORKS OF NAMES BY WITNESS
# Now let's make a plot where we see the "incriminatios" per witness
witnesses_id <- defendants_att[defendants_att$witness == 1,]$id # witnesses IDs
naming_graphs <- list()
for(i in seq_along(witnesses_id)){
  # ties per witness
  naming_graphs[[i]] <- igraph::delete_edges(naming_graph,which(naming_ties$deponent != witnesses_id[i])) 
}
names(naming_graphs) <- witnesses_id # name each graph after its witness

jpeg(filename='Naming ties per witness.jpeg',width=20,height=12,units='in',res=1000)
par(mfrow=c(3,5)) # A 3 by 5 grid
for(i in seq_along(naming_graphs)){
  plot(naming_graphs[[i]],
       vertex.color = ifelse(V(naming_graphs[[i]])$impenitent == 1, "#FF89AA",
                             ifelse(V(naming_graphs[[i]])$witness == 1,"#9ADDEE",'#F4EED2')),
       vertex.size = 8,
       vertex.label= ifelse(igraph::degree(naming_graphs[[i]],mode = 'in') != 0,V(naming_graph)$fullname,NA), 
       vertex.label.cex = .5, vertex.label.color = 'black',
       edge.color = 'tomato', edge.width = 2, edge.arrow.size = 0.25,
       layout = graph_layout,
       main= defendants_att[defendants_att$id == names(naming_graphs)[i]]$label) # Name of the witness
}
dev.off()
par(mfrow=c(1,1)) 

########################################################################################################################

# Now that we have the ties, what are these ties capturing?
# Let's see how those ties overlap with dimensions like family (kinship ties) or living in the same settlement
naming_mtx <- as.matrix(get.adjacency(naming_graph)) # Let's get the adjacency matrix of the naming graph
naming_mtx[rownames(naming_mtx) %!in% witnesses_id,] <- NA # Those who did not depose have NAs in their rowns

# SAME SETTLEMENT matrix
same_settlement_mtx <- naming_mtx*0
same_settlement_mtx[is.na(same_settlement_mtx)] <- 0

# I will use 4 variables here: residence, residence2, former residence, and origin
residences <- defendants_att[,c('id','label','origin_or_residence','origin','residence','residence2','residence_former')]
# Change "Canterbury: Parish of St. George" for just "Canterbury
residences$residence[!is.na(residences$residence) & residences$residence == "Canterbury: Parish of St. George"] <- 'Canterbury'

# residence2 and residence_former are almost empty, we can put them together
for(i in 1:nrow(residences)){
  if(is.na(residences$residence2[i])){
    residences$residence2[i] <- residences$residence_former[i]
  }
}

# If we are missing a persons' all residences and origin, then missing
residences$missing <- ifelse(is.na(residences$origin) & 
                             is.na(residences$residence) & 
                             is.na(residences$residence2) &
                             is.na(residences$origin_or_residence),1,0)

# Same settlement if either same residence, residence 2 or origin
for(i in rownames(same_settlement_mtx)){
  for(j in colnames(same_settlement_mtx)){
    same_settlement_mtx[i,j] <- ifelse((persons[persons$id == i,]$residence == persons[persons$id == j,]$residence) |
                                       (persons[persons$id == i,]$residence == persons[persons$id == j,]$residence2) |
                                       (persons[persons$id == i,]$residence == persons[persons$id == j,]$origin) |
                                       (persons[persons$id == i,]$residence == persons[persons$id == j,]$origin_or_residence) | 
                                       (persons[persons$id == i,]$residence2 == persons[persons$id == j,]$residence) |
                                       (persons[persons$id == i,]$residence2 == persons[persons$id == j,]$residence2) |
                                       (persons[persons$id == i,]$residence2 == persons[persons$id == j,]$origin) |
                                       (persons[persons$id == i,]$residence2 == persons[persons$id == j,]$origin_or_residence) |
                                       (persons[persons$id == i,]$origin == persons[persons$id == j,]$residence) |
                                       (persons[persons$id == i,]$origin == persons[persons$id == j,]$residence2) |
                                       (persons[persons$id == i,]$origin == persons[persons$id == j,]$origin) |
                                       (persons[persons$id == i,]$origin == persons[persons$id == j,]$origin_or_residence) |
                                       (persons[persons$id == i,]$origin_or_residence == persons[persons$id == j,]$residence) |
                                       (persons[persons$id == i,]$origin_or_residence == persons[persons$id == j,]$residence2) |
                                       (persons[persons$id == i,]$origin_or_residence == persons[persons$id == j,]$origin) |
                                       (persons[persons$id == i,]$origin_or_residence == persons[persons$id == j,]$origin_or_residence)
                                       ,1,0)
  }
}

# Change NAs for zeros
same_settlement_mtx[is.na(same_settlement_mtx)] <- 0

# Add NAs for those whose place is missing
same_settlement_mtx[rownames(same_settlement_mtx) %in% residences[residences$missing == 1,]$id,] <- NA
same_settlement_mtx[,colnames(same_settlement_mtx) %in% residences[residences$missing == 1,]$id] <- NA

# KINSHIP matrix
kinship <- kinship[kinship$from %in% defendants_id & kinship$to %in% defendants_id,] # remove ties with non-defendats
kinship_graph <- graph_from_edgelist(as.matrix.data.frame(kinship[,c('from','to')]),directed = FALSE)
kinship_graph <- simplify(kinship_graph,remove.multiple = TRUE) # remove duplicated ties
# Addition of nodes
kinship_graph <- igraph::add.vertices(kinship_graph, 
                                      length(defendants_id[defendants_id %!in% V(kinship_graph)$name]),
                                      name= defendants_id[defendants_id %!in% V(kinship_graph)$name])
kinship_mtx <- as.matrix(get.adjacency(kinship_graph)) 

# Let's reduce to only ties amond defendants
kinship_mtx <- kinship_mtx[rownames(kinship_mtx) %in% defendants_id,colnames(kinship_mtx) %in% defendants_id]
# Use the same order as in the other two matrices
kinship_mtx <- kinship_mtx[match(rownames(naming_mtx),rownames(kinship_mtx)),match(colnames(naming_mtx),colnames(kinship_mtx))]

diag(kinship_mtx) <- diag(same_settlement_mtx) <- diag(naming_mtx) <- NA # remove all the diagonals

# Summary
sum(naming_mtx,na.rm = TRUE) # 65 naming ties
sum(same_settlement_mtx,na.rm = TRUE)/2 # 151 same-setting ties
sum(kinship_mtx,na.rm = TRUE)/2 # 34 kinship ties

########################################################################################################################

# DESCRIPTIVE STATS OF THE NAMING NETWORK

# Turn into a network object
naming_ntw <- network(naming_mtx) 
# Add nodes attributes
network::set.vertex.attribute(naming_ntw,'labels',defendants_att$label)
network::set.vertex.attribute(naming_ntw,'sex',defendants_att$sex)
network::set.vertex.attribute(naming_ntw,'impenitent',defendants_att$impenitent)
network::set.vertex.attribute(naming_ntw,'witness',defendants_att$witness_against_impenitents)

# Jaccard indices for calculating matrix overlap
Jaccard <- function(matrix1,matrix2){
  shared_ties <- matrix1*matrix2
  diff_ties <- 1*((matrix1+matrix2)==1)
  denominator <- sum(shared_ties,na.rm=TRUE)+sum(diff_ties,na.rm=TRUE)
  outcome <- ifelse(denominator==0,0,sum(shared_ties,na.rm=TRUE)/denominator)
  return(outcome)
}

# Descriptive table
data.frame(stat = c('Nodes','Ties (naming)','Missing tie fraction','Density','Reciprocity','Transitivity',
                    'Average degree','SD out-degree','SD in-degree','Isolates',
                    'Ties to same-sex defendants','EI (sex)',
                    'Ties to kin','Jaccard_index (kinship)',
                    'Ties to defendants in same settlement','Jaccard index (same settlement)'),
           val = c(vcount(naming_graph), # nodes
                   ecount(naming_graph),  #ties
                   network::network.naedgecount(naming_ntw) / (vcount(naming_graph)*(vcount(naming_graph)-1)), # missing tie fraction
                   edge_density(naming_graph), # density
                   reciprocity(naming_graph), # reciprocity
                   transitivity(naming_graph), # transitivity
                   mean(degree(naming_ntw,cmode='outdegree')), # average degree
                   sd(degree(naming_ntw,cmode='outdegree')), # SD out-degree
                   sd(degree(naming_ntw,cmode='indegree')), # SD in-degree
                   sum(igraph::components(naming_graph)$csize == 1), # isolates
                   # sex-based homophily
                   sum(naming_mtx * outer(defendants_att$sex,defendants_att$sex,'==')*1 ,na.rm = TRUE),
                   ei(naming_graph,'sex'),
                   # overlap with kinship
                   sum(naming_mtx*kinship_mtx,na.rm=TRUE),
                   Jaccard(naming_mtx,kinship_mtx),
                   # overlap with being in the same settlement
                   sum(naming_mtx*same_settlement_mtx,na.rm=TRUE),
                   Jaccard(naming_mtx,same_settlement_mtx)
           )) -> sum_table

# Export table of results
write.table(sum_table,'descriptive_stats.csv',row.names=FALSE,sep=',')

########################################################################################################################

# Save image
save.image('Kent_data.RData')