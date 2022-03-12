## LOLLARDS DATA
## COVENTRY - Exploratory analysis (1)
## R script written by Jose Luis Estevez (Masaryk University)
## Date: Jan 18th 2022
########################################################################################################################

# DATA LOADING
rm(list=ls())

library(skimr);library(igraph);library(ggplot2);library(ggraph);library(influenceR);library(ggsignif);library(ggpubr)

data <- readxl::read_excel('Coventry_Edges.xlsx',sheet='Edges') 
persons <- readxl::read_excel('Coventry Persons.xlsx',sheet='Persons')

persons <- persons[order(persons$id),] # order by id
persons <- persons[c(1:158,200:202),] # For some reason, Honza seemed to exclude these individuals

# This is to homogenize ID labels across datasets
for(i in 1:nrow(data)){
  if(nchar(data$Source[i]) == 1){
    data$Source[i] <- paste('P000',data$Source[i],sep='')
  }else if(nchar(data$Source[i]) == 2){
    data$Source[i] <- paste('P00',data$Source[i],sep='')
  }else{
    data$Source[i] <- paste('P0',data$Source[i],sep='')
  }
}

for(i in 1:nrow(data)){
  if(nchar(data$Target[i]) == 1){
    data$Target[i] <- paste('P000',data$Target[i],sep='')
  }else if(nchar(data$Target[i]) == 2){
    data$Target[i] <- paste('P00',data$Target[i],sep='')
  }else{
    data$Target[i] <- paste('P0',data$Target[i],sep='')
  }
}

########################################################################################################################

# Quick look at the network data

# Type of ties in the data
unique(data$Classification_level1) 
# Seems that 'public reading', 'public speaking', 'conversation', 'instruction in heretical beliefs', 'book exchange'
# are forms of dissidence
dissidence <- data[data$Classification_level1 %in% c('public reading','public speaking','conversation',
                                                     'instruction in heretical beliefs','book exchange'),]
# All other ties ties kept apart
otherties <- data[data$Classification_level1 %in% c('accommodation','marriage','kinship','escort','servant',
                                                    'apprentice','loan'),]
# Number of people involved in the 'dissidence network'
length(unique(c(dissidence$Source,dissidence$Target)))

########################################################################################################################

# Quick look at the persons' data

# Reduction of variables
persons <- persons[,c('id','name','family_name','origin_or_residence','sex','age','marital_status','dead',
                      'occupation_general','occupation_type','deponent','literate')]

# Data preparation

# Homogeneisation of missing
persons$name[persons$name == '(unknown)'] <- NA
persons$family_name[persons$family_name == '(unknown)'] <- NA
persons$sex[persons$sex == '?'] <- NA
persons$marital_status[persons$marital_status == 'N/A'] <- NA
persons$origin_or_residence[persons$origin_or_residence == 'Unknown'] <- NA

# There seems to be some specific professions not classified 
persons$occupation_type[!is.na(persons$occupation_general)]
persons$occupation_general[!is.na(persons$occupation_general) & is.na(persons$occupation_type)]
# We can consider 'bedder' a 'servant'; 'draper' a 'merchant', 'notary' we can keep as it is, and 'grazier' a 'farmer' 
# 'pseudo-prophet' and 'summoner' I will just sent them to missing 
persons$occupation_type[!is.na(persons$occupation_general) & is.na(persons$occupation_type)] <- c(NA,'servant',NA,'merchant',
                                                                                                  'notary','notary','farmer')
summary(as.factor(persons$occupation_type))
# To reduce categories, I  merge 'churchperson' (N=36) and 'free profession' (cleric) (1) -> 'churchperson' (37)
# 'notary' (2), 'knight' (1) and 'service provider' (physician) (1) -> 'high rank' (4)
# 'craftsman' (36) and 'manufacturer' (2) -> 'artisans' (38)
# 'manual worker' (5), 'servant' (10), 'production worker' (3), and 'farmer' (1) -> 'manual worker' (19)
# This leaves with only 5 categories: churchperson, high rank, artisan, manual worker, and merchant (9)
persons$job <- ifelse(persons$occupation_type %in% c('churchperson','free profession'),'churchperson',
                      ifelse(persons$occupation_type %in% c('notary','knight noble','service provider'),'high rank',
                             ifelse(persons$occupation_type %in% c('craftsman','manufacturer or qualified worker'),'artisan',
                                    ifelse(persons$occupation_type %in% c('manual worker','servant','farmer','production worker'),'manual worker',
                                           ifelse(persons$occupation_type %in% c('merchant'),'merchant',NA)))))

# Same with residence, I will reduce it to either in Coventry or not
persons$coventry <- 1*(persons$origin_or_residence %in% c('Coventry','Coventry, Leicester','Allesley, Coventry'))
persons$coventry[is.na(persons$origin_or_residence)] <- NA

persons[,c('occupation_general','occupation_type')] <- list(NULL)

# Specification of class of variable
persons$sex <- factor(persons$sex,levels=c('f','m'),labels=c('female','male')) 
persons$age <- as.numeric(persons$age)
persons$marital_status <- as.factor(persons$marital_status)
persons$dead <- factor(ifelse(!is.na(persons$dead),'Yes','No'))
persons$deponent <- factor(persons$deponent,levels=c(0,1),labels=c('No','Yes'))
persons$literate <- factor(persons$literate,levels=c(0,1),labels=c('No','Yes'))
persons$job <- as.factor(persons$job)
persons$coventry <- factor(persons$coventry,levels=c(0,1),labels=c('No','Yes'))

# Summary of the data
skim_tee(persons)
# Loads of missing in marital status (ca. 60%), job (ca. 50%), literate (ca. 75%) and age (ca. 80%)

# This improve a little is we only consider those who deposed
skim_tee(persons[persons$deponent == 'Yes',])

########################################################################################################################

# NETWORK CONSTRUCTION

# Matrix
ntw <- matrix(data=0,nrow=nrow(persons),ncol=nrow(persons),dimnames=list(persons$id,persons$id))

# For the moment, I will just build an undirected network with all dissident ties
dissent <- graph_from_edgelist(as.matrix(unique(dissidence[,c('Source','Target')])),directed=FALSE)
dissent <- 1*(as.matrix(get.adjacency(dissent)) != 0)

# Ampliation to big matrix
for(i in rownames(dissent)){
  for(j in colnames(dissent)){
    ntw[i,j] <- dissent[i,j]
  }
}

dissent <- ntw
rm(ntw)

########################################################################################################################

# Let us now create a matrix indicating kinship and co-location in the same city

# For the kinship matrix, first I will use surnames (if same surname, they must be related)
kinship <- 1*outer(persons$family_name,persons$family_name,'==')
diag(kinship) <- NA
dimnames(kinship) <- list(persons$id,persons$id) # row and column names
sum(kinship,na.rm=TRUE)/2 # 82 kinship ties detected

# To these, I add reported marriages or kinship relations
reportedkinships <- otherties[otherties$Classification_level1 %in% c('marriage','kinship'),c('Source','Target')]
reportedkinships <- as.matrix(get.adjacency(graph_from_edgelist(as.matrix(reportedkinships),directed=FALSE)))
diag(reportedkinships) <- NA

for(i in rownames(reportedkinships)){
  for(j in colnames(reportedkinships)){
    if(!is.na(kinship[i,j]) & kinship[i,j] == 0){
      kinship[i,j] <- reportedkinships[i,j]
    }
  }
}

sum(kinship,na.rm=TRUE)/2 # This added an additional 20 kinship ties: 102 in total

# Co-location
# There are two individuals with two locations
persons$origin_or_residence[104];persons$origin_or_residence[138]
sum(persons$origin_or_residence == 'Allesley',na.rm=TRUE) # since nobody from Allesley, just turn to Coventry
persons$origin_or_residence[104] <- 'Coventry'
sum(persons$origin_or_residence == 'Leicester',na.rm=TRUE) # for the subject with Leicester and Coventry, colocation with both

persons$origin_or_residence2 <- persons$origin_or_residence
persons$origin_or_residence[138] <- 'Coventry'
persons$origin_or_residence2[138] <- 'Leicester'

colocation <- 1*outer(persons$origin_or_residence,persons$origin_or_residence,'==')
colocation2 <- 1*outer(persons$origin_or_residence2,persons$origin_or_residence2,'==')

bothcol <- array(NA,dim=c(dim(colocation),2),dimnames=list(persons$id,persons$id,c('1','2')))
bothcol[,,'1'] <- colocation
bothcol[,,'2'] <- colocation2
bothcol <- apply(bothcol,c(1,2),max)

colocation <- bothcol
rm(colocation2);rm(bothcol);rm(reportedkinships)

########################################################################################################################

# Now we can visually explore these networks 

dissent_graph <- graph_from_adjacency_matrix(dissent,mode='undirected')
kinship_graph <- graph_from_adjacency_matrix(kinship,mode='undirected')
colocation_graph <- graph_from_adjacency_matrix(colocation,mode='undirected')

# Addition of actor attributes
V(dissent_graph)$id <-       V(kinship_graph)$id <-       V(colocation_graph)$id <- persons$id
V(dissent_graph)$name <-     V(kinship_graph)$name <-     V(colocation_graph)$name <- paste(substr(persons$name,1,1),persons$family_name)
V(dissent_graph)$sex <-      V(kinship_graph)$sex <-      V(colocation_graph)$sex <- as.character(persons$sex)
V(dissent_graph)$age <-      V(kinship_graph)$age <-      V(colocation_graph)$age <- as.character(persons$age)
V(dissent_graph)$deponent <- V(kinship_graph)$deponent <- V(colocation_graph)$deponent <- as.character(persons$deponent)
V(dissent_graph)$job <-      V(kinship_graph)$job <-      V(colocation_graph)$job <- as.character(persons$job)
V(dissent_graph)$coventry <- V(kinship_graph)$coventry <- V(colocation_graph)$coventry <- as.character(persons$coventry)

# Creation of a layout
set.seed(0708)
nodes_layout <- layout_with_fr(dissent_graph)

jpeg(filename='Coventry_ntw_jobs.jpeg',width=10,height=7.5,units='in',res=500)
ggraph(dissent_graph,layout=nodes_layout)+
  geom_edge_link(colour='darkgrey',width=.5,alpha=.9)+
  geom_node_point(aes(colour=job,shape=sex),size=4)+
  scale_colour_manual(values=c('artisan'='firebrick2','merchant'='chartreuse3','churchperson'='purple',
                               'high rank'='dodgerblue','manual worker'='orange'))+
  geom_node_text(aes(label=V(dissent_graph)$name),size=2.5,repel=TRUE)
dev.off()

jpeg(filename='Coventry_ntw_deponents.jpeg',width=10,height=7.5,units='in',res=500)
ggraph(dissent_graph,layout=nodes_layout)+
  geom_edge_link(colour='darkgrey',width=.5,alpha=.9)+
  geom_node_point(aes(colour=deponent,fill=deponent,shape=sex),size=4)+
  scale_colour_manual(values=c('Yes'='firebrick2','No'='steelblue'))+
  scale_fill_manual(values=c('Yes'='firebrick2','No'='steelblue'))+
  geom_node_text(aes(label=V(dissent_graph)$name),size=2.5,repel=TRUE)
dev.off()

jpeg(filename='Coventry_ntw_kinship.jpeg',width=10,height=7.5,units='in',res=500)
ggraph(kinship_graph,layout=nodes_layout)+
  geom_edge_link(colour='darkgrey',width=.5,alpha=.9)+
  geom_node_point(aes(colour=persons$origin_or_residence,fill=persons$origin_or_residence,shape=sex),size=4)+
  geom_node_text(aes(label=V(dissent_graph)$name),size=2.5,repel=TRUE)
dev.off()

########################################################################################################################

# Some descriptive stats of the network(s)

# DISSENT NETWORK
vcount(dissent_graph) # number of nodes
ecount(dissent_graph) # number of ties

edge_density(dissent_graph)
#reciprocity(dissent_graph)
transitivity(dissent_graph)

sum(degree(dissent_graph,mode='total') == 0) # number of isolates
sum(components(dissent_graph)$csize > 1) # components (excluding isolates)
components(dissent_graph)$csize[components(dissent_graph)$csize > 1] # one of size 106, the other of size 2

mean(degree(dissent_graph,mode='total')) # avg. degree
max(degree(dissent_graph,mode='total')) # max. degree
hist(degree(dissent_graph)[degree(dissent_graph) != 0],breaks = 15) # degree distribution (without isolates)

# CENTRALITY MEASURES
persons$degree <- degree(dissent_graph) # degree
persons$betweenness <- betweenness(dissent_graph) # betweeness
persons$betweenness <- persons$betweenness/max(persons$betweenness) # to a range 0-1

closeness(dissent_graph) #  closeness does not work well with disconnected networks 
# so we obtained it for only those in the large component
V(dissent_graph)$name <- V(dissent_graph)$id
cls <- closeness(decompose(dissent_graph)[[1]]) 
persons$closeness <- NA
persons$closeness[match(names(cls),persons$id)] <- cls 

persons$ens <- ens(dissent_graph) # Effective network size
persons$constraint <- constraint(dissent_graph) # Network constraint
persons$constraint[is.nan(persons$constraint)] <- NA

# Ego-alter similarity (kinship)
# Number of ties with same kinship
persons$kinship_I <- rowSums(1*(dissent + kinship == 2),na.rm = TRUE)
# Number of ties with non same kinship
persons$kinship_E <- rowSums(1*(dissent - kinship == 1),na.rm = TRUE)
# EI index (-1 homophiliy, 1 heterophily)
persons$kinship_EI <- (persons$kinship_E - persons$kinship_I) / persons$degree
persons$kinship_EI[is.nan(persons$kinship_EI)] <- NA

# Number of non-ties with same kinship
persons$kinship_X <- rowSums(1*(abs(dissent - 1) + kinship == 2),na.rm = TRUE)
# Number of non-ties with non same kinship
persons$kinship_Y <- rowSums(1*(abs(dissent - 1) - kinship == 1),na.rm = TRUE)
# Yule Yules Q index (positive scores indicate homophily for Yule Yules Q)
persons$kinship_Q <- (persons$kinship_I*persons$kinship_Y - persons$kinship_E*persons$kinship_X) / 
  (persons$kinship_I*persons$kinship_Y + persons$kinship_E*persons$kinship_X)
persons$kinship_Q[is.nan(persons$kinship_Q)] <- NA

# Ego-alter similarity (colocation)
# Number of ties with same colocation
persons$colocation_I <- rowSums(1*(dissent + colocation == 2),na.rm = TRUE)
# Number of ties with non same colocation
persons$colocation_E <- rowSums(1*(dissent - colocation == 1),na.rm = TRUE)
# EI index (-1 homophiliy, 1 heterophily)
persons$colocation_EI <- (persons$colocation_E - persons$colocation_I) / persons$degree
persons$colocation_EI[is.nan(persons$colocation_EI)] <- NA

# Number of non-ties with same colocation
persons$colocation_X <- rowSums(1*(abs(dissent - 1) + colocation == 2),na.rm = TRUE)
# Number of non-ties with non same colocation
persons$colocation_Y <- rowSums(1*(abs(dissent - 1) - colocation == 1),na.rm = TRUE)
# Yule's Q index (positive scores indicate homophily)
persons$colocation_Q <- (persons$colocation_I*persons$colocation_Y - persons$colocation_E*persons$colocation_X) / 
  (persons$colocation_I*persons$colocation_Y + persons$colocation_E*persons$colocation_X)
persons$colocation_Q[is.nan(persons$colocation_Q)] <- NA

# Summary
persons[,c('kinship_I','kinship_E','kinship_X','kinship_Y',
           'colocation_I','colocation_E','colocation_X','colocation_Y')] <- list(NULL)

skim_tee(persons)

########################################################################################################################

# Visualisations

no.background <- theme_bw()+
  theme(plot.background=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),panel.border=element_blank())+
  theme(axis.line=element_line(color='black'))+
  theme(strip.text.x=element_text(colour='white',face='bold'))+
  theme(strip.background=element_rect(fill='black'))

# Centrality measures by deponent or not
p1 <- ggplot(data=persons,aes(x=deponent,group=deponent,y=degree,colour=deponent,fill=deponent))+
  geom_point(position = position_jitterdodge(dodge.width = 0, jitter.width = 0.75),size=2)+  
  geom_boxplot(colour='black',alpha=.5)+
  geom_signif(comparisons = list(c("No", "Yes")),map_signif_level=TRUE,textsize=4,colour='black')+
  no.background+labs(fill='',colour='')+xlab('')+ylab('')+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
p2 <- ggplot(data=persons,aes(x=deponent,group=deponent,y=betweenness,colour=deponent,fill=deponent))+
  geom_point(position = position_jitterdodge(dodge.width = 0, jitter.width = 0.75),size=2)+  
  geom_boxplot(colour='black',alpha=.5)+
  geom_signif(comparisons = list(c("No", "Yes")),map_signif_level=TRUE,textsize=4,colour='black')+
  no.background+labs(fill='',colour='')+xlab('')+ylab('')+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
p3 <- ggplot(data=persons,aes(x=deponent,group=deponent,y=closeness,colour=deponent,fill=deponent))+
  geom_point(position = position_jitterdodge(dodge.width = 0, jitter.width = 0.75),size=2)+  
  geom_boxplot(colour='black',alpha=.5)+
  geom_signif(comparisons = list(c("No", "Yes")),map_signif_level=TRUE,textsize=4,colour='black')+
  no.background+labs(fill='',colour='')+xlab('')+ylab('')+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
p4 <- ggplot(data=persons,aes(x=deponent,group=deponent,y=ens,colour=deponent,fill=deponent))+
  geom_point(position = position_jitterdodge(dodge.width = 0, jitter.width = 0.75),size=2)+  
  geom_boxplot(colour='black',alpha=.5)+
  geom_signif(comparisons = list(c("No", "Yes")),map_signif_level=TRUE,textsize=4,colour='black')+
  no.background+labs(fill='',colour='')+xlab('')+ylab('')+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
p5 <- ggplot(data=persons,aes(x=deponent,group=deponent,y=constraint,colour=deponent,fill=deponent))+
  geom_point(position = position_jitterdodge(dodge.width = 0, jitter.width = 0.75),size=2)+  
  geom_boxplot(colour='black',alpha=.5)+
  geom_signif(comparisons = list(c("No", "Yes")),map_signif_level=TRUE,textsize=4,colour='black')+
  no.background+labs(fill='',colour='')+xlab('')+ylab('')+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
p6 <- ggplot(data=persons,aes(x=deponent,group=deponent,y=kinship_EI,colour=deponent,fill=deponent))+
  geom_point(position = position_jitterdodge(dodge.width = 0, jitter.width = 0.75),size=2)+  
  geom_boxplot(colour='black',alpha=.5)+
  geom_signif(comparisons = list(c("No", "Yes")),map_signif_level=TRUE,textsize=4,colour='black')+
  no.background+labs(fill='',colour='')+xlab('')+ylab('')+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
p7 <- ggplot(data=persons,aes(x=deponent,group=deponent,y=colocation_EI,colour=deponent,fill=deponent))+
  geom_point(position = position_jitterdodge(dodge.width = 0, jitter.width = 0.75),size=2)+  
  geom_boxplot(colour='black',alpha=.5)+
  geom_signif(comparisons = list(c("No", "Yes")),map_signif_level=TRUE,textsize=4,colour='black')+
  no.background+labs(fill='',colour='')+xlab('')+ylab('')+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())

jpeg(filename='Coventry_centralities_by_deponent.jpeg',width=10,height=5,units='in',res=500)
ggarrange(p1,p2,p3,p4,p5,p6,p7,
          labels=c('A','B','C','D','E','F','G'),
          common.legend = TRUE,
          nrow=1,ncol=7)
dev.off()

# Centrality measures by sex
p1 <- ggplot(data=persons,aes(x=sex,group=sex,y=degree,colour=sex,fill=sex))+
  geom_point(position = position_jitterdodge(dodge.width = 0, jitter.width = 0.75),size=2)+  
  geom_boxplot(colour='black',alpha=.5)+
  geom_signif(comparisons = list(c("male", "female")),map_signif_level=TRUE,textsize=4,colour='black')+
  no.background+labs(fill='',colour='')+xlab('')+ylab('')+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
p2 <- ggplot(data=persons,aes(x=sex,group=sex,y=betweenness,colour=sex,fill=sex))+
  geom_point(position = position_jitterdodge(dodge.width = 0, jitter.width = 0.75),size=2)+  
  geom_boxplot(colour='black',alpha=.5)+
  geom_signif(comparisons = list(c("male", "female")),map_signif_level=TRUE,textsize=4,colour='black')+
  no.background+labs(fill='',colour='')+xlab('')+ylab('')+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
p3 <- ggplot(data=persons,aes(x=sex,group=sex,y=closeness,colour=sex,fill=sex))+
  geom_point(position = position_jitterdodge(dodge.width = 0, jitter.width = 0.75),size=2)+  
  geom_boxplot(colour='black',alpha=.5)+
  geom_signif(comparisons = list(c("male", "female")),map_signif_level=TRUE,textsize=4,colour='black')+
  no.background+labs(fill='',colour='')+xlab('')+ylab('')+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
p4 <- ggplot(data=persons,aes(x=sex,group=sex,y=ens,colour=sex,fill=sex))+
  geom_point(position = position_jitterdodge(dodge.width = 0, jitter.width = 0.75),size=2)+  
  geom_boxplot(colour='black',alpha=.5)+
  geom_signif(comparisons = list(c("male", "female")),map_signif_level=TRUE,textsize=4,colour='black')+
  no.background+labs(fill='',colour='')+xlab('')+ylab('')+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
p5 <- ggplot(data=persons,aes(x=sex,group=sex,y=constraint,colour=sex,fill=sex))+
  geom_point(position = position_jitterdodge(dodge.width = 0, jitter.width = 0.75),size=2)+  
  geom_boxplot(colour='black',alpha=.5)+
  geom_signif(comparisons = list(c("male", "female")),map_signif_level=TRUE,textsize=4,colour='black')+
  no.background+labs(fill='',colour='')+xlab('')+ylab('')+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
p6 <- ggplot(data=persons,aes(x=sex,group=sex,y=kinship_EI,colour=sex,fill=sex))+
  geom_point(position = position_jitterdodge(dodge.width = 0, jitter.width = 0.75),size=2)+  
  geom_boxplot(colour='black',alpha=.5)+
  geom_signif(comparisons = list(c("male", "female")),map_signif_level=TRUE,textsize=4,colour='black')+
  no.background+labs(fill='',colour='')+xlab('')+ylab('')+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
p7 <- ggplot(data=persons,aes(x=sex,group=sex,y=colocation_EI,colour=sex,fill=sex))+
  geom_point(position = position_jitterdodge(dodge.width = 0, jitter.width = 0.75),size=2)+  
  geom_boxplot(colour='black',alpha=.5)+
  geom_signif(comparisons = list(c("male", "female")),map_signif_level=TRUE,textsize=4,colour='black')+
  no.background+labs(fill='',colour='')+xlab('')+ylab('')+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())

jpeg(filename='Coventry_centralities_by_sex.jpeg',width=10,height=5,units='in',res=500)
ggarrange(p1,p2,p3,p4,p5,p6,p7,
          labels=c('A','B','C','D','E','F','G'),
          common.legend = TRUE,
          nrow=1,ncol=7)
dev.off()

# Centrality measures by job
p1 <- ggplot(data=persons,aes(x=job,group=job,y=degree,colour=job,fill=job))+
  geom_point(position = position_jitterdodge(dodge.width = 0, jitter.width = 0.75),size=2)+  
  geom_boxplot(colour='black',alpha=.5)+
  no.background+labs(fill='',colour='')+xlab('')+ylab('')+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
p2 <- ggplot(data=persons,aes(x=job,group=job,y=betweenness,colour=job,fill=job))+
  geom_point(position = position_jitterdodge(dodge.width = 0, jitter.width = 0.75),size=2)+  
  geom_boxplot(colour='black',alpha=.5)+
  no.background+labs(fill='',colour='')+xlab('')+ylab('')+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
p3 <- ggplot(data=persons,aes(x=job,group=job,y=closeness,colour=job,fill=job))+
  geom_point(position = position_jitterdodge(dodge.width = 0, jitter.width = 0.75),size=2)+  
  geom_boxplot(colour='black',alpha=.5)+
  no.background+labs(fill='',colour='')+xlab('')+ylab('')+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
p4 <- ggplot(data=persons,aes(x=job,group=job,y=ens,colour=job,fill=job))+
  geom_point(position = position_jitterdodge(dodge.width = 0, jitter.width = 0.75),size=2)+  
  geom_boxplot(colour='black',alpha=.5)+
  no.background+labs(fill='',colour='')+xlab('')+ylab('')+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
p5 <- ggplot(data=persons,aes(x=job,group=job,y=constraint,colour=job,fill=job))+
  geom_point(position = position_jitterdodge(dodge.width = 0, jitter.width = 0.75),size=2)+  
  geom_boxplot(colour='black',alpha=.5)+
  no.background+labs(fill='',colour='')+xlab('')+ylab('')+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
p6 <- ggplot(data=persons,aes(x=job,group=job,y=kinship_EI,colour=job,fill=job))+
  geom_point(position = position_jitterdodge(dodge.width = 0, jitter.width = 0.75),size=2)+  
  geom_boxplot(colour='black',alpha=.5)+
  no.background+labs(fill='',colour='')+xlab('')+ylab('')+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
p7 <- ggplot(data=persons,aes(x=job,group=job,y=colocation_EI,colour=job,fill=job))+
  geom_point(position = position_jitterdodge(dodge.width = 0, jitter.width = 0.75),size=2)+  
  geom_boxplot(colour='black',alpha=.5)+
  no.background+labs(fill='',colour='')+xlab('')+ylab('')+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())

jpeg(filename='Coventry_centralities_by_job.jpeg',width=10,height=5,units='in',res=500)
ggarrange(p1,p2,p3,p4,p5,p6,p7,
          labels=c('A','B','C','D','E','F','G'),
          common.legend = TRUE,
          nrow=1,ncol=7)
dev.off()

########################################################################################################################

# Overlap between dissent and kinship
Jaccard <- function(matrix1,matrix2){
  shared_ties <- matrix1*matrix2
  diff_ties <- 1*((matrix1+matrix2)==1)
  denominator <- sum(shared_ties,na.rm=TRUE)+sum(diff_ties,na.rm=TRUE)
  outcome <- ifelse(denominator==0,0,sum(shared_ties,na.rm=TRUE)/denominator)
  return(outcome)
}

Jaccard(dissent,kinship)*100 # Overlap between illicit speech and kinship

########################################################################################################################

rm(p1);rm(p2);rm(p3);rm(p4);rm(p5);rm(p6);rm(p7);rm(cls);rm(i);rm(j)

save.image('Coventry_data.RData')