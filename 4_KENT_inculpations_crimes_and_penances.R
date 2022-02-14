## LOLLARDS DATA
## KENT - INCULPATIONS, CRIMES, AND PENANCES (5)
## R script written by Jose Luis Estevez (Masaryk University)
## Date: Feb 11th 2022
########################################################################################################################

# LOADING
rm(list=ls())
load('Kent_data.RData')
crimes_and_penances <- readxl::read_excel('Kent crime and punishment data.xlsx',sheet='Sheet1') # info on individuals

# Required packages
library(igraph);library(MASS);library(ggpubr);library(FactoMineR);library(factoextra)

########################################################################################################################

# DATA SELECTION
# there are some individuals whose crimes are unknown
unknown_crime <- which(rowSums(crimes_and_penances[,c(9:24)],na.rm=TRUE) == 0)
crimes_and_penances <- crimes_and_penances[-unknown_crime,]
# Also, we know that death penalty was the result of non abjuration, so I excluded individuals sentenced to death
crimes_and_penances <- crimes_and_penances[crimes_and_penances$`punishment: execution (= handing over to the secular arm)` != 1,]
# This leaves us with 43 subjects in total

########################################################################################################################

# PENANCES / PUNISHMENTS
names(crimes_and_penances[,c(1,25:38)])

# Simplification of punishment received: prison (most severe), faggot, and minor (least severe)
crimes_and_penances$punishment <- NA

for(i in 1:nrow(crimes_and_penances)){
  if(crimes_and_penances$`punishment: prison for life`[i] == 1){
    crimes_and_penances$punishment[i] <- 'Prison'
  }else if(crimes_and_penances$`punishment: faggot depiction visibly worn on clothes (for life unless commuted)`[i] == 1){
    crimes_and_penances$punishment[i] <- 'Faggot'
  }else{
    crimes_and_penances$punishment[i] <- 'Minor'
  }
}

(crimes_and_penances$punishment <- as.factor(crimes_and_penances$punishment))
summary(crimes_and_penances$punishment)

# CRIMES COMMITED
crimes_and_penances <- crimes_and_penances[,c(1,9:24,40)] 

# NAs to zeros
crimes_and_penances[is.na(crimes_and_penances)] <- 0

# SimpliFIcation of crimes
names(crimes_and_penances)

crimes_and_penances[,5] <-  apply(crimes_and_penances[,5:6],1,max,na.rm=TRUE) # related to confession
crimes_and_penances[,10] <-  apply(crimes_and_penances[,10:11],1,max,na.rm=TRUE) # related to pilgrimages
crimes_and_penances[,12] <-  apply(crimes_and_penances[,12:13],1,max,na.rm=TRUE) # related to images
crimes_and_penances <- crimes_and_penances[,-c(6,11,13)]

# These leaves with 13 crimes or charges
names(crimes_and_penances) <- c('id','Eucharist','Baptism','Confirmation','Confession','Priesthood','Matrimony','Extreme unction',
                                'Pilgrimages','Images','Praying to saints','Blessing meal','Christology','Concealment','punishment')

########################################################################################################################

# BIVARIATE: CRIMES AND PENANCES 
LDA_penance <- lda(punishment ~ Eucharist + Baptism + Confirmation + Confession + Priesthood + Matrimony + 
                     `Extreme unction` + Pilgrimages + Images + `Praying to saints` + `Blessing meal` + Christology + Concealment,
                   data=crimes_and_penances)

penances_mean <- round(LDA_penance$means[c(3,1,2),]*100,1)

# Visualisation
jpeg(filename='Crimes_penances.jpeg',width=10,height=7.5,units='in',res=500)
ggballoonplot(penances_mean,fill='value',show.label = FALSE) +
  gradient_fill(c("red", "gold", "forestgreen")) + 
  labs(fill='Frequency',size='Frequency') 
dev.off()

########################################################################################################################

# ADDITION OF SEX, WITNESSES, AND AGAINST HOW MANY THEY DEPOSED
crimes_and_penances$woman <- ifelse(persons[persons$id %in% crimes_and_penances$id,]$sex == 'f',1,0)
table(crimes_and_penances$punishment,crimes_and_penances$woman) # punishment received by sex

crimes_and_penances$witness <- persons[persons$id %in% crimes_and_penances$id,]$witness # 14 of the 15 witnesses
table(crimes_and_penances$punishment,crimes_and_penances$witness) # punishment received by witness or not

########################################################################################################################

# MULTIPLE CORRESPONDENCE ANALYSIS
crimes <- as.data.frame(crimes_and_penances[,2:14]) 
rownames(crimes) <- persons[persons$id %in% crimes_and_penances$id,]$label
# Define as categorical
for(i in 1:ncol(crimes)){
  crimes[,i] <- factor(crimes[,i],levels=0:1,labels=c('no','yes'))
}

(MCA_crimes <- MCA(crimes,ncp=2,graph=FALSE))
p1 <- fviz_eig(MCA_crimes,addlabels=TRUE) # scree plot
# Correlation between variables and principal dimensions
p2 <- fviz_mca_var(MCA_crimes,choice = "mca.cor",col.var='royalblue',shape.var='circle',repel = TRUE)

var <- get_mca_var(MCA_crimes)
(var$coord) # Coordinates
(var$cos2) # Cos2: quality on the factor map
(var$contrib) # Contributions to the principal components

fviz_mca_var(MCA_crimes, col.var = "cos2",gradient.cols=c("red","gold","forestgreen"),shape.var='circle',repel = TRUE)
fviz_cos2(MCA_crimes, choice = "var", axes = 1:2)

fviz_mca_var(MCA_crimes,col.var="contrib",gradient.cols=c("red","gold","forestgreen"),shape.var='circle',repel = TRUE)
p3 <- fviz_contrib(MCA_crimes,choice="var",axes=1,top=10) # Contributions of rows to dimension 1
p4 <- fviz_contrib(MCA_crimes,choice="var",axes=2,top=10) # Contributions of rows to dimension 2

p5 <- fviz_mca_biplot(MCA_crimes,axes=1:2,repel = TRUE,col.var='royalblue',col.ind='tomato')

jpeg(filename='mca_crimes.jpeg',width=15,height=12,units='in',res=500)
ggarrange(ggarrange(p1,p3,p4,nrow=3,labels=c('A','C','D')),ggarrange(p2,p5,nrow=2,labels=c('B','E')),
          ncol=2,widths=c(.75,1))
dev.off()

crimes_and_penances$PD1 <- MCA_crimes$ind$coord[,1]
crimes_and_penances$PD2 <- MCA_crimes$ind$coord[,2]

########################################################################################################################
