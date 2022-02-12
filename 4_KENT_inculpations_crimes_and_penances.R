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
library(igraph);library(MASS);library(ggpubr);library(factoextra)

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

# PRINCIPAL COMPONENT ANALYSIS TO REDUCE CRIMES
crimes <- as.data.frame(crimes_and_penances[,2:14])
rownames(crimes) <- persons[persons$id %in% crimes_and_penances$id,]$label

(PCA_crimes <- prcomp(crimes,center=TRUE,scale=TRUE)) # Three components 
PCA_crimes$rotation[,1:3] # see loading of the first three components

# Visualisation
p1 <- fviz_eig(PCA_crimes)
p2 <- fviz_pca_var(PCA_crimes, col.var = "contrib", gradient.cols = c("red", "gold", "forestgreen"), repel = TRUE)
p3 <- fviz_pca_biplot(PCA_crimes, repel = TRUE, col.var = "royalblue",col.ind = "darkgrey")

jpeg(filename='pca_crimes.jpeg',width=15,height=7.5,units='in',res=500)
ggarrange(p1,p2,p3,
          common.legend = TRUE,
          nrow=1,ncol=3)
dev.off()

# Extraction of the fist three components
crimes_and_penances$PC1 <- PCA_crimes$x[,1]
crimes_and_penances$PC2 <- PCA_crimes$x[,2]
crimes_and_penances$PC3 <- PCA_crimes$x[,3]

########################################################################################################################
