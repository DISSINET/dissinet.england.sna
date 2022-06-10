## LOLLARDS DATA
## KENT - INCULPATIONS, CRIMES, AND PENANCES (4)
## R script written by Jose Luis Estevez (Masaryk University)
## Date: June 9th 2022
########################################################################################################################

# Required packages
library(igraph);library(MASS);library(ggpubr);library(FactoMineR);library(factoextra);library(psych);library(VGAM)

# LOADING
rm(list=ls())
load('Kent_data.RData')

########################################################################################################################

# DATA SELECTION
# there are some individuals whose crimes are unknown
unknown_crime <- which(rowSums(crimes_and_penances[,c(9:24)],na.rm=TRUE) == 0)
crimes_and_penances <- crimes_and_penances[-unknown_crime,]
# Also, we know that death penalty was the result of non abjuration, so I excluded individuals sentenced to death
crimes_and_penances <- crimes_and_penances[crimes_and_penances$`punishment: execution (= handing over to the secular arm)` != 1,]
# This leaves us with 43 subjects in total

# Let's remove those who did not abjure heresy, or followed a different procedure
crimes_and_penances <- crimes_and_penances[crimes_and_penances$person_label %!in% c('Simon Piers','Joan Bukherst','John Dodde'),]

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

# Let's remove concealment which is constant, and Christology which is collinear
crimes_and_penances <- crimes_and_penances[,names(crimes_and_penances) %!in% c('Concealment','Christology')]

########################################################################################################################

# BIVARIATE: CRIMES AND PENANCES 
LDA_penance <- lda(punishment ~ Eucharist + Baptism + Confirmation + Confession + Priesthood + Matrimony + 
                     `Extreme unction` + Pilgrimages + Images + `Praying to saints` + `Blessing meal`,
                   data=crimes_and_penances)

penances_mean <- round(LDA_penance$means[c(3,1,2),]*100,1)

# Visualisation
jpeg(filename='Charges and penances.jpeg',width=8,height=5.5,units='in',res=1000)
ggballoonplot(penances_mean,show.label = TRUE,fill='value',size=12) +
  gradient_fill(c("royalblue", "white", "red")) +
  labs(fill='Frequency (%)') 
dev.off()

########################################################################################################################

# MULTIPLE CORRESPONDENCE ANALYSIS
crimes <- as.data.frame(crimes_and_penances[,2:12]) 
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

jpeg(filename='Multiple correspondence analysis outcome.jpeg',width=15,height=12,units='in',res=1000)
ggarrange(ggarrange(p1,p3,p4,nrow=3,labels=c('A','C','D')),ggarrange(p2,p5,nrow=2,labels=c('B','E')),
          ncol=2,widths=c(.75,1))
dev.off()

crimes_and_penances$PD1 <- MCA_crimes$ind$coord[,1]
crimes_and_penances$PD2 <- MCA_crimes$ind$coord[,2]

########################################################################################################################

# ADDITION OF SEX, WITNESSES, AGAINST HOW MANY THEY DEPOSED, AND HOW MANY DEPOSED AGAINST THEM

# Extraction of number of inculpations sent and received
defendants_att$inculpations_send <- rowSums(naming_mtx,na.rm = TRUE)
defendants_att$inculpations_rec <- colSums(naming_mtx,na.rm = TRUE)

# Merge crimes and punishment with the rest of the data
crimes_and_penances <- merge(crimes_and_penances,defendants_att,all.x = TRUE)

# Some bivariate tables
table(crimes_and_penances$punishment,crimes_and_penances$inculpations_rec) # punishment received by incriminations received
table(crimes_and_penances$punishment,crimes_and_penances$inculpations_send) # punishment received by incriminations sent
table(crimes_and_penances$punishment,crimes_and_penances$witness) # punishment received by witness or not
table(crimes_and_penances$punishment,crimes_and_penances$sex) # punishment received by sex

########################################################################################################################

# BIVARIATE DESCRIPTION
# change reference category
crimes_and_penances$punishment <- factor(crimes_and_penances$punishment,levels=c('Minor','Faggot','Prison'))
crimes_and_penances$woman <- ifelse(crimes_and_penances$sex == 'f',1,0)

# BIVARIATE: CRIMES by PD1, WOMAN, WITNESS, ETC.
LDA_penance <- lda(punishment ~  PD1 + PD2 + sex + witness + inculpations_send + inculpations_rec,
                   data=crimes_and_penances)
(penances_mean <- round(LDA_penance$means*100,1))

# Visualisation
jpeg(filename='Correlations among variables.jpeg',width=12,height=12,units='in',res=1000)
forplot <- crimes_and_penances[,c('punishment','inculpations_rec','inculpations_send','witness','woman','PD1','PD2')]
names(forplot) <- c('Penance\nreceived','Times\nnamed','Names\ngiven','Witness','Woman','Charges\n(PD1)','Charges\n(PD2)')

pairs.panels(forplot,
             method = "pearson",stars = TRUE,
             lm=TRUE,ci=TRUE,ellipses=FALSE,
             pch = 21,jiggle=TRUE,factor=.15,hist.col = 'skyblue',scale=FALSE)
dev.off()

########################################################################################################################

#  LOGISTIC REGRESSION FOR NOMINAL RESPONSES (MULTINOMIAL)
crimes_and_penances$punishment <- factor(crimes_and_penances$punishment,levels=c('Faggot','Prison','Minor'))

# Standardization
crimes_and_penances$PD1 <- scale(crimes_and_penances$PD1,center=TRUE,scale=TRUE)
crimes_and_penances$PD2 <- scale(crimes_and_penances$PD2,center=TRUE,scale=TRUE)
crimes_and_penances$woman <- scale(crimes_and_penances$woman,center=TRUE,scale=TRUE)
crimes_and_penances$witness <- scale(crimes_and_penances$witness,center=TRUE,scale=TRUE)
crimes_and_penances$inculpations_send <- scale(crimes_and_penances$inculpations_send,center=TRUE,scale=TRUE)
crimes_and_penances$inculpations_rec <- scale(crimes_and_penances$inculpations_rec,center=TRUE,scale=TRUE)

# Model 1: only incriminations received
model1 <- vglm(punishment ~ inculpations_rec,
          data=crimes_and_penances,family=multinomial)
summary(model1)

# Model 2: incriminations received and sent
model2 <- vglm(punishment ~ inculpations_rec + inculpations_send,
               data=crimes_and_penances,family=multinomial)
summary(model2)

# Model 3: incriminations received and witness
model3 <- vglm(punishment ~ inculpations_rec + witness,
               data=crimes_and_penances,family=multinomial)
summary(model3)

# Model 4: incriminations received and gender
model4 <- vglm(punishment ~ inculpations_rec + woman,
               data=crimes_and_penances,family=multinomial)
summary(model4)

# Model 5: incriminations received and charges
model5 <- vglm(punishment ~ inculpations_rec + PD1,
               data=crimes_and_penances,family=multinomial)
summary(model5)
