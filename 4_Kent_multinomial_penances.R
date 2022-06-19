## LOLLARDS DATA
## KENT - INCULPATIONS, CRIMES, AND PENANCES (4)
## R script written by Jose Luis Estevez (Masaryk University)
## Date: June 17th 2022
########################################################################################################################

# Required packages
library(igraph);library(MASS);library(ggpubr);library(FactoMineR);library(factoextra);library(psych)

# LOADING
rm(list=ls())
load('Kent_data.RData')

########################################################################################################################

# DATA SELECTION

# Let's remove the 5 impenitent heretics
impenitent_her <- persons[!is.na(persons$impenitent) & persons$impenitent == 1,]$id
crimes_and_penances <- crimes_and_penances[crimes_and_penances$person_id %!in% impenitent_her,]
# There are three people whose penances are missing
crimes_and_penances$penance_missing <- (crimes_and_penances$person_label %in% c('Simon Piers','Agnes Chetynden','Robert Hilles'))*1

# Charges and punishments as numeric
crimes_and_penances[,9:39] <- lapply(crimes_and_penances[,9:39],as.numeric)

########################################################################################################################

# PENANCES / PUNISHMENTS
names(crimes_and_penances[,c(1,26:39)]) # The punishments

# Simplification of punishment received: prison (most severe), faggot, and minor (least severe)
crimes_and_penances$punishment <- NA

for(i in 1:nrow(crimes_and_penances)){
  if(crimes_and_penances$penance_missing[i] == 1){
    crimes_and_penances$punishment[i] <- NA
  }else if(crimes_and_penances$`punishment: prison for life`[i] == 1){
    crimes_and_penances$punishment[i] <- 'Prison'
  }else if(crimes_and_penances$`punishment: faggot depiction visibly worn on clothes (for life unless commuted)`[i] == 1 
           | crimes_and_penances$`punishment: faggot in a main church`[i] == 1 
           | crimes_and_penances$`punishment: faggot in own parish church or cemetery`[i] == 1
           ){
    crimes_and_penances$punishment[i] <- 'Faggot'
  }else{
    crimes_and_penances$punishment[i] <- 'Minor' # minor or no penance at all
  }
}

(crimes_and_penances$punishment <- as.factor(crimes_and_penances$punishment))
summary(crimes_and_penances$punishment)

# VISUALISATION
# Add penances to the naming grahp object
V(naming_graph)$penance <- as.character(crimes_and_penances[match(V(naming_graph)$name,crimes_and_penances$person_id),]$punishment)

jpeg(filename='Naming ties with penances.jpeg',width=15,height=15,units='in',res=1000)
plot(naming_graph,
     vertex.color = ifelse(V(naming_graph)$impenitent == 1, "indianred1",
                           ifelse(V(naming_graph)$penance == 'Prison',"sandybrown",
                                  ifelse(V(naming_graph)$penance == 'Faggot','goldenrod1','darkolivegreen1'))),
     vertex.frame.color=grey(0.25,.75),
     vertex.size = 8,
     vertex.label= V(naming_graph)$fullname, vertex.label.cex = .5, vertex.label.color = 'black',
     vertex.label.font=2,
     vertex.label.family= "Times",
     edge.color = 'tomato', edge.width = 2, edge.arrow.size = 0.75,
     layout = graph_layout,
     main = "Naming ties")
legend("bottomleft",pch=21,
       legend=c('Execution','Prison','Faggot','Minor or no penance','Penance missing'),
       pt.bg=c('indianred1','sandybrown','goldenrod1','darkolivegreen1','white'),
       pt.cex=2, cex=1.75, bty="o", ncol=1)
dev.off()

########################################################################################################################

# CRIMES COMMITED
crimes_and_penances <- crimes_and_penances[,c(1:2,9:25,ncol(crimes_and_penances))] # Keep only crimes and punishment received

# NAs to zeros
crimes_and_penances[crimes_and_penances$person_label == 'Thomas Mannyng',]$`concealment of heresy` <- 0

# Simplification of crimes
names(crimes_and_penances)

crimes_and_penances[,3] <-  apply(crimes_and_penances[,3:4],1,max,na.rm=TRUE) # related to Eucharist
crimes_and_penances[,7] <-  apply(crimes_and_penances[,7:8],1,max,na.rm=TRUE) # related to confession
crimes_and_penances[,12] <-  apply(crimes_and_penances[,12:13],1,max,na.rm=TRUE) # related to pilgrimages
crimes_and_penances[,14] <-  apply(crimes_and_penances[,14:15],1,max,na.rm=TRUE) # related to images
crimes_and_penances <- crimes_and_penances[,-c(4,8,13,15)]

# Remmeber that 'John Bampton' (P03), 'Robert Franke' (P28) and 'William Pelland' (P50) have their crimes missing
crimes_and_penances[crimes_and_penances$person_id %in% c('P03','P28','P50'),c(3:15)] <- NA

# These leaves with 13 crimes or charges
names(crimes_and_penances) <- c('id','label','Eucharist','Baptism','Confirmation','Confession','Priesthood','Matrimony','Extreme unction',
                                'Pilgrimages','Images','Praying to saints','Blessing meal','Christology','Concealment','punishment')

# Let's remove Christology
crimes_and_penances <- crimes_and_penances[,names(crimes_and_penances) %!in% c('Christology')]

########################################################################################################################

# BIVARIATE: CRIMES AND PENANCES 
LDA_penance <- lda(punishment ~ Eucharist + Baptism + Confirmation + Confession + Priesthood + Matrimony + 
                     `Extreme unction` + Pilgrimages + Images + `Praying to saints` + `Blessing meal` + Concealment,
                   data=crimes_and_penances)

penances_mean <- round(LDA_penance$means[c(3,1,2),]*100,1)

# Visualisation
jpeg(filename='Charges and penances.jpeg',width=8,height=5.5,units='in',res=1000)
ggballoonplot(penances_mean,show.label = TRUE,fill='value',size=12) +
  gradient_fill(c("firebrick2","coral","wheat","cadetblue","cornflowerblue")) +
  labs(fill='Frequency (%)') 
dev.off()

########################################################################################################################

# MULTIPLE CORRESPONDENCE ANALYSIS
crimes <- as.data.frame(crimes_and_penances[,3:14]) 
rownames(crimes) <- crimes_and_penances$label

# remove those whose crimes are missing
crimes <- crimes[rownames(crimes) %!in% c('John Bampton','Robert Franke','William Pelland'),]

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

jpeg(filename='Multiple correspondence analysis outcome.jpeg',width=16,height=12,units='in',res=1000)
ggarrange(ggarrange(p1,p3,p4,nrow=3,labels=c('A','B','C')),
          ggarrange(p5,nrow=1,labels=c('D')),
          ncol=2,widths=c(.5,1))
dev.off()

# Merge data
pd1 <- as.data.frame(MCA_crimes$ind$coord[,1])
names(pd1) <- 'PD1'
pd2 <- as.data.frame(MCA_crimes$ind$coord[,2])
names(pd2) <- 'PD2'
crimes_and_penances <- merge(crimes_and_penances,pd1,by.x='label',by.y=0,all.x=TRUE)
crimes_and_penances <- merge(crimes_and_penances,pd2,by.x='label',by.y=0,all.x=TRUE)

########################################################################################################################

# ADDITION OF SEX, WITNESSES, AGAINST HOW MANY THEY DEPOSED, AND HOW MANY DEPOSED AGAINST THEM

# Extraction of number of inculpations sent and received
defendants_att$inculpations_send <- rowSums(naming_mtx,na.rm = TRUE)
defendants_att$inculpations_rec <- colSums(naming_mtx,na.rm = TRUE)
defendants_att$named <- (defendants_att$inculpations_rec > 0)*1 # Whether named (yes or no)

# Merge crimes and punishment with the rest of the data
crimes_and_penances <- merge(crimes_and_penances,defendants_att,all.x = TRUE)

# Some bivariate tables
table(crimes_and_penances$punishment,crimes_and_penances$inculpations_rec) # punishment received by incriminations received
table(crimes_and_penances$punishment,crimes_and_penances$inculpations_send) # punishment received by incriminations sent
table(crimes_and_penances$punishment,crimes_and_penances$witness_against_impenitents) # punishment received by witness or not
table(crimes_and_penances$punishment,crimes_and_penances$sex) # punishment received by sex

########################################################################################################################

# BIVARIATE DESCRIPTION
# change reference category
crimes_and_penances$punishment <- factor(crimes_and_penances$punishment,levels=c('Minor','Faggot','Prison'))
crimes_and_penances$woman <- ifelse(crimes_and_penances$sex == 'f',1,0)

# BIVARIATE: CRIMES by PD1, WOMAN, WITNESS, ETC.
LDA_penance <- lda(punishment ~  PD1 + PD2 + sex + witness_againts_impenitents + inculpations_send + inculpations_rec,
                   data=crimes_and_penances)
(penances_mean <- round(LDA_penance$means*100,1))

# Visualisation
jpeg(filename='Correlations among variables.jpeg',width=12,height=12,units='in',res=1000)
forplot <- crimes_and_penances[,c('punishment','inculpations_rec','inculpations_send',
                                  'woman','PD1','PD2')]

forplot$severe <- ifelse(crimes_and_penances$punishment %in% c('Prison','Faggot'),1,0)
forplot$severe[which(is.na(forplot$punishment))] <- NA # remove those missing

forplot$prisontofaggot <- ifelse(crimes_and_penances$punishment %in% c('Prison'),1,0)
forplot$prisontofaggot[which(is.na(forplot$punishment) | forplot$punishment == 'Minor')] <- NA # remove missing and minor penances

names(forplot) <- c('Penance received\n(minor-faggot-prison)',
                    'Number of other defendants\nwho gave your name\n(naming in-degree)',
                    'Number of names given\nto the inquisitor\n(naming out-degree)',
                    'Sex\n(man vs. woman)','Charges\n(Dimension 1)','Charges\n(Dimension 2)',
                    'Penance received\n(minor/none vs. prison/faggot)','Penance received\n(faggot vs. prison)')

pairs.panels(forplot[,-1], # don't show the variable with three values: minor, faggot, prison
             method = "spearman",stars = TRUE,
             lm=TRUE,ci=TRUE,ellipses=FALSE,
             pch = 21,jiggle=TRUE,factor=.15,hist.col = 'skyblue',scale=FALSE)
dev.off()

########################################################################################################################

#  HYPOTHESIS TESTING
# Standardization
crimes_and_penances$PD1 <- scale(crimes_and_penances$PD1,center=TRUE,scale=TRUE)
crimes_and_penances$PD2 <- scale(crimes_and_penances$PD2,center=TRUE,scale=TRUE)
crimes_and_penances$woman <- scale(crimes_and_penances$woman,center=TRUE,scale=TRUE)
crimes_and_penances$witness <- scale(crimes_and_penances$witness_against_impenitents,center=TRUE,scale=TRUE)
crimes_and_penances$named <- scale(crimes_and_penances$named,center=TRUE,scale=TRUE)
crimes_and_penances$inculpations_send <- scale(crimes_and_penances$inculpations_send,center=TRUE,scale=TRUE)
crimes_and_penances$inculpations_rec <- scale(crimes_and_penances$inculpations_rec,center=TRUE,scale=TRUE)

#  LOGISTIC REGRESSION: PRISON OR FAGGOT VS. MINOR
crimes_and_penances$prisonfaggot <- ifelse(crimes_and_penances$punishment %in% c('Prison','Faggot'),1,0)
crimes_and_penances$prisonfaggot[which(is.na(crimes_and_penances$punishment))] <- NA # remove those missing

# Model 1: Penances
model1 <- glm(prisonfaggot ~ PD1 + PD2,
              data=crimes_and_penances,family=binomial)
summary(model1)

# Model 2: Penances and gender
model2 <- glm(prisonfaggot ~ PD1 + PD2 + woman,
              data=crimes_and_penances,family=binomial)
summary(model2)

########################################################################################################################

#  LOGISTIC REGRESSION: PRISON VS FAGGOT OR MINOR

crimes_and_penances$prison <- ifelse(crimes_and_penances$punishment == 'Prison',1,0)
crimes_and_penances$prison[which(is.na(crimes_and_penances$punishment))] <- NA # remove those missing

# Model 1: Penances and gender
model1 <- glm(prison ~ PD1 + PD2 + woman,
              data=crimes_and_penances,family=binomial)
summary(model1)

# Model 2: Penances, gender, witness and named
model2 <- glm(prison ~ PD1 + PD2 + woman + witness + named,
              data=crimes_and_penances,family=binomial)
summary(model2)

# Model 3: Penances, gender, naming out-degree and naming in-degree
model3 <- glm(prison ~ PD1 + PD2 + woman + inculpations_send + inculpations_rec,
              data=crimes_and_penances,family=binomial)
summary(model3)

########################################################################################################################

# LOGISTIC REGRESSION: ONLY PRISON VS. FAGGOT

# Model 1
model1 <- glm(prison ~ PD1 + PD2 + woman,
              data=crimes_and_penances[crimes_and_penances$punishment %in% c('Prison','Faggot'),],family=binomial)
summary(model1)

# Model 2: Penances, gender, witness and named
model2 <- glm(prison ~ PD1 + PD2 + woman + witness + named,
              data=crimes_and_penances[crimes_and_penances$punishment %in% c('Prison','Faggot'),],family=binomial)
summary(model2)

# Model 3: Penances, gender, naming out-degree and naming in-degree
model3 <- glm(prison ~ PD1 + PD2 + woman + inculpations_send + inculpations_rec,
              data=crimes_and_penances[crimes_and_penances$punishment %in% c('Prison','Faggot'),],family=binomial)
summary(model3)