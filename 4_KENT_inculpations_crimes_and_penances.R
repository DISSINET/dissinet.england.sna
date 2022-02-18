## LOLLARDS DATA
## KENT - INCULPATIONS, CRIMES, AND PENANCES (4)
## R script written by Jose Luis Estevez (Masaryk University)
## Date: Feb 11th 2022
########################################################################################################################

# LOADING
rm(list=ls())
load('Kent_data.RData')
crimes_and_penances <- readxl::read_excel('Kent crime and punishment data.xlsx',sheet='Sheet1') # info on individuals

# Required packages
library(igraph);library(MASS);library(ggpubr);library(FactoMineR);library(factoextra);library(VGAM)

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
jpeg(filename='Crimes_penances.jpeg',width=8,height=5.5,units='in',res=500)
ggballoonplot(penances_mean,fill='value',show.label = FALSE) +
  gradient_fill(c("red", "gold", "forestgreen")) + 
  labs(fill='Frequency',size='Frequency') 
dev.off()

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

# ADDITION OF SEX, WITNESSES, AGAINST HOW MANY THEY DEPOSED, AND HOW MANY DEPOSED AGAINST THEM
crimes_and_penances$woman <- ifelse(persons[persons$id %in% crimes_and_penances$id,]$sex == 'f',1,0)
table(crimes_and_penances$punishment,crimes_and_penances$woman) # punishment received by sex

crimes_and_penances$witness <- persons[persons$id %in% crimes_and_penances$id,]$witness # 14 of the 15 witnesses
table(crimes_and_penances$punishment,crimes_and_penances$witness) # punishment received by witness or not

persons$inculpations_send <- rowSums(inculpations)
crimes_and_penances$inculpations_send <- persons[persons$id %in% crimes_and_penances$id,]$inculpations_send
table(crimes_and_penances$punishment,crimes_and_penances$inculpations_send) 
# logarithm version
crimes_and_penances$`inculpations sent (log)` <- log(crimes_and_penances$inculpations_send + 1)

persons$inculpations_rec <- colSums(inculpations)
crimes_and_penances$inculpations_rec <- persons[persons$id %in% crimes_and_penances$id,]$inculpations_rec
table(crimes_and_penances$punishment,crimes_and_penances$inculpations_rec) 
# logarithm version
crimes_and_penances$`inculpations received (log)` <- log(crimes_and_penances$inculpations_rec + 1)

########################################################################################################################

# BIVARIATE DESCRIPTION
# change reference category
crimes_and_penances$punishment <- factor(crimes_and_penances$punishment,levels=c('Minor','Faggot','Prison'))

# BIVARIATE: CRIMES by PD1, WOMAN, WITNESS, ETC.
LDA_penance <- lda(punishment ~  PD1 + woman + witness + `inculpations sent (log)` + `inculpations received (log)`,
                   data=crimes_and_penances)
(penances_mean <- round(LDA_penance$means*100,1))

# Visualisation
panel.hist <- function(x, ...) {
  usr = par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  hist(x, freq = FALSE, add=TRUE, col = adjustcolor(4, .4),breaks=12) 
  lines(density(x),col='red')
}
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- (cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  text(0.5, 0.5, txt, cex = 1.25, font = 4)
}

jpeg(filename='corr_plot.jpeg',width=18,height=10,units='in',res=500)
pairs(crimes_and_penances[,c('punishment','PD1','PD2','witness','inculpations sent (log)','inculpations received (log)')],
      diag.panel=panel.hist,
      upper.panel=panel.cor,
      lower.panel=panel.smooth,
      cex = 1.5, pch = 19, col = adjustcolor(4, .4), cex.labels = 1.75,font.labels = 2)
dev.off()

########################################################################################################################

# ALL VARIABLES ARE STANDARDISED
crimes_and_penances$PD1 <- scale(crimes_and_penances$PD1,center=TRUE,scale=TRUE)
crimes_and_penances$woman <- scale(crimes_and_penances$woman,center=TRUE,scale=TRUE)
crimes_and_penances$witness <- scale(crimes_and_penances$witness,center=TRUE,scale=TRUE)
crimes_and_penances$`inculpations sent (log)` <- scale(crimes_and_penances$`inculpations sent (log)`,center=TRUE,scale=TRUE)
crimes_and_penances$`inculpations received (log)` <- scale(crimes_and_penances$`inculpations received (log)`,center=TRUE,scale=TRUE)

#  LOGISTIC REGRESSION FOR NOMINAL RESPONSES (MULTINOMIAL)
crimes_and_penances$punishment <- factor(crimes_and_penances$punishment,levels=c('Faggot','Prison','Minor'))

# Punishment as a function of charges
model1 <- vglm(punishment ~ PD1 + PD2,
          data=crimes_and_penances,family=multinomial)
summary(model1)

# Punishment as a function of charges + individual attributes (being a witness and sex)
model2 <- vglm(punishment ~ PD1 + PD2 + woman + witness,
               data=crimes_and_penances,family=multinomial)
summary(model2)

# Punishment as a function of chargers + individual attributes + inculpations
model3 <- vglm(punishment ~ PD1 + PD2 + woman + witness + `inculpations received (log)`,
               data=crimes_and_penances,family=multinomial)
summary(model3)

model4 <- vglm(punishment ~ PD1 + PD2 + woman + witness + `inculpations received (log)` + `inculpations sent (log)`,
               data=crimes_and_penances,family=multinomial)
summary(model4)

########################################################################################################################

# VISUALSIATION OF RESULTS
model3_vis <- data.frame(
  parameter = c('Intercept (faggot)','Intercept (prison)','Charges PD1 (faggot)','Charges PD1 (prison)','Charges PD2 (faggot)',
                'Charges PD2 (prison)','Woman (faggot)','Woman (prison)','Witness (faggot)','Witness (prison)',
                'Inculpations received (log) (faggot)','Inculpations received (log) (prison)'),
  estimate = as.numeric(coefficients(model3)),
  lower = as.numeric(confint(model3,level=.90)[,1]),
  upper = as.numeric(confint(model3,level=.90)[,2])
)

# Order the y axis
model3_vis$parameter <- factor(model3_vis$parameter,
                               level = c('Inculpations received (log) (prison)','Inculpations received (log) (faggot)',
                                         'Witness (prison)','Witness (faggot)','Woman (prison)','Woman (faggot)',
                                         'Charges PD2 (prison)','Charges PD2 (faggot)','Charges PD1 (prison)','Charges PD1 (faggot)',
                                         'Intercept (prison)','Intercept (faggot)'))

jpeg(filename='model3.jpeg',width=9,height=7,units='in',res=500)
ggplot(data=model3_vis, aes(x=parameter, y=estimate, ymin=lower, ymax=upper)) +
  geom_hline(yintercept= 0, lty=2,colour='red') +
  geom_pointrange(position=position_dodge(width = 0.5)) +
  coord_flip() +
  xlab("") + ylab("Estimate (90% CI)") +
  theme_bw() +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=12))
dev.off()
