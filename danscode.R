#Ciarasdata
library(tidyverse)
library(gridExtra)
library(ggplot2)
library(wesanderson)
library(ggpubr)
library(VGAM)
library(ggfortify)
library(corrplot)
library(broom)
install.packages("openxlsx")
library(forestplot)
library(openxlsx)
#Load dataset
ciaradata<-read_excel("/Users/AGaffney/Documents/ciara-project/data/Copy of Complete CHorio with NDI ct.xlsx")
summary(as.factor(ciaradata$`Bayley scores 1=Y, 0=n`))
summary(as.factor(ciaradata$`CP 1=yes, 0=no`))
summary(as.factor(ciaradata$`ASD 1=yes`))
summary(as.numeric(ciaradata$`Length of follow up (months)`))
summary(as.factor(ciaradata$`If no, reason`))
summary(as.factor(ciaradata$`Referral to CDNT 1=yes, 0=no`))
#Questions regarding composite scores <85 NDI in cognitive, motor, language and social emotional
#?Median NDI
#SES and FIRS see what we get
#Why they were lost to follow up?
#Dan and ciara practice
ciaradata$SESgroups<-cut(ciaradata$`HP Dep Score`,
                         breaks=c(-40,-10,10,40),
                         labels = c("Disadvantaged",  "Average", "Affluent"))
summary(ciaradata$SESgroups)
str(ciaradata$`Length of follow up (months)`)
ciaradata$`Length of follow up (months)`<-as.numeric(ciaradata$`Length of follow up (months)`)
summary(ciaradata$)
SESFUbp<-ggplot(data=subset(ciaradata,!is.na(SESgroups)), aes(x=SESgroups, y=`Length of follow up (months)`, fill= SESgroups))+
  geom_boxplot()
SESFUbp2<-SESFUbp+stat_compare_means(method="anova")
SESFUbp3<-SESFUbp2+scale_fill_manual(values=wes_palette(name="Zissou1", n=3))
SESFUbp3+theme_pubclean()
str(ciaradata$Motor)
ciaradata$Motor<-as.numeric(ciaradata$Motor)
ciaradata$Language<-as.numeric(ciaradata$Language)
ciaradata$Cognitive<-as.numeric(ciaradata$Cognitive)
#composite outcome
ciaradata<-ciaradata%>%mutate(compNDIdeath=
                                case_when(`Death? 1=yes, 0=No`==1~"YES",
                                          `NDI (CP or GDD without Bayley score) 1=yes, 0=no`==1~"YES",
                                          Motor<85~"YES",
                                          Language<85~"YES",
                                          Cognitive<85~"YES",
                                          TRUE~"NO"))
ciaradata<-ciaradata%>%mutate(compsevNDIdeath=
                                case_when(`Death? 1=yes, 0=No`==1~"YES",
                                          `CP 1=yes, 0=no`==1~"YES",
                                          Motor<70~"YES",
                                          Language<70~"YES",
                                          Cognitive<70~"YES",
                                          TRUE~"NO"))

summary(as.factor(ciaradata$compNDIdeath))
summary(as.factor(ciaradata$compsevNDIdeath))
summary(as.factor(ciaradata$`NDI (CP or GDD without Bayley score) 1=yes, 0=no`))
ciaradata$`Grade of IVH 1-4=GRADES 5=PVL`<-as.factor(ciaradata$`Grade of IVH 1-4=GRADES 5=PVL`)
summary(ciaradata$`Grade of IVH 1-4=GRADES 5=PVL`)
ciaradata$`PVL 1=y, 0=n`<-as.factor(ciaradata$`PVL 1=y, 0=n`)
summary(ciaradata$`PVL 1=y, 0=n`)
ciaradata<-ciaradata%>%mutate(Deathsevivh=
                                case_when(`Grade of IVH 1-4=GRADES 5=PVL`==3~"Severe IVH/Death",
                                          `Grade of IVH 1-4=GRADES 5=PVL`==4~"Severe IVH/Death",
                                          `Grade of IVH 1-4=GRADES 5=PVL`==5~"Severe IVH/Death",
                                          `PVL 1=y, 0=n`==1~"Severe IVH/Death",
                                          `Death? 1=yes, 0=No`==1~"Severe IVH/Death",
                                          T~"No/Low grade IVH, Survival"))
summary(as.factor(ciaradata$Deathsevivh))
ciaradata$Deathsevivh<-as.factor(ciaradata$Deathsevivh)
#Univariate analysis for ciara at JENS
ciaradata$`FIRS grade`<-as.factor(ciaradata$`FIRS grade`)
summary(ciaradata$`FIRS grade`)
levels(ciaradata$`FIRS grade`)<-c(NA, "1", "2", "1", NA, NA,NA)
summary(ciaradata$`FIRS grade`)
ciaradata$`FIRS stage`<-as.factor(ciaradata$`FIRS stage`)
levels(ciaradata$`FIRS stage`)<-c(NA,NA, "1", "2", "3",NA, NA,NA)
summary(ciaradata$`FIRS stage`)
ciaradata$`FIRS grade`<-as.numeric(ciaradata$`FIRS grade`)
ciaradata$`FIRS stage`<-as.numeric(ciaradata$`FIRS stage`)
ciaradata<-ciaradata%>% mutate(FIRS_exposed=
                                 case_when(`FIRS grade`>=1~"YES",
                                           `FIRS stage`>=1~"YES",
                                           TRUE~"NO"))

summary(ciaradata$FIRS_exposed)
summary(as.factor(ciaradata$`Death? 1=yes, 0=No`))
ciaradata$compNDIdeath<-as.factor(ciaradata$compNDIdeath)
ciaradata$compsevNDIdeath<-as.factor(ciaradata$compsevNDIdeath)
ciaradata$FIRS_exposed<-as.factor(ciaradata$FIRS_exposed)
FIRSuniciara<-glm(compNDIdeath~FIRS_exposed+`Birth weight`+`Gestational age`,family = binomial("logit"),data = ciaradata)

tidy(FIRSuniciara
     ,
     exponentiate = TRUE,
     conf.int = TRUE)

FIRSmultisev<-glm(compsevNDIdeath~FIRS_exposed+`Birth weight`+`Gestational age`,family = binomial("logit"),data = ciaradata)

summary(FIRSmultisev)
summary(ciaradata$compNDIdeath)
ciaradata$compNDIdeath<-relevel(ciaradata$compNDIdeath, "YES")
exp(cbind(Odds_ratio=coef(FIRSuniciara), confint(FIRSuniciara)))
FIRSciaramotorlm<-lm(Motor~FIRS_exposed+`Birth weight`+`Gestational age`, data=ciaradata)
FIRSciaralanguage<-lm(Language~FIRS_exposed+`Birth weight`+`Gestational age`, data=ciaradata)
FIRSciaracogntive<-lm(Cognitive~FIRS_exposed+`Birth weight`+`Gestational age`, data=ciaradata)
summary(FIRSciaramotorlm)
summary(FIRSciaralanguage)
summary(FIRSciaracogntive)
Motorscoreplot<-ggplot(data=ciaradata, aes(x=FIRS_exposed, y=Motor, fill= FIRS_exposed))+
  geom_boxplot()
confint(FIRSciaramotorlm, "FIRS_exposedYES",level=0.95)
ciaradata$`ASD 1=yes`<-as.factor(ciaradata$`ASD 1=yes`)
summary(ciaradata$`ASD 1=yes`)
levels(ciaradata$`ASD 1=yes`)<-c("NO", "ASD", "NO")
FIRSciaraASDuni<-glm(`ASD 1=yes`~FIRS_exposed, family= binomial("logit"), data= ciaradata)
summary(FIRSciaraASDuni)
str(ciaradata$SESgroups)
ciaradata$SESgroups<-relevel(ciaradata$SESgroups, "Average")
summary(ciaradata$SESgroups)
SESctNDI<-glm(compNDIdeath~SESgroups, family=binomial("logit"), data=ciaradata)
summary(SESctNDI)
SESctsev<-glm(compsevNDIdeath~SESgroups, family=binomial("logit"), data=ciaradata)
summary(SESctsev)
ASDctSES<-glm(`ASD 1=yes`~SESgroups, family=binomial("logit"), data=ciaradata)
summary(ASDctSES)
ciaradata$`Death? 1=yes, 0=No`<-as.factor(ciaradata$`Death? 1=yes, 0=No`)
summary(ciaradata$`Death? 1=yes, 0=No`)
nodeadbabies<-ciaradata[ciaradata$`Death? 1=yes, 0=No`==0,]  
nodeadbabies<-nodeadbabies%>%mutate(LTF=
                                      case_when(nodeadbabies$`Length of follow up (months)`<20~"LOST",
                                                T~"Full Follow"))
summary(as.factor(nodeadbabies$LTF))
nodeadbabies$LTF<-as.factor(nodeadbabies$LTF)
nodeadbabies$LTF<-relevel(nodeadbabies$LTF, "Full Follow")
SESltf<-glm(LTF~SESgroups, family=binomial("logit"), data=nodeadbabies)
summary(SESltf)
Propfullfollow<-ggplot(data=subset(nodeadbabies,!is.na(SESgroups)), aes(x=SESgroups, fill=LTF))+
  geom_bar(position = "fill")
propfirsdeadndi<-ggplot(data=ciaradata, aes(x=FIRS_exposed, fill=compNDIdeath))+
  geom_bar(position="fill")
CtIVH<-glm(ciaradata$Deathsevivh~FIRS_exposed+`Birth weight`+`Gestational age`, family=binomial("logit"), data=ciaradata)
summary(CtIVH)
CtDead<-glm(ciaradata$`Death? 1=yes, 0=No`~FIRS_exposed+`Birth weight`+`Gestational age`, family=binomial("logit"), data=ciaradata)
summary(CtDead)
summary(ciaradata$`Length of follow up (months)`)
ciaradata<-ciaradata%>% mutate(weekofgestation=
                                 case_when(`Gestational age`<24~"23 weeks",
                                           `Gestational age`<25~"24 weeks",
                                           `Gestational age`<26~"25 weeks",
                                           `Gestational age`<27~"26 weeks",
                                           `Gestational age`<28~"27 weeks",
                                           `Gestational age`<29~"28 weeks",
                                           `Gestational age`<30~"29 weeks",
                                           `Gestational age`<31~"30 weeks",
                                           `Gestational age`<32~"31 weeks"))
stackedgestationalagecomp<-ggplot(data=subset(ciaradata,!is.na(weekofgestation)), aes(x=weekofgestation, fill=FIRS_exposed))+
  geom_bar(position = "fill")
summary(as.factor(ciaradata$weekofgestation))
noexprembabies<-ciaradata[ciaradata$`Gestational age`>25,] 
BigbabyIVH<-glm(Deathsevivh~FIRS_exposed+`Birth weight`+`Gestational age`, family=binomial("logit"), data=noexprembabies)
summary(BigbabyIVH)
NDIbigbaby<-glm(compsevNDIdeath~FIRS_exposed+`Birth weight`+`Gestational age`,family = binomial("logit"),data = noexprembabies)
summary(NDIbigbaby)
summary(ciaradata$FIRS_exposed)

###############################################################################

dim(dt)
dim(ciaradata)

setdiff(dt$study_num, ciaradata$`Study no`)  # in dt but not in ciaradata
setdiff(ciaradata$`Study no`, dt$study_num)  # in ciaradata but not in dt

ciaradata %>% filter(is.na(`Study no`))

sum(duplicated(ciaradata$`Study no`))
sum(duplicated(dt$study_num))

summary(dt$firs_exposed)
summary(ciaradata$FIRS_exposed)

summary(dt$birth_weight)
summary(ciaradata$`Birth weight`)

summary(dt$gest_age)
summary(ciaradata$`Gestational age`)

summary(dt$compNDIdeath)
summary(ciaradata$compNDIdeath)

summary(dt$compsevNDIdeath)
summary(ciaradata$compsevNDIdeath)


#########################
## Check coding levels ##
#########################

levels(dt$firs_exposed)
levels(ciaradata$FIRS_exposed)

table(dt$compNDIdeath)

# R treats “YES” as the baseline, so the model predicts odds of NO
table(ciaradata$compNDIdeath)
