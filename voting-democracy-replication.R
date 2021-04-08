# Replication code for Taylor C. Boas (2015), "Voting for Democracy: Campaign Effects in Chile's Democratic Transition," Latin American Politics & Society 57, 2. This article analyzes the survey "Estudio NP55," October 1988, conducted by the Centro de Estudios de la Realidad Contemporánea. The replication file contains only those variables used in the analysis, which I have been given permisson to post online. Anyone interested in gaining access to the full file should contact Carlos Huneeus, Director of CERC, at corpcerc@rdc.cl. The questionnaire is archived at the Harvard Dataverse Network (http://thedata.harvard.edu/dvn/, same location as this file) and is also reproduced in J. L. Piñuel Raigada (1990), "La cultura pol�?tica del ciudadano y la comunicación pol�?tica en TV, en la transición pol�?tica del plebiscito chileno (octubre 1988)," Revista Española de Investigaciones Sociales 50, 90: 125-237.

rm(list=ls(all=T))

# Set working directory as appropriate

setwd('D:\tiago\Downloads\Direction Year\Classes\CS112\Final Project') #Set your wd

# Load packages

library(Matching)
library(nnet)
library(Hmisc)
library(mvtnorm)
library(ggplot2)
library(foreign)
library(car)

#Set random seed, allowing consistent matching with GenMatch()
set.seed(0)

# =====================
# Load replication file
# =====================

# The file contains the following objects:
#
# CERC: Survey data file (only those variables used in the analysis, as noted above).
# polls: Vote intention for plebiscite options, from internal surveys by CIS, used to reproduce Figure 1.
# gen1.12fi, gen1.13fi: Objects of class GenMatch, returned by the function GenMatch() in the Matching package, containing the matches used in the analysis. The code that generated these objects is below but is commented out. One should not expect running the code to reproduce these objects exactly, since (at least at the time the analysis was run) the psuedo-random solution returned by GenMatch() is platform-specific.

CERC <- load('voting_democracy_replication.RData')

# ==============================================================
# Create new variables for analysis by recoding from survey file
# ==============================================================

religion<-recode(CERC$RELIG,"1='catholic';2:8='evangelical';
    10:11='other';12:13='none';else=NA")
religion <- as.factor(religion)
practicing<-recode(CERC$P6,"1='very';2='yes';3='not very';
    4='no';else=NA")
practicing <- as.factor(practicing)

religious<-4-as.numeric(practicing) # More religious higher
religious[religion=='none']<-0 # No religion = not religious
religious[is.na(religious)]<-mean(religious,na.rm=T) # NA's get mean values

medium<-ifelse(!CERC$P7,NA,CERC$P7)
medium<-factor(medium,labels=c('TV','newspaper','radio',
                               'magazines','other','TV/newspaper','TV/radio',
                               'TV/newspaper/radio'))
TVinfo<-medium %in% c('TV','TV/newspaper','TV/radio',
                      'TV/newspaper/radio')

freq.TVnews<-ifelse(!CERC$P141,NA,CERC$P141)
TVnews<-4-as.numeric(freq.TVnews) # Higher is more often
TVnews[is.na(TVnews)]<-mean(TVnews,na.rm=T)

freq.TVmovie<-ifelse(!CERC$P143,NA,CERC$P143)
TVmovie<-4-as.numeric(freq.TVmovie) # Higher is more often
TVmovie[is.na(TVmovie)]<-mean(TVmovie,na.rm=T)

freq.TVsoaps<-ifelse(!CERC$P146,NA,CERC$P146)
TVsoaps<-4-as.numeric(freq.TVsoaps) # Higher is more often
TVsoaps[is.na(TVsoaps)]<-mean(TVsoaps,na.rm=T)

vote.full<-CERC$VOTO
vote.full[!vote.full]<-4
vote.full<-factor(vote.full,labels=c('yes','no',
                                     'blank/none','no response'))

urban<-CERC$CIU %in% c(11,12,13,19)

police.widow<-ifelse(!CERC$P211,NA,CERC$P211)==1
youth.protest<-ifelse(!CERC$P212,NA,CERC$P212)==2
dark.tunnel<-ifelse(!CERC$P213,NA,CERC$P213)==1
mother.cazzely<-ifelse(!CERC$P215,NA,CERC$P215)==2
yes.correct<-apply(cbind(police.widow,dark.tunnel),1,sum,na.rm=T)
no.correct<-apply(cbind(youth.protest,mother.cazzely),1,sum,na.rm=T)
reception<-(yes.correct > no.correct) +
  2 * (no.correct > yes.correct) +
  3 * (yes.correct + no.correct == 4) +
  4 * (yes.correct == 1 & no.correct == 1)
reception<-factor(reception,labels=c('None','Yes','No','Both','Half'))

educ<-ifelse(!CERC$REDUC,NA,CERC$REDUC)
educ<-factor(educ,labels=c('none','some primary',
                           'primary','some secondary','secondary','some college',
                           'college'))
education<-7-as.numeric(educ) # Higher is more education
education[is.na(educ)]<-mean(education,na.rm=T)

sex<-factor(CERC$SEXO,labels=c('male','female'))
male<-sex=='male' 

age<-CERC$EDAD

income<-ifelse(CERC$INGRE %in% c(0,10),NA,CERC$INGRE)
income[is.na(income)]<-mean(income,na.rm=T)

student<-CERC$SOCUP==8|CERC$OCUPRE==4
housewife<-CERC$OCUPRE==1&CERC$SOCUP!=8
unemployed<-CERC$SOCUP==(CERC$SOCUP==3&CERC$OCUPRE %in% c(5,25,99)|
                           CERC$SOCUP==7&CERC$OCUPRE==99)
retired<-CERC$SOCUP %in% 5:6|CERC$OCUPRE==3
indep.wealth<-CERC$SOCUP==4&CERC$OCUPRE==5
worker<-(CERC$OCUPRE %in% 6:11)&!student
whitecol<-CERC$OCUPRE %in% 12:17
military<-CERC$OCUPRE %in% 18:19
professional<-CERC$OCUPRE %in% 20:21
farmer<-CERC$OCUPRE %in% 22:23
otherjob<-(CERC$OCUPRE %in% c(0,24,49,99)&CERC$SOCUP==1)|
  (CERC$OCUPRE==99&CERC$SOCUP==2)

works<-worker|whitecol|military|professional|farmer|otherjob

R1<-CERC$CIU %in% 1:2
R2<-CERC$CIU %in% 3:4
R3<-CERC$CIU == 5
R4<-CERC$CIU %in% 6:7
R5<-CERC$CIU %in% 8:12
RM<-CERC$CIU == 13
R6<-CERC$CIU == 14
R7<-CERC$CIU %in% 15:16
R8<-CERC$CIU %in% 17:22
R9<-CERC$CIU == 23
R10<-CERC$CIU %in% 24:26

woman.home<-ifelse(!CERC$P481,NA,CERC$P481)
woman.home<-ifelse(is.na(woman.home),mean(woman.home,
                                          na.rm=T),woman.home)
woman.kids<-ifelse(!CERC$P482,NA,CERC$P482)
woman.kids<-ifelse(is.na(woman.kids),mean(woman.kids,
                                          na.rm=T),woman.kids)

opp.paper<-CERC$P8 %in% c(2,5)
opp.radio<-CERC$P10==2
UCTV<-CERC$P12==4
TVN<-CERC$P12==2

final.data<-data.frame(no.correct,yes.correct,reception,education,income,age,religious,male,works,urban,TVnews,TVinfo,R1,R2,R3,R4,R5,R6,R7,R8,R9,R10,opp.paper,opp.radio,UCTV,TVN,TVsoaps,TVmovie,vote.full,woman.home,woman.kids)


# =====================================================
# Table 2. Balance Statistics Before and After Matching
# =====================================================

CERC.Tr12<-final.data[final.data$reception %in% c('Yes','No'),]
CERC.Tr13<-final.data[final.data$reception %in% c('Yes','Both'),]

Tr12<-CERC.Tr12$reception=='Yes'
CERC.Tr12<-cbind(Tr12,CERC.Tr12)
Tr13<-CERC.Tr13$reception=='Yes'
CERC.Tr13<-cbind(Tr13,CERC.Tr13)

pTr12<-glm(Tr12~ education + I(education^2) + income +
             I(income^2) + age + I(age^2) + religious + 
             I(religious^2) + male + works + TVnews + I(TVnews^2) +
             TVinfo + opp.paper + opp.radio + UCTV + R1 + R2 + R3
           + R4 + R5 + R6 + R7 + R8 + R9 + R10 +
             TVN + urban, family=binomial, data=CERC.Tr12)

pTr13<-glm(Tr13~ education + I(education^2) + income +
             I(income^2) + age + I(age^2) + religious + 
             I(religious^2) + male + works + TVnews + I(TVnews^2) +
             TVinfo + opp.paper + opp.radio + UCTV + R1 + R2 + R3
           + R4 + R5 + R6 + R7 + R8 + R9 + R10 +
             TVN + urban, family=binomial, data=CERC.Tr13)

X12<-cbind(pTr12$linear.predictors,CERC.Tr12[,c('education',
                                                'income','age','religious','male','works','urban',
                                                'TVnews','TVinfo','R1','R2','R3','R4','R5','R6','R7',
                                                'R8','R9','R10','opp.paper','opp.radio','UCTV','TVN')])

X13<-cbind(pTr13$linear.predictors,CERC.Tr13[,c('education',
                                                'income','age','religious','male','works','urban',
                                                'TVnews','TVinfo','R1','R2','R3','R4','R5','R6','R7',
                                                'R8','R9','R10','opp.paper','opp.radio','UCTV','TVN')])

BM12<-cbind(X12,X12$education^2,X12$income^2,X12$age^2,
            X12$religious^2,X12$TVnews^2,X12$male*X12$TVN,
            X12$opp.paper*X12$TVN,X12$TVinfo*X12$opp.paper,
            X12$works*X12$opp.radio,X12$works*X12$TVN,
            X12$education*X12$urban,X12$male*X12$income,
            X12$income*X12$UCTV,X12$age*X12$male,X12$TVinfo*X12$TVN,
            X12$opp.paper*X12$UCTV,X12$urban*X12$TVnews,
            X12$male*X12$opp.paper,X12$education*X12$age)

BM13<-cbind(X13,X13$education^2,X13$income^2,X13$age^2,
            X13$religious^2,X13$TVnews^2,X13$education*X13$religious,
            X13$age*X13$works,X13$male*X13$works,X13$works*X13$TVinfo,
            X13$urban*X13$opp.paper,X13$urban*X13$opp.radio,
            X13$religious*X13$opp.paper,X13$works*X13$TVnews,
            X13$TVnews*X13$TVN,X13$TVinfo*X13$opp.radio,X13$TVinfo*
              X13$TVN,X13$education*X13$male,X13$income*X13$male,
            X13$income*X13$opp.radio,X13$age*X13$male,X13$urban*
              X13$TVN,X13$age*X13$opp.paper,X13$religious*X13$male,
            X13$male*X13$urban,X13$male*X13$TVnews,X13$male*X13$TVN,
            X13$opp.radio*X13$TVN,X13$religious*X13$TVN,X13$TVnews*
              X13$opp.paper,X13$income*X13$UCTV)

# The two lines below generate the matches, contained in the objects gen1.12fi and gen1.13fi. These objects have already been loaded as part of the replication file. Uncomment these lines to regenerate them from scratch. See note, above, under "Load replication file."

gen1.12fi<-GenMatch(Tr=Tr12,X=X12,BalanceMatrix=BM12,pop.size=1000)
gen1.13fi<-GenMatch(Tr=Tr13,X=X13,BalanceMatrix=BM13,pop.size=1000)

mgen1.12fi<-Match(Tr=Tr12,X=X12,Weight.matrix=gen1.12fi)
mgen1.13fi<-Match(Tr=Tr13,X=X13,Weight.matrix=gen1.13fi)

balance.12<-MatchBalance(Tr12~education + I(education^2) + income +
                           I(income^2) + age + I(age^2) + religious + 
                           I(religious^2) + male + works + urban + 
                           TVnews + I(TVnews^2) + TVinfo + R1 + R2 + R3 + R4 +
                           R5 + R6 + R7 + R8 + R9 + R10 + opp.paper + opp.radio +
                           UCTV + TVN + woman.home + woman.kids, match.out=mgen1.12fi, nboots=1000, data=CERC.Tr12)

balance.13<-MatchBalance(Tr13~education + I(education^2) + income +
                           I(income^2) + age + I(age^2) + religious + 
                           I(religious^2) + male + works + urban + 
                           TVnews + I(TVnews^2) + TVinfo + R1 + R2 + R3 + R4 +
                           R5 + R6 + R7 + R8 + R9 + R10 + opp.paper + opp.radio +
                           UCTV + TVN + woman.home + woman.kids, match.out=mgen1.13fi, nboots=1000, data=CERC.Tr13)

min.pval.no.before<-sapply(balance.12$BeforeMatching,function(x){min(x$ks$ks.boot.pvalue,x$tt$p.value)})
min.pval.no.after<-sapply(balance.12$AfterMatching,function(x){min(x$ks$ks.boot.pvalue,x$tt$p.value)})
min.pval.both.before<-sapply(balance.13$BeforeMatching,function(x){min(x$ks$ks.boot.pvalue,x$tt$p.value)})
min.pval.both.after<-sapply(balance.13$AfterMatching,function(x){min(x$ks$ks.boot.pvalue,x$tt$p.value)})

sdiff.no.before<-sapply(balance.12$BeforeMatching,function(x) x$sdiff)
sdiff.no.after<-sapply(balance.12$AfterMatching,function(x) x$sdiff)
sdiff.both.before<-sapply(balance.13$BeforeMatching,function(x) x$sdiff)
sdiff.both.after<-sapply(balance.13$AfterMatching,function(x) x$sdiff)

balance.table<-round(data.frame(sdiff.no.before,sdiff.no.after,min.pval.no.before,min.pval.no.after,sdiff.both.before,sdiff.both.after,min.pval.both.before,min.pval.both.after),2)

bottom.note.balance<-paste("Note: Standardized mean difference is 100 times the mean difference of the treated and control observations divided by the standard deviation of the treated observations. Minimium p-values are from bootstrapped Kolmogorov-Smirnov (KS) tests or mean difference t-tests (two-sample before matching, paired after matching). Chile's Regions 11-–12 were not sampled by the survey. Urban is an indicator variable for residing in Santiago, Valpara\\'iso, Vi\\~na del Mar, or Concepci\\'on.")

#balance.table.latex<-latex(balance.table,file='balance_table.tex',collabel.just=rep('r',8),col.just = rep('r',8),rowname=c("Education","Education$^2$","Income","Income$^2$","Age","Age$^2$","Religiosity","Religiosity$^2$","Male","Employed","Urban","TVnews","TVnews$^2$","TVinfo","Region 1","Region 2","Region 3","Region 4","Region 5","Region 6","Region 7","Region 8","Region 9","Region 10","Opp.\\ Paper","Opp.\\ Radio","UCTV","TVN","Woman-Home","Woman-Kids"),rowlabel='',colheads=rep(c("StdDiff","StdDiff","MinPval","MinPval"),2),extracolheads=rep(c("Before","After"),4),extracolsize="normalsize",caption = 'Balance Statistics Before and After Matching',insert.bottom=bottom.note.balance,booktabs = F, ctable = T, where = "htp",cgroup=c("Yes vs.\\ No", "Yes vs.\\ Both"),n.cgroup=c(4,4),rgroup=c('Variable','Placebo Tests'),n.rgroup=c(28,2))