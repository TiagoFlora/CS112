
##################################
##################################
######## CAUSAL INFERENCE ########
##################################
##################################

#Setting up libraries
library(rbounds)
library(Matching)
library(arm)
set.seed(8008)

#################
### Problem 1 ###
#################

# Part A. 
# The code in this part tries to find match balance before doing the matching itself.
# It takes the output of “genout”, which calls GenMatch and is thus a weight matrix, to evaluate match balance, even though no matching is being performed by GenMatch.

# Part B
# The arguments passed to the GenMatch function are not the same as the ones passed to Match() later on.
# Specifically, there is no “caliper” argument in Match, which defaults it to NULL, and the “estimand” argument would  also be a problem were it not defaulted to “ATT” or if we had another estimand in GenMatch.

# Part C
# Y is defined only as “re78”, without specifying the dataset the variable is in.
# This means that will be a traceback error.


#################
### Problem 2 ###
#################

foo <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")

foo2 <- foo[, c(6:8, 11:16, 99, 50, 114, 49, 63, 136, 109, 126, 48, 160, 142, 10)]

# remove 2 rows with missing data (there are better ways to handle missing data)
foo2 <- na.omit(foo2)

# take a peek at the data set (identify the columns)
head(foo2)
summary(foo2)

glm.orig <- glm(pbs2s3 ~ wartype + logcost + wardur + factnum + factnum2 + 
              trnsfcap + develop + exp + decade + treaty + untype4,
            data = foo2, family = 'binomial')

glm.interac <- glm(pbs2s3 ~ wartype + logcost + wardur + factnum + factnum2 + 
                     trnsfcap + develop + exp + decade + treaty + untype4 + 
                     I(foo2$exp*foo2$untype4) + I(foo2$wardur*foo2$logcost), 
                   data = foo2, family = 'binomial')

# Geographic variables were not taken into account in Gary King's model of interest
mean.wartype <- mean(foo2$wartype)
mean.logdead <- mean(foo2$logcost)

# In this case, all variables but wardur (which we plot) are held at their means
# mean.wardur <- mean(foo2$wardur)

mean.factnum <- mean(foo2$factnum)
mean.factnum2 <- mean(foo2$factnum2)
mean.trnsfcap <- mean(foo2$trnsfcap)
mean.develop <- mean(foo2$develop)
mean.exp <- mean(foo2$exp)
mean.decade <- mean(foo2$decade)
mean.treaty <- mean(foo2$treaty)

# Hold interaction terms at their means as well
mean.exptreat <- mean(foo2$exp*foo2$untype4)
mean.durlogcost <- mean(foo2$wardur*foo2$logcost)

# How many wars in the data set had peacebuilding missions?
# mean.untype4 <- mean(foo2$untype4)

# There is only one observation with 'wardur' above 315 months, at 600 months
# We discard the observation as it is an outlier too far from the range of the remaining data
length(which(foo2$wardur>315))


logits <- function(Xs, coef) {
  logit <- coef[1] + sum(coef[2:length(coef)]*Xs)
  return(exp(logit) / (1 + exp(logit)))
}

# Create empty vectors to store war durations
# Each vector has a length of 315 as that is 
# the maximum duration plotted in King (2007)

wardur.origtreat <- rep(NA, 315)
wardur.origctrl <- rep(NA, 315)

for (k in 1:315) {
  Xs.orig.treat <- c(mean.wartype, mean.logdead, k, mean.factnum, mean.factnum2, 
                     mean.trnsfcap, mean.develop, mean.exp, mean.decade, mean.treaty, 1)
  
  Xs.orig.ctrl <- c(mean.wartype, mean.logdead, k, mean.factnum, mean.factnum2, 
                    mean.trnsfcap, mean.develop, mean.exp, mean.decade, mean.treaty, 0)
  
  wardur.origtreat[k]  <- logits(Xs.orig.treat, coef(glm.orig))
  wardur.origctrl[k]  <- logits(Xs.orig.ctrl, coef(glm.orig))
}

original.y <- wardur.origtreat - wardur.origctrl

wardur.interac.treat <- rep(NA, 315)
wardur.interac.ctrl <- rep(NA, 315)

for (i in 1:315) {
  Xs.interac.treat <- c(mean.wartype, mean.logdead, i, mean.factnum, mean.factnum2, 
                        mean.trnsfcap, mean.develop, mean.exp, mean.decade, mean.treaty, 1, 1*mean.exp,
                        i*mean.logdead)
  Xs.interac.ctrl <- c(mean.wartype, mean.logdead, i, mean.factnum, mean.factnum2, 
                 mean.trnsfcap, mean.develop, mean.exp, mean.decade, mean.treaty, 0, 0*mean.exp, i*mean.logdead)
  
  wardur.interac.treat[i]  <- logits(Xs.interac.treat, coef(glm.interac))
  wardur.interac.ctrl[i]  <- logits(Xs.interac.ctrl, coef(glm.interac))
}


interac_y <- wardur.interac.treat - wardur.interac.ctrl

plot(1:315, original.y, xlab = "Duration of wars in months", ylab="Marginal effects of UN peacekeeping operations",
     type = "l", ylim = c(0, 0.8), main = 'Causal effect of multidimensional UN peacekeeping operations')
lines(1:315, interac_y, col = "blue", ylim = c(0, 0.8))
legend(1, 95, legend=c("Original model", "Model with interaction terms"),
              col=c("blue", "red"), lty=1:2, cex=0.8)


#################
### Problem 3 ###
#################

## If we are to only select the variables to be used in problems 3 & 4, we can use the
# the following code to only pick those columns of the dataset

## Find 'uncint', 'pbs2l', and 'pbs5l
# which(foo=='Enforcement', arr.ind = T)
# which(colnames(foo)=='pbs2l')
# which(colnames(foo)=='pbs5l')

## Keep 'uncint', 'pbs2l', and 'pbs5l
foo3 <- foo[, c(6:8, 11:16, 99, 50, 34, 35, 52, 114, 49, 63, 136, 109, 126, 48, 160, 142, 10)]

## Remove rows with missing data 
foo3 <- na.omit(foo3)

# Create a vector full of zeroes of the same length
# as there are data points for 'uncint'
Tr <- rep(0, length(foo3$uncint))
Tr[which(foo3$uncint != "None")] <- 1

foo3$Tr <- Tr

#################
### Problem 4 ###
#################

## Logistic regressions

# Check balance for logistic regressions
mbinit <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + 
                         trnsfcap + develop + exp + decade + treaty + 
                         I(foo3$logcost*foo3$wardur) + I(foo3$factnum*foo3$logcost),
                       data = foo3, nboots=500)

# From mbinit, we can see that there is quite a lot of difference in logcost
# between the treatment and control groups, with treatment group wars having more deaths
# There are even more noticeable differences to keep in mind in regard to ineraction terms
# between the human cost and the number of factions in the conflict


glm2l <- glm(pbs2l ~ wartype + logcost + wardur + factnum + 
               trnsfcap + develop + exp + decade + treaty + 
               I(foo3$logcost*foo3$wardur) + I(foo3$factnum*foo3$logcost) + Tr,
             data = foo3, family = 'binomial')
summary(glm2l)


glm5l <- glm(pbs5l ~ wartype + logcost + wardur + factnum + 
               trnsfcap + develop + exp + decade + treaty + 
               I(foo3$logcost*foo3$wardur) + I(foo3$factnum*foo3$logcost) + Tr,
             data = foo3, family = 'binomial')
summary(glm5l)

# Below, there are calculations of treatment effect estimates by considering
# the unit-level difference in outcomes obtained by simulating counterfactuals
# This was not included in the table in the responses as it is part of 
# last year's answer key and is not my own work, but it is worth it 
# to compare the estimates and the coefficients

# Two years
# foo.counter_factual <- foo3
# foo.counter_factual$Tr <- 1 - foo3$Tr
# counter.factuals <- predict(glm2l, newdata=foo.counter_factual, type="response")
# unit_treat_effects <- rep(NA, nrow(foo3))

# mask <- foo3$Tr == 1
# unit_treat_effects[mask] <- glm2l$fitted.values[mask] - counter.factuals[mask]
# unit_treat_effects[!mask] <- counter.factuals[!mask] - glm2l$fitted.values[!mask]
# mean(unit_treat_effects)

# Out[1] = 0.09374585

# Five years
# foo5.counter_factual <- foo3
# foo5.counter_factual$Tr <- 1 - foo3$Tr
# counter.factuals5 <- predict(glm5l, newdata=foo5.counter_factual, type="response")
# unit_treat_effects5 <- rep(NA, nrow(foo3))

# mask <- foo3$Tr == 1
# unit_treat_effects5[mask] <- glm5l$fitted.values[mask] - counter.factuals5[mask]
# unit_treat_effects5[!mask] <- counter.factuals5[!mask] - glm5l$fitted.values[!mask]
# mean(unit_treat_effects5)

# Out[2] = 0.126225

### Propensity Score Matching

glmps <- glm(Tr ~ wartype + logcost + wardur + factnum + 
               trnsfcap + develop + exp + decade + treaty + 
               I(foo3$logcost*foo3$wardur) + I(foo3$factnum*foo3$logcost), data = foo3, family = 'binomial')

summary(glmps)

pscores <- glmps$fitted.values
Y2l <- foo3$pbs2l
Y5l <- foo3$pbs5l

m2lps <- Match(Y=Y2l, Tr=Tr, X=pscores, M=1, BiasAdjust = T, replace = T)
summary(m2lps)

m2lps$est.noadj

mb2lps <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + 
                      trnsfcap + develop + exp + decade + treaty + 
                      I(foo3$logcost*foo3$wardur) + I(foo3$factnum*foo3$logcost), data = foo3, 
                    match.out = m2lps, nboots=500)

m5lps  <- Match(Y=Y5l, Tr=Tr, X=pscores, M=1, BiasAdjust = T, replace = T)
summary(m5lps)

m5lps$est.noadj

mb5lps <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + 
                      trnsfcap + develop + exp + decade + treaty + 
                      I(foo3$logcost*foo3$wardur) + I(foo3$factnum*foo3$logcost), data = foo3, 
                    match.out = m5lps, nboots=500)


### Genetic Matching w/o Propensity Scores

attach(foo3)
X4 <- cbind(wartype, logcost, foo3$wardur, factnum, 
            factnum2, trnsfcap, develop, exp, decade, treaty, I(foo3$logcost*foo3$wardur), I(foo3$factnum*foo3$logcost))

Xps <- cbind(glmps$fitted.values, wartype, logcost, foo3$wardur, factnum, 
            factnum2, trnsfcap, develop, exp, decade, treaty, I(foo3$logcost*foo3$wardur), I(foo3$factnum*foo3$logcost))
detach(foo3)

genout <- GenMatch(Tr=Tr, X=X4, M=1, replace = T, pop.size=200, max.generations=10, wait.generations=25)
genout2 <- GenMatch(Tr=Tr, X=X4, M=1, replace = T, pop.size=200, max.generations=10, wait.generations=25)

match.nops2l <- Match(Y=Y2l, Tr=Tr, X=X4, M=1, BiasAdjust = T, Weight.matrix = genout, replace = T)
summary(match.nops2l)

match.nops2l$est.noadj

match.nops5l <- Match(Y=Y5l, Tr=Tr, X=X4, M=1, BiasAdjust = T, Weight.matrix = genout2, replace = T)
summary(match.nops5l)

match.nops5l$est.noadj

mbnops2l <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + 
                           trnsfcap + develop + exp + decade + treaty + 
                           I(foo3$logcost*foo3$wardur) + I(foo3$factnum*foo3$logcost), data = foo3, 
                         match.out = match.nops2l, nboots=500)

mbnops5l <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + 
                           trnsfcap + develop + exp + decade + treaty + 
                           I(foo3$logcost*foo3$wardur) + I(foo3$factnum*foo3$logcost), data = foo3, 
                         match.out = match.nops5l, nboots=500)

### Genetic Matching w/ Propensity Scores

genoutps <- GenMatch(Tr=Tr, X=Xps, M=1,
                   pop.size=200, max.generations=10, wait.generations=25, replace = T)

genoutps2 <- GenMatch(Tr=Tr, X=Xps, M=1,
                     pop.size=200, max.generations=10, wait.generations=25, replace = T)


gm2lps  <- Match(Y=Y2l, Tr=Tr, X=Xps, M=1, BiasAdjust = T, Weight.matrix = genoutps, replace = T)
summary(gm2lps)

gm2lps$est.noadj

gmb2lps <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + 
                      trnsfcap + develop + exp + decade + treaty + 
                      I(foo3$logcost*foo3$wardur) + I(foo3$factnum*foo3$logcost), data = foo3, 
                    match.out = gm2lps, nboots=500)

gm5lps <- Match(Y=Y5l, Tr=Tr, X=Xps, M=1, BiasAdjust = T, Weight.matrix = genoutps2, replace = T)
summary(gm5lps)

gm5lps$est.noadj

gmb5lps <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + 
                      trnsfcap + develop + exp + decade + treaty + 
                      I(foo3$logcost*foo3$wardur) + I(foo3$factnum*foo3$logcost), data = foo3, 
                    match.out = gm5lps, nboots=500)



