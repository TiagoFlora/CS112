################################
# Regression and Bootstrapping #
################################

library(formattable)
library(arm)
library(dplyr)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(ggplot2)
library(ggpmisc)

###############
## Problem 1 ##
###############

# Set seed for reproducibility
set.seed(1880)
# Item (a)
# Generates X and Y values and puts them in a dataframe
xvar <- rnorm(998, 2)
yvar <- 16 - 4.2*xvar + rnorm(998, 0, 4.5)
df_vars <- data.frame(xvar, yvar)

# Item (b)
lm.998 <- lm(yvar ~ xvar, data = df_vars)
summary(lm.998)
tab_model(lm.998)

# Item (c)
# Generating 
df_outl <- rbind(df_vars, c(63, 106), c(-10,-35))
lm.outl <- lm(yvar ~ xvar, data = df_outl)
summary(lm.outl)
tab_model(lm.outl)

# Item (d)
plot(df_outl$xvar, df_outl$yvar, main = "Regression models with and without outliers",
     xlab = "X variable", ylab = "Y variable")
abline(a = coef(lm.998)[1],
       b = coef(lm.998)[2],
       col = "blue", lwd = 3)
abline(a = coef(lm.outl)[1],
       b = coef(lm.outl)[2],
       col = "red", lwd = 3)
legend(20, 70, legend = c("Data w/o outliers", "Data w/ outliers"), col = c("blue", "red"), lty = 1,cex = 0.8)

###############
## Problem 2 ##
###############

library(Matching)
data(lalonde)

age_sq <- c((lalonde$age)**2)
lalonde$age_sqr <- age_sq
  
# Item (a) #
treat.lalonde <- lalonde[which(lalonde$treat == 1), ]

# Define the regression model w/o age*treat & treat because of singularities
## age*treat is equal to age when considering only treated units,
## and equal to treat when considering only control units
lm.treat <- lm(re78 ~ age + age_sqr + educ + re74 + re75, data=treat.lalonde)


## Number of simulated predictions
## We will use this value for all subsequent bootstrapping and iterating

n_its <- 10000
sim.treat <- sim(lm.treat, n.sims = n_its)

# Create a matrix to store simulated predictions for every age
ytilde.treat <- matrix(NA, nrow = n_its, ncol = length(
  min(lalonde$age):max(lalonde$age)))

# Store means of educ, re74 and re75 for later use
mean.educ <- mean(treat.lalonde$educ)
mean.re74 <- mean(treat.lalonde$re74)
mean.re75 <- mean(treat.lalonde$re75)

# Loop through every age in the range of the treated units' data
for(age_it in min(lalonde$age):max(lalonde$age)) {
  predictors <- c(1, age_it, age_it^2, mean.educ, mean.re74, mean.re75)
  #Simulate 10000 predictions of re78 for every age in the age range
  for(k in 1:n_its) {
    ytilde.treat[k, age_it - min(lalonde$age) + 1] <- sum(predictors*sim.treat@coef[k,])
  }
}

# Creates table with prediction bounds and means
confint.treat <- apply(ytilde.treat, 2, quantile, probs = c(0.025, 0.975))
table.treat <- data.frame(min(lalonde$age):max(lalonde$age), t(confint.treat),
                          mean.educ, mean.re74, mean.re75)
names(table.treat)[1] <- "Age"
names(table.treat)[2] <- "2.5%"
names(table.treat)[3] <- "97.5%"
names(table.treat)[4] <- "Mean 'educ'"
names(table.treat)[5] <- "Mean 're74'"
names(table.treat)[6] <- "Mean 're75'"

tab_df(table.treat)

# Plots prediction intervals for all ages in lalonde
plot(x = c(1:100), y = c(1:100), type = "n", 
     xlim = c(min(lalonde$age),max(lalonde$age)), 
     ylim = c(-10000,20000), 
     main = "re78 by age for treated units", xlab = "Age (years)", 
     ylab = "re78 ($)")

for(age_it in min(lalonde$age):max(lalonde$age)) {
  segments(
    x0 = age_it,
    y0 = confint.treat[1, age_it - min(lalonde$age) + 1],
    x1 = age_it,
    y1 = confint.treat[2, age_it - min(lalonde$age) + 1],
    lwd = 2)
}

# Item (b) #


ctrl.lalonde <- lalonde[which(lalonde$treat == 0), ]
lm.ctrl <- lm(re78 ~ age + age_sqr + educ + re74 + re75, data=ctrl.lalonde)

sim.ctrl <- sim(lm.ctrl, n.sims = n_its)

# Create another matrix to store simulated predictions for every age in control units
ytilde.ctrl <- matrix(NA, nrow = n_its, ncol = length(
  min(lalonde$age):max(lalonde$age)))

# Loop through every age in the range of the control units' data
for(age_it in min(lalonde$age):max(lalonde$age)) {
  predictors <- c(1, age_it, age_it^2, mean.educ, mean.re74, mean.re75)
  #Simulate 10000 predictions of re78 for every age in the age range
  for(k in 1:n_its) {
    ytilde.ctrl[k, age_it - min(lalonde$age) + 1] <- sum(predictors*sim.ctrl@coef[k,])
  }
}

confint.ctrl <- apply(ytilde.ctrl, 2, quantile, probs = c(0.025, 0.975))
table.ctrl <- data.frame(min(lalonde$age):max(lalonde$age), t(confint.ctrl),
                         mean.educ, mean.re74, mean.re75)
names(table.ctrl)[1] <- "Age"
names(table.ctrl)[2] <- "2.5%"
names(table.ctrl)[3] <- "97.5%"
names(table.ctrl)[4] <- "Mean 'educ'"
names(table.ctrl)[5] <- "Mean 're74'"
names(table.ctrl)[6] <- "Mean 're75'"

tab_df(table.ctrl)

plot(x = c(1:100), y = c(1:100), type = "n", 
     xlim = c(min(lalonde_ctrl$age),max(lalonde_ctrl$age)), 
     ylim = c(-10000,20000), 
     main = "re78 by age for control units", xlab = "Age (years)", 
     ylab = "re78 ($)")

for (age in min(lalonde$age):max(lalonde$age)) {
  segments(
    x0 = age,
    y0 = confint.ctrl[1, age - min(lalonde$age) + 1],
    x1 = age,
    y1 = confint.ctrl[2, age - min(lalonde$age) + 1],
    lwd = 2)
}

# Item (c) #
##################################################################
#    For this item, we consider two interpretations of the       #
#    question, with only treatment coefficients in lm.teff1      #
#    or with treatment effect defined by all variables           #
#    of the initial model in lm.teff2                            #
##################################################################

## 
lm.teff1 <- lm(re78 ~ treat + treat*age, data = lalonde)
sim.teff1 <- sim(lm.teff, n.sims = n_its)

lm.teff2 <- lm(re78 ~ age + age_sqr + educ + re74 + re75 + treat + treat*age, data=lalonde)
sim.teff2 <- sim(lm.teff2, n.sims = n_its)

ytilde.teff1 <- matrix(NA, nrow = n_its, ncol = length(
  min(lalonde$age):max(lalonde$age)))

ytilde.teff2 <- matrix(NA, nrow = n_its, ncol = length(
  min(lalonde$age):max(lalonde$age)))

# Loops through a model that has all the variables 
for(age_it in min(lalonde$age):max(lalonde$age)) {
  # We hold treat's value as the proportion of treatment group units
  predictors <- c(1, mean(lalonde$treat), age_it*mean(lalonde$treat))
  #Simulate 10000 predictions of treatment for every age in the age range
  for(k in 1:n_its) {
    ytilde.teff1[k, age_it - min(lalonde$age) + 1] <- sum(predictors*sim.teff1@coef[k,])
  }
}

confint.teff1 <- apply(ytilde.teff1, 2, quantile, probs = c(0.025, 0.975))
table.teff1 <- data.frame(min(lalonde$age):max(lalonde$age), t(confint.teff1),
                          mean.educ, mean.re74, mean.re75)
names(table.teff1)[1] <- "Age"
names(table.teff1)[2] <- "2.5%"
names(table.teff1)[3] <- "97.5%"
names(table.teff1)[4] <- "Mean 'educ'"
names(table.teff1)[5] <- "Mean 're74'"
names(table.teff1)[6] <- "Mean 're75'"

tab_df(table.teff1)

plot(x = c(1:100), y = c(1:100), type = "n", 
     xlim = c(min(lalonde$age),max(lalonde$age)), 
     ylim = c(-5000,20000), 
     main = "re78 treatment effect by age", xlab = "Age (years)", 
     ylab = "re78 ($)")

for (age_it in min(lalonde$age):max(lalonde$age)) {
  segments(
    x0 = age_it,
    y0 = confint.teff1[1, age_it - min(lalonde$age) + 1],
    x1 = age_it,
    y1 = confint.teff1[2, age_it - min(lalonde$age) + 1],
    lwd = 2)
}

# Loops through a model that only takes treatment and age as a predictors
for(age_it in min(lalonde$age):max(lalonde$age)) {
  # We hold treat's value as the proportion of treatment group units
  predictors <- c(1, age_it, age_it^2, mean.educ, mean.re74, mean.re75, 
                  mean(lalonde$treat), age_it*mean(lalonde$treat))
  #Simulate 10000 predictions of treatment for every age in the age range
  for(k in 1:n_its) {
    ytilde.teff2[k, age_it - min(lalonde$age) + 1] <- sum(predictors*sim.teff2@coef[k,])
  }
}

confint.teff2 <- apply(ytilde.teff2, 2, quantile, probs = c(0.025, 0.975))
table.teff2 <- data.frame(min(lalonde$age):max(lalonde$age), t(confint.teff2),
                          mean.educ, mean.re74, mean.re75)
names(table.teff2)[1] <- "Age"
names(table.teff2)[2] <- "2.5%"
names(table.teff2)[3] <- "97.5%"
names(table.teff2)[4] <- "Mean 'educ'"
names(table.teff2)[5] <- "Mean 're74'"
names(table.teff2)[6] <- "Mean 're75'"

tab_df(table.teff2)

plot(x = c(1:100), y = c(1:100), type = "n", 
     xlim = c(min(lalonde$age),max(lalonde$age)), 
     ylim = c(-5000,20000), 
     main = "re78 by age, predicted with all variables", xlab = "Age (years)", 
     ylab = "re78 ($)")

for (age_it in min(lalonde$age):max(lalonde$age)) {
  segments(
    x0 = age_it,
    y0 = confint.teff2[1, age_it - min(lalonde$age) + 1],
    x1 = age_it,
    y1 = confint.teff2[2, age_it - min(lalonde$age) + 1],
    lwd = 2)
}


## Item (d) ##


ytilde.teffsmed <- matrix(NA, nrow = n_its, ncol = length(
  min(lalonde$age):max(lalonde$age)))

median.educ <- median(lalonde$educ)
median.re74 <- median(lalonde$re74)
median.re75 <- median(lalonde$re75)

for(age_it in min(lalonde$age):max(lalonde$age)) {
  predictors <- c(1, age_it, age_it^2, median.educ, median.re74, median.re75,
                  mean(lalonde$treat), age_it*mean(lalonde$treat))
  for(k in 1:n_its) {
    ytilde.teffsmed[k, age_it - min(lalonde$age) + 1] <- sum(predictors*sim.teff2@coef[k,])
    + rnorm(1, mean = 0, sim.teff2@sigma[k])
  }
}

confint.teffsmed <- apply(ytilde.teffsmed, 2, quantile, probs = c(0.025, 0.975))
table.teffsmed <- data.frame(min(lalonde$age):max(lalonde$age), t(confint.teffsmed),
                             median.educ, median.re74, median.re75)
names(table.teffsmed)[1] <- "Age"
names(table.teffsmed)[2] <- "2.5%"
names(table.teffsmed)[3] <- "97.5%"
names(table.teffsmed)[4] <- "Median 'educ'"
names(table.teffsmed)[5] <- "Median 're74'"
names(table.teffsmed)[6] <- "Median 're75'"

tab_df(table.teffsmed)

plot(x = c(1:100), y = c(1:100), type = "n", 
     xlim = c(min(lalonde$age),max(lalonde$age)), 
     ylim = c(-5000,20000), 
     main = "re78 by age with predictors at medians and incorporated sigmas", xlab = "Age (years)", 
     ylab = "re78 ($)")

for (age_it in min(lalonde$age):max(lalonde$age)) {
  segments(
    x0 = age_it,
    y0 = confint.teffsmed[1, age_it - min(lalonde$age) + 1],
    x1 = age_it,
    y1 = confint.teffsmed[2, age_it - min(lalonde$age) + 1],
    lwd = 2)
}

###############
## Problem 3 ##
###############

library(boot)
foo_three <- read.csv(url("https://tinyurl.com/y2prc9xq"))
foo_three <- na.omit(foo_three)
reg_math <- lm(MATH_SCORE ~ TREATMENT, data = foo_three)
summary(reg_math)

# Declare bootstrapping function

storvec <- rep(NA, n_its)

for (i in 1:n_its) {
  looplm = lm(MATH_SCORE ~ TREATMENT, data = foo_three[sample(1:nrow(foo_three), nrow(foo_three), replace = T),])
  storvec[i] <- looplm$coefficients[2]
}

# Find simulated confidence intervals and compare with regression standard error
quantile(storvec, c(0.025, 0.975))
confint(reg_math)[2,]
formattable(data.frame(simulation = quantile(storvec, c(0.025, 0.975)), analytical = confint(reg_math)[2,]), format = "pandoc")

# Item (b)
hist(storvec, main = "Group-assignment coefficient frequency", xlab = "Treatment Coefficient", ylab = "Count", col = "blue")

###############
## Problem 4 ##
###############

# R-squared function #
rsq <- function (predy, y){
  rss <- sum((y-predy)**2)
  tss <- sum((y-mean(y))**2)
  return(1-(rss/tss))
}

# Bootstrapping R-squared values #
len_result <- length(foo_three$MATH_SCORE)
storage_vec <- numeric(n_its)
for (i in 1:n_its) {
  boot.sample <-  sample(x = 1:len_result, size = len_result, replace = TRUE) ## Sample indices
  boot.data <- na.omit(foo_three[boot.sample,])
  storage_vec[i] <- rsq(predict(reg_math, newdata = boot.data), boot.data$MATH_SCORE)
}
mean(storage_vec)

# Working example #

predys <- predict(reg_math, newdata = foo_three)
ys <- na.omit(foo_three$MATH_SCORE)

rsq(predys, ys)

summary(reg_math)$r.sq

###############
## Problem 5 ##
###############

foo_five <- read.csv(url("https://tinyurl.com/yx8tqf3k"))

set.seed(6789)
test_set_rows <- sample(1:length(foo_five$age), 2000, replace = FALSE)
prfive.test <- foo_five[test_set_rows,]

prfive.train <- foo_five[-test_set_rows,]

# Model #1
model_one <- glm(treat ~ .*. - re78 - re78*., data = prfive.train)

# Model #2
model_two <- glm(treat ~ . - re78, data = prfive.train)

# Model #3
model_three <- glm(treat ~ re74 + re75 + re74*re75, data = prfive.train)

# Model #4
model_four <- glm(treat ~ age + education + black + hispanic + 
                   married + nodegree, data = prfive.train)

# Model #5
model_five <- glm(treat ~ . -education - re78, data = prfive.train)

## LOOCV and test MSE comparison ##

# Model #1
cv.mse1 <- cv.glm(prfive.train, model_one)$delta[1]
cv.mse1

testerr1 <- mean((prfive.test$treat - predict.lm(model_one, prfive.test))^2)
testerr1

# Model #2
cv.mse2 <- cv.glm(prfive.train, model_two)$delta[1]
cv.mse2

testerr2 <- mean((prfive.test$treat - predict.lm(model_two, prfive.test))^2)
testerr2

# Model #3
cv.mse3 <- cv.glm(prfive.train, model_three)$delta[1]
cv.mse3

testerr3 <- mean((prfive.test$treat - predict.lm(model_three, prfive.test))^2)
testerr3

# Model #4
cv.mse4 <- cv.glm(prfive.train, model_four)$delta[1]
cv.mse4

testerr4 <- mean((prfive.test$treat - predict.lm(model_four, prfive.test))^2)
testerr4

# Model #5
cv.mse5 <- cv.glm(prfive.train, model_five)$delta[1]
cv.mse5

testerr5 <- mean((prfive.test$treat - predict.lm(model_five, prfive.test))^2)
testerr5

loocv.model_err <- c(cv.mse1, cv.mse2, cv.mse3, cv.mse4, cv.mse5)
loocv.model_err[which.min(loocv.model_err)]

test.errc <- c(testerr1, testerr2, testerr3, testerr4, testerr5)
test.errc[which.min(test.errc)]

errorcol <- cbind(loocv.model_err, test.errc)
table.error <- data.frame(1:5, errorcol)
names(table.error)[1] <- "Model number"
names(table.error)[2] <- "LOOCV MSE"
names(table.error)[3] <- "Model test error"

formattable(table.error)

####################################################################
####                         Problem 5                          ####
#### Experimental function for finding missclassification rates ####
####################################################################

n.folds <- length(prfive.train$treat)
folds <- sample(1:n.folds, size = n.folds, replace = T)

loocv.missclass <- function(logitmodel) {
  confustion.tables <- list()
  misclassfrac <- numeric(n.folds)
  misclassrate <- function(x) { (sum(x) - sum(diag(x)))/sum(x) }
  for (i in 1:n.folds) {
    # Create training set
    train <- filter(prfive.train, folds != i)  # Take all other samples than i
    
    # Create test set
    test <- filter(prfive.train, folds == i)
    
    # Fit the glm model on the training set
    glm.train <- glm(logitmodel, data = train, family = binomial)
    
    # Use the fitted model on the test set
    logit.prob <- predict(glm.train, newdata = test)
    
    # Classifiy results
    pred.class <- ifelse(logit.prob < 0, 0, 1)
    
    # Construct the confusion table
    conf_tables[[i]] <- table(pred = pred.class, true = test$treat)
    misrate <- sapply(conf_tables, misclassrate)
    misclassfrac[i] <- misrate
  }
  return(mean(riskvec))
}