###Christopher VanDemark 
#Prostate-Sepcific Antigen Coding

###Variable Creation for Environment 

PSA <- Cancer$psa
CancerV <- Cancer$cancerv
weight <- Cancer$weight
Age <- Cancer$age
Hyperplasia <- Cancer$hyperplasia
Seminal <- Cancer$seminal
Capsular <- Cancer$capsular
GleasonScore <- Cancer$score

###General Linear Model (Complete)

completemodel <- lm(PSA ~ CancerV + weight + age + Hyperplasia + Seminal + Capsular +GleasonScore)

###Plotting initial variable graphs 

plot(PSA ~ weight)
abline(lm(PSA ~ weight))
abline(lm(PSA ~ age))
plot(PSA ~ age)
abline(lm(PSA ~ age))
plot(PSA ~ Hyperplasia)
abline(lm(PSA ~ Hyperplasia))
plot(PSA ~ Seminal)
abline(lm(PSA ~ Seminal))
plot(PSA ~ Seminal)
plot(PSA ~ Capsular)
abline(lm(PSA ~ Capsular))
plot(PSA ~ GleasonScore)

###Boxplots

boxplot(PSA, xlab = "PSA Boxplot")
boxplot(CancerV, xlab = "Prostate Cancer Volume Boxplot")
boxplot(weight, xlab = "Weight Boxplot")
boxplot(age, xlab = "Age Boxplot")
boxplot(Hyperplasia, xlab = "Hyperplasia Boxplot")
boxplot(Capsular, xlab = "Capsular Boxplot")

###Correlation Matrix 

cor(Cancer)

###Plotting Correlation Matrix

install.packages("corrplot")
library(corrplot)
PearsonCorMat <- cor(Cancer, method = "pearson")
corrplot(PearsonCorMat, method = "circle")
SpearmanCorMat <- cor(Cancer, method = "spearman")
corrplot(SpearmanCorMat, method = "circle")

###Variation Inflation Factor

install.packages("car")
library(car)
vif(completemodel)

###Automatic Model Selection Methods

install.packages("LEAP")
library(LEAP)
completemodel1 <- regsubsets(completemodel, data = Cancer)
summary(completemodel1)
sum$which
sum$cp
sum$bic

###Reduced Model with Summary 

finalmodel <- lm(PSA ~ CancerV + Seminal)
summary(finalmodel)

###ttest statistic

install.packages("lmtest")
library(lmtest)
tstat <- coeftest(finalmodel)
tstat

###Checking normality/Variance of Residuals

library(car)
residuals <- finalmodel$residuals
qqPlot(residuals)
plot(residuals)


