# College-Basketball-Data-Analysis
Multiple Linear Regression Analysis Stat 632

# Download Dataset
library(tidyverse)
cbb <- read_csv("Downloads/cbb19.csv", col_types = cols(TEAM = col_skip(), CONF = col_skip(),  G = col_skip(), WAB = col_skip(), POSTSEASON = col_skip(), SEED = col_skip()))

# Model Selection
mod.0<-lm(W~1,data=cbb)
mod.full<-lm(W~.,data=cbb)
n<-length(cbb$W)
# Backward BIC
step(mod.full, scope = list(lower = mod.0, upper = mod.full), direction = 'backward',k = log(n), trace = 0)
fit1<-step(mod.full, scope = list(lower = mod.0, upper = mod.full), direction = 'backward',k = log(n), trace = 0)
#Backward AIC
step(mod.full, scope = list(lower = mod.0, upper = mod.full), direction = 'backward',trace = 0)
fitAIC<-step(mod.full, scope = list(lower = mod.0, upper = mod.full), direction = 'backward',trace = 0)
# Stepwise Selection
step(mod.0, scope = list(lower = mod.0, upper = mod.full),trace=0)
fitstep<-step(mod.0, scope = list(lower = mod.0, upper = mod.full),trace=0)
# Comparison of three models
s1<-summary(fit1)
s2<-summary(fitAIC)
s3<-summary(fitstep)
c(s1$adj.r.squared, s2$adj.r.squared, s3$adj.r.squared)
#Adjusted R-squared is similar, thus I choose 9 predictor Backward BIC Model

# Histogram for all relevant Variables
library(car)
attach(cbb)
par(mfrow=c(3,4))
hist(EFG_D, col="deepskyblue1")
hist(TOR, col="red")
hist(TORD, col="chartreuse1")
hist(ORB, col="darkorange1")
hist(DRB, col="blueviolet")
hist(FTR, col="deeppink1")
hist(FTRD, col="gold2")
hist(`2P_O`, col="paleturquoise")
hist(`3P_O`,col="magenta")
hist(W, col="yellow")
detach(cbb)

# Explorotory Analysis
pairs(W ~ EFG_D + TOR + TORD + ORB + DRB + FTR + FTRD + `2P_O` + `3P_O`, data = cbb)
round(vif(fit1), 2) 
avPlots(fit1)

# Diagnostics
par(mfrow=c(2,2))
plot(fit1,1:2) # Constant variance assumption not met, slight U-shaped
shapiro.test(rstandard(fit1))
influenceIndexPlot(fit1)
#Influential Points
p<-9
cbb.cooks <- cooks.distance(fit1)
which(cbb.cooks > 4/(n - p - 1))
# High Leverage Points
p<-9
n<-nrow(cbb)
cbb.hats <- hatvalues(fit1)
sum(cbb.hats)
which(cbb.hats > 2*(p+1)/n)
which(cbb.hats > 3*(p+1)/n)
# Outliers
cbb.std <- rstandard(fit1)
which(abs(cbb.std) > 2)
which(abs(cbb.std) > 3)
# Bad Leverage Points
plot(hatvalues(fit1), rstandard(fit1),xlab='Leverage', ylab='Standardized Residuals')
abline(v = 2*(p+1)/n, lty = 2, lwd = 2, col = "red")
abline(h = c(-2, 2), lty = 2, lwd = 2, col = "blue")
inds <- which(hatvalues(fit1) > 2*(p+1)/n & abs(rstandard(fit1)) >2)
inds
# Since No bad Leverage Points We move on to Transformation

# Transformation
attach(cbb)
pt<-powerTransform(cbind(EFG_D, TOR, TORD, ORB, DRB, FTR, FTRD, `2P_O`, `3P_O`)~1)
summary(pt)
# Refit new model with TOR TORD FTRD as log transformed and check if response needs to be transformed
fit3<-lm(W ~ EFG_D + log(TOR) + log(TORD) + ORB + DRB + FTR + log(FTRD) + `2P_O` + `3P_O`, data = cbb)
pt2 <- powerTransform(fit3)
summary(pt2)

# Recheck Diagnostics after fitting transformed predictors and square root transformed response 
fit4<-lm(sqrt(W) ~ EFG_D + log(TOR) + log(TORD) + ORB + DRB + FTR + log(FTRD) + `2P_O` + `3P_O`, data = cbb)
par(mfrow=c(2,2))
plot(fit4,1:2) # constant variance is better but tails off at the end
shapiro.test(rstandard(fit4))
influenceIndexPlot(fit4)
cbb[1,] #Highly influential point 
cbb[233,] #Highly influential point 
#Remove Highly Influential points
detach(cbb)
cbb2 <- cbb[-c(1,233), ] # remove highly influential points
fit5<-lm(sqrt(W) ~ EFG_D + log(TOR) + log(TORD) + ORB + DRB + FTR + log(FTRD) + `2P_O` + `3P_O`, data = cbb2) #Final Model
par(mfrow=c(2,2))
plot(fit5,1:2) #Constant varaince and normality met 
shapiro.test(rstandard(fit5))

# Research Question 1: Does defense affect winning games
red.lm<-lm(sqrt(W)~log(TOR)+ORB+FTR+`2P_O`+`3P_O`, data=cbb2)
anova(red.lm,fit5)

# Research Question 2: How do Two-Point and Three-Point Shooting affect Winning games 
summary(fit5)

# Research Question 3: Does Offense or Defense Explain more of the variability in winning games
offense.lm<-lm(sqrt(W)~log(TOR)+ORB+FTR+`2P_O`+`3P_O`, data=cbb2)
defense.lm<-lm(W~log(TORD)+DRB+log(FTRD)+EFG_D,data=cbb2)
par(mfrow=c(2,2))
plot(offense.lm,1:2)
shapiro.test(rstandard(offense.lm))
par(mfrow=c(2,2))
plot(defense.lm,1:2)
shapiro.test(rstandard(defense.lm))
s4<-summary(offense.lm)
s5<-summary(defense.lm)
c(s4$adj.r.squared, s5$adj.r.squared)
