# 17102021
# By Phuong Thao
# Book: Benchmarking with DEA, SFA, and Rlibrary(Benchmarking)
library(ggplot2)
library(Benchmarking)
library(frontier)
data("charnes1981")

# parametric approach
x = charnes1981$x1
y = charnes1981$y1
ols <- lm(log(y) ~ log(x))
ols

max(residuals(ols))
coef(ols)[1] + max(residuals(ols))

plot(log(x),log(y))
abline(coef(ols), lty="dashed")
abline(coef(ols)[1] + max(residuals(ols)), coef(ols)[2])
hist(exp(residuals(ols)) - max(residuals(ols)), main=NULL)
ggplot(ols, aes(x=(exp(residuals(ols)) - max(residuals(ols)))))+
  geom_histogram()+
  xlim(c(0,1))

# sfa
msfa <- sfa(matrix(log(x)), matrix(log(y)))
msfa

# Practical application: Milk producers
data("milkProd")

x <- with(milkProd, cbind(vet, energy, cows))
y <- matrix(milkProd$milk)
milkSfa <- Benchmarking::sfa(log(x), log(y))
summary(milkSfa)

e <- residuals(milkSfa)
s2 <- sigma2.sfa(milkSfa)
lambda <- lambda.sfa(milkSfa)


mustar <- (-e*lambda^2)/(1+lambda^2)
sstar <- lambda/(1+lambda^2)*sqrt(s2)
teJ <- exp(-mustar
             -sstar*( dnorm(mustar/sstar)/pnorm(mustar/sstar)))

# frontier package
front <- frontier::sfa(log(milkProd$milk) ~ log(milkProd$vet) + log(milkProd$energy) + log(milkProd$cows))
summary(front, extraPar = T)

lmodel <- lm(data = milkProd, log(milk) ~ log(vet) + log(energy) + log(cows))
summary(lmodel)

lrtest(front)

frontier::efficiencies(front)
efficiencies(milkSfa)

