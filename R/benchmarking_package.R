# 17102021
# By Phuong Thao
# Book: Benchmarking with DEA, SFA, and Rlibrary(Benchmarking)
library(ggplot2)
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

