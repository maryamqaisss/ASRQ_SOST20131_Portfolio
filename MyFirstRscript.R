install.packages("wooldridge")
library(wooldridge)
data("wage1")
dim(wage1)
head(wage1)
mean(wage1$wage)
median(wage1$wage)
sd(wage1$wage)
min(wage1$wage)
max(wage1$wage)
install.packages("knitr")
library(knitr)
sumstats <- t(sapply(wage1[c("wage", "educ", "exper")], function(x)
  c(Mean = mean(x), Median = median(x),
    SD = sd(x), Min = min(x), Max = max(x))))
kable(sumstats, digits = 2)
mean(wage1$female)
hist(wage1$wage)
hist(wage1$wage, breaks = 30,
     main = "", col = "grey80",
     border = "white",
     xlab = "Hourly wage (dollars)")
abline(v = mean(wage1$wage), lty = 2, lwd = 2)
abline(v = median(wage1$wage), lty = 1, lwd = 2)
legend("topright",
       legend = c("Mean", "Median"),
       lty = c(2,1),
       lwd = 2, bty = "n")
hist(wage1$lwage, breaks = 30,
     main = "", col = "grey80",
     border = "white",
     xlab = "Log hourly wage")
boxplot(wage ~ factor(female, labels = c("Male", "Female")),
        data = wage1,
        xlab = "",
        ylab = "Hourly wage (dollars)",
        col = "grey85")
med <- tapply(wage1$wage, wage1$female, median)
iqr <- tapply(wage1$wage, wage1$female, IQR)
rbind(Median = med, IQR = iqr)
plot(wage1$educ, wage1$wage,
     pch = 16,
     col = rgb(0,0,0,0.35),
     xlab = "Years of education",
     ylab = "Hourly wage (dollars)")
cor(wage1$wage, wage1$educ)
reg1 <- lm(wage ~ educ, data = wage1)
coef(reg1)
abline(reg1, col = "firebrick", lwd = 2)
pred <- predict(reg1, newdata = data.frame(educ = c(12, 16)))
pred
plot(wage1$exper, wage1$wage,
     pch = 16,
     col = rgb(0, 0, 0, 0.12),
     xlab = "Years of potential experience",
     ylab = "Hourly wage (dollars)")
cor(wage1$wage, wage1$exper)
reg2 <- lm(wage ~ exper, data = wage1)
coef(reg2)
abline(reg2, col = "firebrick", lwd = 2)
lines(lowess(wage1$exper, wage1$wage),
      lty = 2, lwd = 3)
legend("topright",
       legend = c("OLS line", "Lowess curve"),
       col = c("firebrick", "black"),
       lty = c(1,2),
       lwd = c(2,3), 
       bty = "n")
b1 <- cov(wage1$educ, wage1$wage) / var(wage1$educ)
b1
b0 <- mean(wage1$wage) - b1 *mean(wage1$educ)
c(b0 = b0, b1 = b1)
all.equal(unname(coef(reg1)), c(b0, b1))
res1 <- resid(reg1)
head(res1)
sum(res1)
plot(wage1$educ, res1,
     pch = 16,
     col = rgb(0, 0, 0, 0.35),
     xlab = "Years of education",
     ylab = "Residual")
abline(h = 0, lty = 2, lwd = 2)

