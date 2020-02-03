options(scipen = 999)
date <- seq(as.Date("2020/01/21"), by = "day", length.out = 13)
data <- data.frame(time = c(1:13),
                   case = c(278, 309, 571, 830, 1297, 1958, 2741, 4537, 6065, 7736, 9720, 11821, 14411))

exponential.model <- lm(log(data$case)~data$time)
summary(exponential.model)


timevalues <- c(1:13)
Counts.exponential2 <- exp(predict(exponential.model, list(time=timevalues)))
plot(data$time, data$case, pch=16, las =1)
lines(timevalues, Counts.exponential2, lwd=2, col = "red", xlab = "Time (s)", ylab = "Counts")

gr1 <- exponential.model$coefficients[1]
gr2 <- exponential.model$coefficients[2]


time <- c(1:28)
case <- exp(gr2*time)
par(oma=c(0,2,3,0))
plot(time, case, type="l", las=1, lwd=2, xlab = "Time after disease start transmission (day)", ylab = "", col="blue")
case <- exp(gr2*1.2*time)
lines(time, case, type="l", las=1, col="blue", lty=2)
case <- exp(gr2*0.8*time)
lines(time, case, type="l", las=1, col="blue", lty=2)
abline(h=10000, lty=3, col="grey20")
abline(h=1000, lty=3, col="grey20")

axis(3, 1:13, date)
lines(timevalues, Counts.exponential2, lwd=2, col = "red")
points(data$time, data$case, pch=16, las=1)
mtext("2019-nCoV data fitting and prediction", side=3, line=0, cex=2, outer=TRUE) 
mtext("Total confirmed cases", side=2, line=0, outer=TRUE) 






###
library(tidyverse)
library(dplyr)
library(brms)

prior <- prior(normal(exp(5.3), 20), nlpar = "b0") + prior(normal(0.35, 0.035), nlpar = "b1")
fit <- brm(bf(case ~ b0 * exp(b1 * time), b0 + b1 ~ 1, nl = TRUE), data = data, prior = prior)

summary(fit)

plot(fit)
plot(conditional_effects(fit), points = TRUE)

as.data.frame(fit)$b_b0_Intercept

b1 <- as.data.frame(fit)$b_b1_Intercept

time <- c(1:50)
y <- exp(b1[1]*time)
plot(time, y, log = "y", type="l", col=scales::alpha(rgb(0,0,0), 0.1))
for(i in 2:100){
  y <- exp(b1[i]*time)
  lines(time, y, col=scales::alpha(rgb(0,0,0), 0.1))
}

abline(h=1000, lty=3)
abline(h=10000, lty=3)
abline(h=100000, lty=3)
abline(h=1000000, lty=3)

prior <- prior(normal(exp(5.3), 20), nlpar = "b0") + prior(normal(0.35, 0.035), nlpar = "b1")
fit <- brm(bf(case ~ b0 * exp(b1 * time), b0 + b1 ~ 1, nl = TRUE), data = data, prior = prior)
