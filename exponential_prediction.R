options(scipen = 999)
date <- seq(as.Date("2020/01/21"), by = "day", length.out = 10)
data <- data.frame(time = c(1:10),
                   case = c(278, 309, 571, 830, 1297, 1958, 2741, 4537, 6065, 7736))

exponential.model <- lm(log(data$case)~data$time)
exponential.model

timevalues <- c(1:10)
Counts.exponential2 <- exp(predict(exponential.model, list(time=timevalues)))
plot(data$time, data$case, pch=16, las =1)
lines(timevalues, Counts.exponential2, lwd=2, col = "red", xlab = "Time (s)", ylab = "Counts")

gr1 <- exponential.model$coefficients[1]
gr2 <- exponential.model$coefficients[2]


time <- c(1:24)
case <- exp(gr2*time)
par(oma=c(0,2,3,0))
plot(time, case, type="l", las=1, lwd=2, xlab = "Time after disease start transmission (day)", ylab = "", col="blue")
case <- exp(gr2*1.1*time)
lines(time, case, type="l", las=1, col="blue", lty=2)
case <- exp(gr2*0.9*time)
lines(time, case, type="l", las=1, col="blue", lty=2)
abline(h=10000, lty=3, col="grey20")
abline(h=1000, lty=3, col="grey20")

axis(3, 1:10, date)
lines(timevalues, Counts.exponential2, lwd=2, col = "red")
points(data$time, data$case, pch=16, las=1)
mtext("2019-nCoV data fitting and prediction", side=3, line=0, cex=2, outer=TRUE) 
mtext("Total confirmed cases", side=2, line=0, outer=TRUE) 