# https://www.medrxiv.org/content/10.1101/2020.02.09.20021261v1
# Parameter searching
n <- 40000
x<-matrix(nrow=n,ncol=2)
set.seed(2019)
for(i in 1:n){
  scale <- runif(1, 2, 5)
  shape <- runif(1, 0.9, 1.4)
  sims <- rweibull(n, shape=shape, scale = scale)
  
  if(max(sims)>24 & max(sims)<25 & median(sims)>2 & median(sims)<3) x[i,] <- c(scale, shape)
}

X <- x[complete.cases(x),]
nrow(X)
hist(X[,1])
hist(X[,2])

# Monte Carlo Simulation
scale <- X[1,1]
shape <- X[1,2]

n<-100000
sim <- rweibull(n, shape=shape, scale = scale)
max(sim)
median(sim)

d <- seq(0, 28, 0.1)
p <- pweibull(d, shape=shape, scale = scale)
p14 <- (1 - p[141]) * 1000
p24 <- (1 - p[241]) * 1000

plot(d,p, type="l", col="grey", xlab="Incubation time (day)", ylab = "CDF")

for(i in 2:nrow(X)){
  scale <- X[i,1]
  shape <- X[i,2]
  p <- pweibull(d, shape=shape, scale = scale)
  lines(d, p, type="l", col="grey")
  P14 <- c(P14, (1 - p[141]) * 1000)
}
abline(v=14, lty=3, lwd=2, col="red")

par(new=TRUE, oma=c(7,1,0,2), mar=c(4,4,2,0))
layout(matrix(1:4,2))
hist(P14, main = "Incubation time over 14 days", 
     xlab = "per 1000 population",col=rgb(1,0,0,0.2))

quantile(P14, c(0.025,0.975))
boxplot(P14, horizontal = T)
