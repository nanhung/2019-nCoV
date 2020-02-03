#http://epirecip.es/epicookbook/chapters/seir/r_desolve
library(deSolve)

# Function to return derivatives of SEIR model
seir_ode<-function(t,Y,par){
  S<-Y[1]
  E<-Y[2]
  I<-Y[3]
  J<-Y[4]
  R<-Y[5]
  N<-S+E+I+J+R
  
  beta<-par[1]  # Transmission rate
  sigma<-par[2] # 1 / Incubation time 
  alpha<-par[3] # 1 / Sympton onset to isolation
  gamma<-par[4] # 1 / Isolation to recovery
  mu<-par[5]    # Death rate
  
  dYdt<-vector(length=3)
  dYdt[1]=-beta*I*S/N
  dYdt[2]=beta*I*S/N-sigma*E
  dYdt[3]=sigma*E-alpha*I
  dYdt[4]=alpha*I-(gamma+mu)*J
  dYdt[5]=gamma*J
  
  return(list(dYdt))
}

# Set parameter values
R0 <- 2 # # (range 2-3)
sigma<-1/5 # (range 1-9)
alpha<-1/6 # (range 2-10)
beta<-R0 * alpha  
gamma<-1/6 # (no information to set uncertainty)
mu<-0.02 #(range 0.017-0.024)

init<-c(100,0,1,0,0)
t<-seq(0,100)
par<-c(beta,sigma,alpha,gamma,mu)
# Solve system using lsoda
sol<-lsoda(init,t,seir_ode,par)

# Plot solution
plot(t,sol[,2],type="l",col="blue",ylab="Proportion", ylim=c(0,100))
lines(t,sol[,3],col="orange")
lines(t,sol[,4],col="red")  
lines(t,100-rowSums(sol[,2:4]),col="green")
legend(300,0.7,legend=c("S","E","I","R"),col=c("blue","orange","red","green"), lty=1, cex=0.8)

#
source("MCSim/function.R")
# makemod()
model <- "SEIR.model.R"
input <- "SEIR.MTC.in.R"
#makemcsim(model)
df <- mcsim(model, input)

str <- 5
t<-seq(0,100)
ylim <- range(df[,str:ncol(df)])

for(i in 1:500){
  max.d <- which(df[i,str:ncol(df)]==max(df[i,str:ncol(df)]))
  if(i == 1) max.ds <- max.d else max.ds <- c(max.ds, max.d)
}

layout(matrix(c(1,2,2)), heights=c(1,2))
par(mar=c(0,4,0,0))
hist(max.ds, axes = F, main="", ylab="")
par(mar=c(4,4,0,0))
plot(t, df[1,str:ncol(df)], type="l", 
     ylim=ylim, 
     col=scales::alpha(rgb(0,0,0), 0.2))
for(i in 2:500){
  lines(t, df[i,str:ncol(df)], col=scales::alpha(rgb(0,0,0), 0.2))
}


dev.off()
par(mar=c(4,4,0,0))
plot(t, df[1,str:ncol(df)], type="l", 
     ylim=ylim, 
     #log="y",
     col=scales::alpha(rgb(0,0,0), 0.2))
for(i in 2:500){
  lines(t, df[i,str:ncol(df)], col=scales::alpha(rgb(0,0,0), 0.2))
}

y <- exp(0.3*t)
lines(t, y, col="red")

