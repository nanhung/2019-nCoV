#http://epirecip.es/epicookbook/chapters/seir/r_desolve
library(deSolve)

# Function to return derivatives of SEIR model
seir_ode<-function(t,Y,par){
  S<-Y[1]
  E<-Y[2]
  I<-Y[3]
  R<-Y[4]
  
  beta<-par[1]
  sigma<-par[2]
  gamma<-par[3]
  mu<-par[4]
  
  dYdt<-vector(length=3)
  dYdt[1]=mu-beta*I*S-mu*S
  dYdt[2]=beta*I*S-(sigma+mu)*E
  dYdt[3]=sigma*E-(gamma+mu)*I
  
  return(list(dYdt))
}

# Set parameter values
beta<-0.8;
sigma<-0.2;
gamma<-0.16;
mu<-0 # UK birth and population figures 2016
init<-c(100,0,1)
t<-seq(0,100)
par<-c(beta,sigma,gamma,mu)
# Solve system using lsoda
sol<-lsoda(init,t,seir_ode,par)

# Plot solution
plot(t,sol[,2],type="l",col="blue",ylab="Proportion")
lines(t,sol[,3],col="orange")
lines(t,sol[,4],col="red")  
lines(t,100-rowSums(sol[,2:4]),col="green")
legend(300,0.7,legend=c("S","E","I","R"),col=c("blue","orange","red","green"), lty=1, cex=0.8)