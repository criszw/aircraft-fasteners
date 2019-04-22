#Create a model that predicts the probability that a fastener passes based on load.

# download data
Aircraft.fasteners <- read.csv("Aircraftfasteners.csv", header= TRUE)
attach(Aircraft.fasteners)


#explore data
source("summary.info.R")
summary.info(Aircraft.fasteners)

npass<-ntotal-nfail
perc.pass<- npass/ntotal

#fit model
model<- glm(cbind(npass,nfail)~load,data=Aircraft.fasteners, family=binomial)
summary(model)

simulated.load<- seq(from=2500, to =4300, by=100)

plot (perc.pass~load,col="green",data=Aircraft.fasteners,
      xlab="Pressure Load (psi)", ylab="Percentage of Aircraft Fastners Passing",
      pch=19)
lines(simulated.load,ilogit(coef(model)[1] + coef(model)[2]*simulated.load),
      col="black",lwd=3,lty=1)

x=seq(from=0, to=1, by =0.05)
odds<-x/(1-x)

loads<-(log(odds)-coef(model)[1])/coef(model)[2]
cbind(x, ceiling(loads))


