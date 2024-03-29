dir<-getwd()
setwd(dir)
data <-read.table("gdpB400_BC10")
gdp <- data$V2
data <-read.table("indegreeCB400_BC10")
indegreeC <- data$V2
data <-read.table("indegreeIB400_BC10")
indegreeI <- data$V2
l_gdp <- log(gdp)
l_indegreeC <- log(indegreeC)
l_indegreeI <- log(indegreeI)

library(mFilter)
l_gdp.hp <- hpfilter(l_gdp,freq=1600,type="lambda")
noise <- l_gdp.hp$cycle
nrf <- length(l_gdp)
alpha <- mat.or.vec(nrf,1)
beta <- mat.or.vec(nrf,1)
a <- seq(-4,4,by=0.01)
b <- seq(-5,5,by=0.01)
zeta <- mat.or.vec(nrf,nrf)
zeta.hat <- mat.or.vec(nrf,1)
for(t in 1:nrf){
	zeta.min<-100
	for(foo in 1:length(a)){
		for(iii in 1:length(b)){
			zeta[foo,iii] <- a[foo]*l_indegreeC[t] + b[iii]*l_indegreeI[t] + noise[t]
			if(zeta[foo,iii] < zeta.min && zeta[foo,iii] > 0){
				zeta.min <- zeta[foo,iii]
				p1 <- a[foo]
				p2 <- b[iii]
				p3 <- foo
				p4 <-iii	
			}
		}
	}	
	test <- (zeta.min - l_gdp[t])/l_gdp[t]
		for(foo in p3:length(a)){
			for(iii in 1:length(b)){
				if(abs(test) > 0.01){
					zeta.min <- zeta[foo,iii]
					x1 <- a[foo]
					x2 <- b[iii]
					x3 <- foo
					x4 <- iii
					test <- ( zeta.min - l_gdp[t])/l_gdp[t]
					#print(abs(test))
				}
			}
	}
	zeta.hat[t] <- zeta.min
	alpha[t] <- x1
	beta[t] <- x2
}
save.image("alg2.RData")
write(t(alpha), file="alphaB400_B10.txt", ncolumns=1)
write(t(beta), file="betaB400_B10.txt", ncolumns=1)
write(t(zeta.hat), file="zeta_hatB400_B10.txt", ncolumns=1)
write(t(l_gdp), file="l_gdpB400_B10.txt", ncolumns=1)
