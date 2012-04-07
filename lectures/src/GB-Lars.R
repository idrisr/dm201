# TODO: Add comment
# 
# Author: mike-bowles
###############################################################################


# setwd("/home/mike-bowles/Documents/StatisticsPapers/ESL/DataSets/Prostate")
setwd('/home/id/learning/dm201/lectures/data/')
pdata <- read.table(file="prostate", header = TRUE, row.names=1)
#put predictors into X
X <- pdata[,1:8]
#put response into Y
Y <- pdata[,9]
#pull out indices which authors used as training set
Itrain <- which(pdata[,10])
#demean X and bring to unit variance
for(i in 1:8){
	m <- sum(X[,i])
	m <- m/length(X[,i])
	X[,i] <- X[,i]-m
	v <- var(X[,i])
	X[,i] <- X[,i]/sqrt(v)
}
#put X into matrix form
X <- as.matrix(X)
m <- sum(Y)/length(Y)
Y<- Y - m

alpha <- rep(0.0,8)
alphaHist <- alpha
residual <- Y
yHat <- rep(0.0,length(Y))

eps <- 0.01
small <- eps*1.5
countMax <- 2000
count <- 0
repeat{
	ssResidual <- sum(residual*residual)
	bestResidual <- ssResidual
	for(i in 1:8){
		betaTemp <- cov(X[,i],residual)
		residTemp <- residual - betaTemp*X[,i]
		ssResidTemp <- sum(residTemp*residTemp)
		if(ssResidTemp < bestResidual) {
			bestResidual = ssResidTemp
			iBest <- i
			betaStar <- betaTemp
		}	
		
	}
	if(abs(betaStar) < small) {break}
	count <- count +1
	if(count == countMax) {break}
	alpha[iBest] <- alpha[iBest] + eps*betaStar/abs(betaStar)
	alphaHist <- rbind(alphaHist, alpha)
	yHat <- X%*%alpha
	residual <- Y-yHat
}


plot(alphaHist[,1],type="l",pch=".")
for(i in 2:8){
	points(alphaHist[,i],type="l",pch=".")
}
