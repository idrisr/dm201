###############################################################################
# Author: mike-bowles
# Edited: idris-raja
###############################################################################

setwd('/home/id/learning/dm201/lectures/data/')

data <- read.table(file="mixtureSimData.data")

train <- data.frame(X1 = data[1:200, ], X2 = data[201:400, ], 
                    Y = rep(c(0, 1), each=100))
require(ggplot2)
g <- ggplot(train, aes(X1, X2)) + geom_point(aes(colour=Y)) +
        opts(legend.position="none")

# create grid of potential points
minX1 <- min(train$X1)
minX2 <- min(train$X2)
maxX1 <- max(train$X1)
maxX2 <- max(train$X2)

testMat <- matrix(0.0, 10000, 2)
for(i in 1:100){
	for(j in 1:100){
		x1 <- minX1 + i*(maxX1 - minX1)/100
		x2 <- minX2 + j*(maxX2 - minX2)/100
		index <- (i-1)*100 + j
		testMat[index,1] <- x1
		testMat[index,2] <- x2
	}	
}
test <- data.frame(testMat)

require(class)
knnplot <- function(train, test, classifier, k) {
    KNN <- knn(train, test, classifier, k)
    test$predict <- KNN
    test$z <- c(0, 1)[sapply(test$predict, as.numeric)]
    title = paste('k=', as.character(k), sep='')
    # TODO: get the original points back in the graph
    g <- ggplot(test, aes(X1, X2, z=z)) + 
            geom_point(aes(colour = predict), size=1) + 
            geom_contour(colour='black', size = 0.1) + 
            opts(legend.position="none") + opts(title=title)
    return(g)
}

setwd('/home/id/learning/dm201/lectures/plots')
for(k in 1:dim(train)[1]){
# for(k in 1:10){
    plot <- knnplot(train[, c('X1', 'X2')], test, train$Y, k)
    name = paste(as.character(k), '.png', sep='')
    ggsave(filename=name, plot=plot, height=5, width=5)
}
# make movie
# ffmpeg -f image2 -f 1 -i %d.png -b 800k knn.mp4
