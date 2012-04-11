###############################################################################
# Author: mike-bowles
# Edited: idris-raja, idris.raja@gmail.com
###############################################################################

setwd('/home/id/learning/dm201/lectures/')
data <- read.table(file="data/mixtureSimData.data")
train <- data.frame(X1 = data[1:200, ], X2 = data[201:400, ], 
                    Y = rep(c(0, 1), each=100))
require(ggplot2)
g <- ggplot(train, aes(X1, X2)) + geom_point(aes(colour=Y)) +
        opts(legend.position="none")
ggsave(filename='plots/orig.png', plot=g, height=5, width=5)

# create grid of test points
minX1 <- min(train$X1)
minX2 <- min(train$X2)
maxX1 <- max(train$X1)
maxX2 <- max(train$X2)
X1.range <- seq(from=minX1, to=maxX1, length.out=100)
X2.range <- seq(from=minX2, to=maxX2, length.out=100)
test <- data.frame(X1 = rep(X1.range, 100), X2 = rep(X2.range, each=100))

require(class)
knnplot <- function(train, test, k) {
    KNN <- knn(train[, c('X1', 'X2')], test, train$Y, k)
    test$predict <- KNN

    # change factor to numeric
    test$z <- c(0, 1)[sapply(test$predict, as.numeric)]

    title = paste('k=', as.character(k), sep='')
    g <- ggplot(data=test, aes(X1, X2)) + 
            geom_point(aes(colour = predict), size=0.5) + 
            geom_contour(aes(z=z), colour='black', size = 0.1) + 
            opts(legend.position="none") + opts(title=title)
    # training points
    g <- g + geom_point(data=train, aes(X1, X2, colour=as.factor(Y), shape='x'))
    return(g)
}

for(k in 1:dim(train)[1]){
# for(k in 10:10){
    plot <- knnplot(train, test, k)
    name = paste('plots/', as.character(k), '.png', sep='')
    ggsave(filename=name, plot=plot, height=5, width=5)
}

# to make movie
# ffmpeg -f image2 -r 1 -i plots/%d.png -b 800k knn.mp4
