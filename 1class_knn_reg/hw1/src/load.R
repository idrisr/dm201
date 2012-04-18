# load required data

load_data <- function() {
    data <- read.csv('data/winequality-red.csv', header=TRUE, sep=';')
}
