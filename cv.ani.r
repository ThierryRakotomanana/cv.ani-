#first laod library
library(animint2)
library(mlbench)

data(Sonar)
set.seed(123)
k <- 5
folds <- sample(rep(1:k, length.out = nrow(Sonar)))