#first laod library
library(animint2)
library(mlbench)

data(Sonar)
set.seed(123)
k <- 5
folds <- sample(rep(1:k, length.out = nrow(Sonar)))

fit_model <- function(train, test) {
  model <- glm(Class ~ ., data = train, family = binomial)
  prob <- predict(model, test, type = "response")
  pred <- ifelse(prob > 0.5, "R", "M")
  return(list(pred = pred, obs = test$Class))
}