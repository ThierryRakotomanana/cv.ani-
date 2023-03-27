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

group_animation <- function(iter) {
  df <- data.frame(x = Sonar$V1, y = Sonar$V2, fold = as.factor(folds))
  df <- df[df$fold %in% 1:k,]
  p <- ggplot(df, aes(x, y, color = fold)) +
    geom_point() +
    labs(title = paste("Fold", iter), x = "Feature 1", y = "Feature 2")
  return(p)
}