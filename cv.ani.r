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

model_animation <- function(iter) {
  train <- Sonar[folds != iter,]
  test <- Sonar[folds == iter,]
  res <- fit_model(train, test)
  df <- data.frame(x = Sonar$V1, y = Sonar$V2, pred = res$pred, obs = res$obs)
  p <- ggplot(df, aes(x, y, color = pred, shape = obs)) +
    geom_point(size = 3) +
    scale_color_manual(values = c("#FF0000", "#0000FF")) +
    scale_shape_manual(values = c(24, 21)) +
    labs(title = paste("Fold", iter), x = "Feature 1", y = "Feature 2")
  return(p)
}

cv_animation <- animint(
  data = Sonar,
  ggplot = list(
    group_animation(1) + transition_states(fold),
    model_animation(1) + transition_states(fold),
    group_animation(2) + transition_states(fold),
    model_animation(2) + transition_states(fold),
    group_animation(3) + transition_states(fold),
    model_animation(3) + transition_states(fold),
    group_animation(4) + transition_states(fold)
  )