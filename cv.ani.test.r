library(testthat)

# Exemple de validation croisée k-fold
data(Sonar)
set.seed(123)
k <- 5
folds <- sample(rep(1:k, length.out = nrow(Sonar)))

# Fonction à tester : ajustement du modèle
fit_model <- function(train, test) {
  model <- glm(Class ~ ., data = train, family = binomial)
  prob <- predict(model, test, type = "response")
  pred <- ifelse(prob > 0.5, "R", "M")
  return(list(pred = pred, obs = test$Class))
}

# Fonction à tester : visualisation du modèle
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

# Test unitaire
test_that("fit_model() and model_animation() return expected results", {
  train <- Sonar[folds != 1,]
  test <- Sonar[folds == 1,]
  res <- fit_model(train, test)
  expect_equal(length(res$pred), nrow(test))
  expect_equal(length(res$obs), nrow(test))
  p <- model_animation(1)
  expect_true("ggplot" %in% class(p))
  expect_equal(length(p$data), nrow(Sonar))
})
