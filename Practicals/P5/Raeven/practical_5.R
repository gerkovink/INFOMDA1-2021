# Practical 5
# Raeven van den Acker
# 05-10-2021

# Load packages ----
library(ISLR)
library(glmnet)
library(tidyverse)

set.seed(45)

# 1. Baseball dataframe removing rows w/o salary

baseball <- 
  filter(Hitters, !is.na(Salary))

nrow(baseball) # there are 263 baseball players left

# 2. Create baseball_train (50%), baseball_valid (30%) and baseball_test (20%) data sets

split <- c(rep("train", 132), rep("valid", 79), rep("test",  52))

baseball <- baseball %>% 
  mutate(split = sample(split))
baseball_train <- baseball %>% filter(split=="train")
baseball_valid <- baseball %>% filter(split=="valid")
baseball_test <- baseball %>% filter(split=="train")

# 3. Create the function lm_mse()

lm_mse <- function(formula, train_data, valid_data) {
  y_name <- as.character(formula)[2]
  y_true <- valid_data[[y_name]]
  
  fit <- lm(formula, train_data)
  y_pred <- predict(fit, newdata = valid_data)
  
  mean((y_true - y_pred)^2)
}

# 4. Try out the lm_mse() function with baseball_train and baseball_valid

lm_mse(Salary ~ Hits + Runs, baseball_train, baseball_valid)

# 5. Create a character vector for all predictor variables in Hitters

source("generate_formulas.R")

x_vars <- colnames(Hitters)
x_vars <- x_vars[x_vars!='Salary']

# 6. Generate all formulas with outcome Salary and input 3 predictors
formulas <- generate_formulas(p = 3, x_vars = x_vars, y_var = 'Salary')

# 7. Find the best 3 predictors in the Hitters data

mse_array <- rep(0, length(formulas))

for (i in 1:length(formulas)) {
  mse_array[i] <- lm_mse(as.formula(formulas[i]), baseball_train, baseball_valid)
}

best_pred <- formulas[which.min(mse_array)]

# 8. Do the same with 1, 2 and 4 predictors

best_pred <- rep(0, 4)
min_mses <- rep(0,4)

for (j in 1:4) {
  
  formulas <- generate_formulas(p = j, x_vars = x_vars, y_var = 'Salary')
  mse_array <- rep(0, length(formulas))
  
  for (i in 1:length(formulas)) {
    mse_array[i] <- lm_mse(as.formula(formulas[i]), baseball_train, baseball_valid)
  }
  
  min_mses[j] <- min(mse_array)
  best_pred[j] <- formulas[which.min(mse_array)]
}

best_model <- best_pred[which.min(min_mses)]

# 9. Calculate the test MSE for the best model and plot true v pred

lm_best <- lm(best_model, baseball_train)
mse <- function(y_true, y_pred) mean((y_true-y_pred)^2)
mse(baseball_test$Salary, predict(lm_best, newdata = baseball_test))

tibble(y_true = baseball_test$Salary, y_pred = predict(lm_best, newdata = baseball_test)) %>% 
  ggplot(aes(x = y_pred, y = y_true)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  theme_minimal() 

# 10. Our data should be in a "sparseMatrix" form, with a matrix of predictors (x)
#     and corresponding obervations (y)

# 11. Generate the input matrix and remove intercept column

x_train <- model.matrix(Salary ~ ., data = baseball_train %>% select(-split))

# 12. Perform a LASSO regression on x_train

lasso_reg <- glmnet(x_train[,-1], baseball_train$Salary, family='gaussian', alpha = 1, lambda = 15)

# 13. Which variables have been selected?

rownames(coef(lasso_reg))[which(coef(lasso_reg) != 0)]

# 14. Create a plot of predicted v. observed values with baseball_valid

x_valid <- model.matrix(Salary ~ ., data = baseball_valid %>% select(-split))[, -1]
y_pred <- as.numeric(predict(lasso_reg, newx = x_valid))

tibble(Predicted = y_pred, Observed = baseball_valid$Salary) %>% 
  ggplot(aes(x = Predicted, y = Observed)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  theme_minimal() 

# 15. Fit a LASSO regression without lambda value

lasso_no_lambda <- glmnet(x_train[,-1], baseball_train$Salary, family='gaussian', alpha = 1)

plot(lasso_no_lambda)

# 16. What is the best lambda value?

x_cv <- model.matrix(Salary ~ ., bind_rows(baseball_train, baseball_valid)[, -21])[, -1]
result_cv <- cv.glmnet(x = x_cv, y = c(baseball_train$Salary, baseball_valid$Salary), nfolds = 15)
best_lambda <- result_cv$lambda.min
best_lambda

# 17. Plot the result. Discuss findings

plot(result_cv)

# there is a local peak in the MSE at log(lambda=1.7)
# the MSE is relatively constant until log(lambda ~ 3.5), then it increases

# 18. Predict new salaries for baseball players in baseball_test

x_test <- model.matrix(Salary ~ ., data = baseball_test %>% select(-split))[, -1]
y_pred <- as.numeric(predict(result_cv, newx = x_test, s = best_lambda))

tibble(Predicted = y_pred, Observed = baseball_test$Salary) %>% 
  ggplot(aes(x = Predicted, y = Observed)) +
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  theme_minimal()

# 19. Method comparison

train_data <- bind_rows(baseball_train, baseball_valid)

pred_linear <- predict(lm(Salary ~ ., data = train_data), newdata = baseball_test)
pred_subset <- predict(lm(Salary ~ Runs + CHits + Division + PutOuts, data = train_data),
                        newdata = baseball_test)
pred_lasso <- as.numeric(predict(glmnet(x_cv, train_data$Salary, lambda = 50), newx = x_test))
pred_cv <- as.numeric(predict(result_cv, newx = x_test, s = best_lambda))

mses <- c(
  mse(baseball_test$Salary, pred_linear),
  mse(baseball_test$Salary, pred_subset),
  mse(baseball_test$Salary, pred_lasso),
  mse(baseball_test$Salary, pred_cv)
)

tibble(Method = as_factor(c("lm", "subset", "lasso", "cv_las")), MSE = mses) %>% 
  ggplot(aes(x = Method, y = MSE, fill = Method)) +
  geom_bar(stat = "identity", col = "black") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "Comparison of test set MSE for different prediction methods") +
  scale_fill_viridis_d() # different colour scale



