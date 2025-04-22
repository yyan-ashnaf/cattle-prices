library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(geobr)
library(tidyverse)
library(knitr)
library(ggplot2)
library(glmnet)
library(fixest)
library(MASS)

# load regression data
regression_data = read.csv("data/regression_data.csv")

### INITAL TESTING

# run against cattle quantity
first_stage = lm(production_carcass ~ w_avg, data = regression_data)
summary(first_stage) # gives negative estimate which is weird

# check for fixed effects
feols(production_carcass ~ w_avg | state_code_name + year, data = regression_data) # this gives a positive estimate now which aligns with beliefs

# make a graph
ggplot(regression_data, aes(x = w_avg, y = production_carcass, color = factor(state_code_name))) +
  geom_point(alpha = 0.6) +  # Add points with transparency
  labs(title = "Densities of average SPEI vs Cattle Price by State",
       x = "Weighted Average",
       y = "Carcass Production",
       color = "State") # label

### ADDING COMPLEXITY

# test non-linearities
quad_reg = lm(production_carcass ~ w_avg + I(w_avg^2), data = regression_data)
summary(quad_reg)
anova(first_stage, quad_reg) # test to see if there is a significant difference

model_finder = function(starting_order, p_value_threshold, max_polynomial_order) {
  # initialize a "best model" object, initially linear
  best_formula <- as.formula("production_carcass ~ w_avg")
  best_model <- lm(best_formula, data = regression_data)
  
  # iteratively add polynomial terms up to 10
  for (k in 2:max_polynomial_order) {
    
    # create a formula with all of the terms
    polynomial_terms <- paste0("I(w_avg^", 1:k, ")", collapse = " + ")
    formula_k <- as.formula(paste("production_carcass ~", polynomial_terms))
    
    # fit the model with k polynomial terms
    model_k <- lm(formula_k, data = regression_data)
    
    # extract p-value for the highest-order term
    coef_summary <- summary(model_k)$coefficients
    pval_highest_order <- coef_summary[nrow(coef_summary), 4]
    
    # compare p-val of new higher order eq to threshold
    if (pval_highest_order < p_value_threshold) {
      # if significant, repeat process
      best_model <- model_k
    } else {
      # if not, then stop and accept latest model
      break
    }
  }
  
  # print final model summary
  summary(best_model)
}

model_finder(1, 0.05, 10)

# do a fixed effects version of the quadratic equation
feols(production_carcass ~ w_avg + w_avg^2 | state_code_name + year, data = regression_data) 3 # not very good results!

# add lags for time
regression_data_lag1 = regression_data %>%
  mutate(
    year = if_else(quarter == 4, year + 1, year),
    quarter = if_else(quarter == 4, 1, quarter + 1) # can lag more by repeating this process
  )

# run the lagged regression w/ fixed effects
feols(production_carcass ~ w_avg | state_code_name + year, data = regression_data_lag1) # now gives negative estimate again

# maybe one more lag?
regression_data_lag2 = regression_data_lag1 %>%
  mutate(
    year = if_else(quarter == 4, year + 1, year),
    quarter = if_else(quarter == 4, 1, quarter + 1) # can lag more by repeating this process
  )

feols(production_carcass ~ w_avg | state_code_name + year, data = regression_data_lag2) # even more negative?

# lets do it again for till with reach a year
regression_data_lag3 = regression_data_lag2 %>%
  mutate(
    year = if_else(quarter == 4, year + 1, year),
    quarter = if_else(quarter == 4, 1, quarter + 1) # can lag more by repeating this process
  )

feols(production_carcass ~ w_avg | state_code_name + year, data = regression_data_lag3)

regression_data_lag4 = regression_data_lag3 %>%
  mutate(
    year = if_else(quarter == 4, year + 1, year),
    quarter = if_else(quarter == 4, 1, quarter + 1) # can lag more by repeating this process
  )

feols(production_carcass ~ w_avg | state_code_name + year, data = regression_data_lag4) # became positive again after a full year!
# probably sign we shouldn't be working with lags

# maybe use a lasso technique to find a best equation
set.seed(04222025)
x = data.matrix(regression_data[, c("state_code_name", "w_avg", "year", "city_lean", "price_lean", "month", "quarter", "inspection_type" "price_lean")])
y = regression_data$production_carcass

cv_model = cv.glmnet(x, y, alpha = 1) # find best lambda
lambda = cv_model$lambda.min

best_model <- glmnet(x, y, alpha = 1, lambda = lambda) # use new lambda
coef(best_model) # can ask about this with the others














