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
summary(first_stage) # seems strong, correlated w/ carcass production

# check for fixed effects
feols(production_carcass ~ w_avg | code_name + year, data = regression_data) # this still gives us a negative estimate which is weird

# make a graph
ggplot(regression_data, aes(x = w_avg, y = production_carcass, color = factor(code_name))) +
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

## Forward Stepwise Regression
base <- lm(production_carcass ~ 1, data=regression_data)
full <- lm(production_carcass ~ . + .^2, data=regression_data)
fwdAIC <- stepAIC(base, scope = list(lower = base, upper = full), regression_data, direction = "forward", trace = FALSE)
summary(fwdAIC)

# subsample the variables/interactions from the AIC
significant_vars <- names(coef(summary(fwdAIC))[, "Pr(>|t|)"][
  coef(summary(fwdAIC))[, "Pr(>|t|)"] < 0.05
])
#significant_vars <- paste0("`", significant_vars, "`")
#significant_vars <- make.names(significant_vars)

# run the new model
AIC_form <- as.formula(paste("production_carcass ~", paste(significant_vars, collapse = " + ")))
AIC_model <- lm(AIC_form, data = regression_data)
summary(AIC_model)

# use lasso maybe?
X <- model.matrix(~ poly(w_avg, 3, raw = TRUE), data = regression_data)[, -1]
y <- regression_data$production_carcass

set.seed(04152025)
cv_lasso <- cv.glmnet(X, y, alpha = 1) # alpha is the penalty
best_lambda <- cv_lasso$lambda.min # choosing the best lambda
lasso_model <- glmnet(X, y, alpha = 1, lambda = best_lambda) # refit lasso
coef(lasso_model) # check coefficients and which one zero

# make scatter plots and have the regression line in their
model_6 <- lm(production_carcass ~ poly(w_avg, 6, raw = TRUE), 
              data = regression_data)
summary(model_6)

w_grid <- seq(min(regression_data$w_avg, na.rm = TRUE),
              max(regression_data$w_avg, na.rm = TRUE),
              length.out = 100)

# predict the fitted values for this grid
pred_data <- data.frame(w_avg = w_grid)
pred_data$fit <- predict(model_6, newdata = pred_data)

# plot the data and the fitted curve
ggplot(regression_data, aes(x = w_avg, y = production_carcass, color = "red")) +
  geom_point() +
  geom_line(data = pred_data, aes(x = w_avg, y = fit), color = "black") +
  labs(
    x = "w_avg",
    y = "State Production (Carcass)",
    title = "Degree-6 Polynomial Fit"
  )

# how do I tweak this hmmmmmmmm




# how do I construct an instrument???? start with a linear regression I guess
initial_reg = lm(price_fat ~ w_avg, data = regression_data)
summary(initial_reg) # seems like weighted spei average is a statistically significant factor in price, aligns with prior beliefs
# tried for w/ cities and w/o cities and got the same results

# make some graphs showing the relationships based on different vars
ggplot(regression_data, aes(x = w_avg, y = price_fat, color = year)) +
  geom_point(alpha = 0.6) +  # Add points with transparency
  labs(title = "Densities of average SPEI vs Cattle Price by Month",
       x = "Weighted Average",
       y = "Price",
       color = "Year") +  # Label for legend
  ylim(NA, 200)

# test non-linearities

# use lasso maybe?

# make scatter plots and have the regression line in their

# how do I tweak this hmmmmmmmm


### ADDING LAGS

lagged1_regression = regression_data %>%
  mutate(
    quarter = case_when(
      quarter == 1 ~ 4,  # Move Q1 to Q4
      quarter == 2 ~ 1,  # Move Q2 to Q1
      quarter == 3 ~ 2,  # Move Q3 to Q2
      quarter == 4 ~ 3   # Move Q4 to Q3
    ),
    year = ifelse(quarter == 4 & lag(quarter, default = 4) == 1, year - 1, year)  # Adjust year for Q1 -> Q4 shift
  )

# run the first stage
first_stage1 = lm(production_carcass ~ w_avg, data = lagged1_regression)
summary(first_stage1)

# make tables for multiple lags, can do this manually



# then do lasso and the specification it gives (make sure to do cross-validation for different tuning parameters)

# make sure to post all this stuff on slack











