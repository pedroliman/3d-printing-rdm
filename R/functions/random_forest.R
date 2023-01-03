
library(dplyr)
library(randomForest)
library(ggplot2)


# make test data ----------------------------------------------------------

test_data = data.frame(a = rnorm(n = 1000, mean = 10, sd = 9), 
                       b = rnorm(n = 1000, mean = 20, sd = 4),
                       c = rnorm(n = 1000, mean = 2, sd = 3)) %>%
            mutate(y = a + b^3 * c) %>%
            mutate(y_high = as.factor(y > quantile(y, probs = 0.95))) %>%
            select(-y)
      

left_hand_side_vars <- c("a","b","c")

# train random forest -----------------------------------------------------

rf <- randomForest::randomForest(y_high ~ ., data = test_data)
                
# Use importance to rank important variables

rf_importance <- importance(rf)

vars_by_importance <- rownames(rf_importance)[order(rf_importance, decreasing = T)]

# run predictions for the two-most important variables ------------------------

mins <- sapply(test_data %>% select(-y_high), min) 
maxs <- sapply(test_data %>% select(-y_high), max)
means <- sapply(test_data %>% select(-y_high), mean)

# make a grid for the two-most important variables:

variables <- c(vars_by_importance[1], vars_by_importance[2])

variables_not_included <- left_hand_side_vars[!left_hand_side_vars %in% variables] 

x1 <- seq.default(from = mins[variables[1]], to = maxs[variables[1]], length.out = 50)
x2 <- seq.default(from = mins[variables[2]], to = maxs[variables[2]], length.out = 50)

df_prediction <- tidyr::expand_grid(x1, x2)

names(df_prediction) <- variables

# Assign mean of variables not included:

for (v in variables_not_included) {
  df_prediction[,v] <- means[v]
}

# predict y:
df_prediction[,"y_pred"] <- predict(rf, newdata = df_prediction)

# plot

ggplot() + 
  geom_tile(data = df_prediction, mapping = aes(x = c, y = b, fill = y_pred), alpha = 0.1) + 
  geom_point(data = test_data, shape=21, mapping = aes(x = c, y = b, fill = y_high), color = "white") + 
  randplot::theme_rand()


# Fit random forest -------------------------------------------------------



# 2x2 decision boundaries -------------------------------------------------


