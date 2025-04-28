# import packages
library(dplyr)
library(ggplot2)
library(rpart) # model packages
library(caret) # model packages
library(rpart.plot)
library(vip)
library(pdp)


# prep data --------------------------------------------------------------------

# Ames housing data
# install.packages("AmesHousing")
library(AmesHousing)
ames <- AmesHousing::make_ames()

# in base R
set.seed(42) # HhGTTG reproducibility
index_1 <- sample(1:nrow(ames), round(nrow(ames) * 0.7)) # randomly select 70% of the row indices without replacement
ames_train_1 <- ames[index_1,] # select rows from index_1
ames_test_1 <- ames[-index_1,] # select those rows outside of index_1
dim(ames_test_1)

set.seed(123)
index_2 <- sample(1:nrow(ames), round(nrow(ames) * 0.7)) # randomly select 70% of the row indices without replacement
ames_train_2 <- ames[index_2,] # select rows from index_2
ames_test_2 <- ames[-index_2,] # select those rows outside of index_2


# fitting trees : classification------------------------------------------------
ames_dt1 <- rpart(
  formula = Central_Air ~ .,
  data    = ames_train_1,
  method  = "class", # anova is regression
  parms = list(split = "gini"), # alt split = "information"
)

# visualizing tree output
ames_dt1
rpart.plot(ames_dt1)

# visualizing feature importance using the vip package
vip(ames_dt1, num_features = 15)

# evaluate performance: predict class labels
pred_class <- predict(ames_dt1, newdata = ames_test_1, type = "class")
table(Predicted = pred_class, Actual = ames_test_1$Central_Air)
confusionMatrix(pred_class, ames_test_1$Central_Air)



# fitting trees : regression----------------------------------------------------
ames_dt2 <- rpart(
  formula = Year_Built ~ .,
  data    = ames_train_2,
  method  = "anova", # anova is regression
  control = list(cp = 0, xval = 10)
)

# visualizing tree output
ames_dt2
rpart.plot(ames_dt2)

# hyperparameter tuning (built-in default 10 fold cross validation)
plotcp(ames_dt2)
abline(v = 10, lty = "dashed")

ames_dt2 <- rpart(
  formula = Year_Built ~ .,
  data    = ames_train_2,
  method  = "anova", # anova is regression
  control = rpart.control(maxdepth = 5))

# visualizing feature importance using the vip package
# visualizing tree output
ames_dt2
rpart.plot(ames_dt2)
vip(ames_dt2, num_features = 15)

# evaluate performance: root mean squared error
pred_val <- predict(ames_dt2, newdata = ames_test_2)
rmse <- sqrt(mean((pred_val - ames_test_2$Year_Built)^2))
rmse # off by about 10 years

