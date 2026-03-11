## ----echo=FALSE, message=FALSE, warning = FALSE----------------------------------------
library(tidyverse)
library(RColorBrewer)
library(mosaic)
library(infer)
library(nycflights13)

## ----echo = FALSE, message=FALSE-------------------------------------------------------
set.seed(14)
Chicago1000 <- flights %>%
  filter(dest %in% c('ORD', 'MDW'), !is.na(arr_delay)) %>% 
  sample_n(size=1000)


## --------------------------------------------------------------------------------------
set.seed(365)
test_id <- sample(1:nrow(Chicago1000), 
                  size=round(0.2*nrow(Chicago1000)))
TEST <- Chicago1000[test_id,]
TRAIN <- Chicago1000[-test_id,]


## --------------------------------------------------------------------------------------
model1 = lm(arr_delay ~ hour + dep_delay, data = TRAIN)


## --------------------------------------------------------------------------------------
predictions <- predict(model1, TEST)
head(predictions)


## ----message=FALSE---------------------------------------------------------------------
library(Metrics)
rmse(TEST$arr_delay, predictions)


## --------------------------------------------------------------------------------------
mae(TEST$arr_delay, predictions)

## --------------------------------------------------------------------------------------
model2 = lm(arr_delay ~ hour + dep_delay + distance, data = TRAIN)
predictions_distance <- predict(model2, TEST)

TEST %>% summarize(
  MSE1 = rmse(arr_delay, predictions),
  MSE2 = rmse(arr_delay, predictions_distance))

TEST %>% summarize(
  MAE1 = mae(arr_delay, predictions),
  MAE2 = mae(arr_delay, predictions_distance))

## ----message=FALSE---------------------------------------------------------------------
library(caret)
train_control <- trainControl(method = "cv", number = 5)

model <- train(arr_delay ~ hour + dep_delay, data = Chicago1000, 
               trControl = train_control, method = "lm")
model
