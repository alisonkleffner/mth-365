## ----echo=FALSE, message=FALSE, warning = FALSE---------------------------------------------------------
library(tidyverse)
library(mosaic)
library(infer)


## ----echo = FALSE, message=FALSE------------------------------------------------------------------------
library(nycflights13)
set.seed(14)
Chicago1000 <- flights %>%
  filter(dest %in% c('ORD', 'MDW'), !is.na(arr_delay)) %>% 
  sample_n(size=1000)


## -------------------------------------------------------------------------------------------------------
set.seed(365)
test_id <- sample(1:nrow(Chicago1000), 
                  size=round(0.2*nrow(Chicago1000)))
TEST <- Chicago1000[test_id,]
TRAIN <- Chicago1000[-test_id,]


## -------------------------------------------------------------------------------------------------------
model1 = lm(arr_delay ~ hour + dep_delay, data = TRAIN)


## -------------------------------------------------------------------------------------------------------
predictions <- predict(model1, TEST)


## ----message=FALSE--------------------------------------------------------------------------------------
library(Metrics)
rmse(TEST$arr_delay, predictions)

## ----message=FALSE--------------------------------------------------------------------------------------
library(caret)
train_control <- trainControl(method = "cv", number = 5)

model <- train(arr_delay ~ hour + dep_delay, data = Chicago1000, 
               trControl = train_control, method = "lm")
model


## ----echo=FALSE, fig.align='center', fig.height=5.5, fig.width=8----------------------------------------
set.seed(365)
x1 <- runif(min=0.1, max=0.5, n=20)
x2 <- runif(min=0.3, max=0.7, n=20)
x3 <- runif(min=0.5, max=0.9, n=20)
y1 <- runif(min=0.5, max=0.7, n=20)
y2 <- runif(min=0.4, max=0.6, n=20)
y3 <- runif(min=0.2, max=0.5, n=20)
x <- c(x1, x2, x3)
y <- c(y1, y2, y3)
group <- c(rep('A', 20), rep('B', 20), rep('C', 20))
data <- as.data.frame(cbind(x, y, group))
colnames(data) <- c('x', 'y', 'group')
data$x <- as.numeric(data$x)
data$y <- as.numeric(data$y)
ggplot(data, aes(x=x, y=y))+geom_point(size = 3)


## ----echo=FALSE, fig.align='center', fig.height=6.5, fig.width=8----------------------------------------
new.pts <- data.frame(x = c(0.2, 0.4), y = c(0.5, 0.2), group=c('Point 1', 'Point 2'))
data.new <- rbind(data, new.pts)
ggplot(data.new, aes(x=x, y=y, group=group))+geom_point(size = 5, aes(col=group, pch=group))+scale_color_brewer(palette='Set1')


## -------------------------------------------------------------------------------------------------------
library(class)


## -------------------------------------------------------------------------------------------------------
knnMod1 = knn(train=data[,1:2], 
          test = new.pts[,1:2], 
          cl = data$group, 
          k = 2, prob = TRUE)
knnMod1


## -------------------------------------------------------------------------------------------------------
knnMod2 = knn(train =data[,1:2], 
          test = new.pts[,1:2], 
          cl = data$group, 
          k = 10, prob = TRUE)
knnMod2

## ----echo = FALSE---------------------------------------------------------------------------------------
library(ISLR)
data(Credit)


## ----echo=FALSE-----------------------------------------------------------------------------------------
Credit <- Credit %>% mutate(Utilization = Balance/Limit) %>% 
  mutate(Quartile = ifelse(Utilization<0.01851, 'Q1', 
                           ifelse(Utilization<0.09873, 'Q2',
                           ifelse(Utilization<0.14325, 'Q3','Q4'))))



## ----echo=FALSE, fig.align='center', fig.height=6.5, fig.width=11---------------------------------------
ggplot(Credit, aes(x=Age, y=Rating, group=Quartile)) + 
  geom_point(aes(col=Quartile, pch=Quartile), size = 3) + 
  scale_color_brewer(palette='Set1')


## ----echo = FALSE---------------------------------------------------------------------------------------
apps <- data.frame(Age = c(33, 47, 21), 
                         Rating = c(750, 400, 250), 
                         Quartile=c('New', 'New', 'New'))


## ----echo = FALSE---------------------------------------------------------------------------------------
old = Credit %>% dplyr::select(Age, Rating, Quartile)
full = rbind(old, apps)
full = full %>%
  mutate(Quartile = fct_relevel(Quartile,
                                "Q1", "Q2", "Q3", "Q4", "New"))
#str(full$Quartile)



## ----fig.align='center', fig.height=4, fig.width=9, echo = FALSE----------------------------------------
ggplot(full, aes(x=Age, y=Rating, group=Quartile)) + 
  geom_point(aes(col=Quartile, pch=Quartile), size = 4) + 
  scale_color_brewer(palette='Set1')


## -------------------------------------------------------------------------------------------------------
knn20 = knn(train = old[,1:2],
            test = apps[,1:2],
            cl = old[,3], 
            k = 20, prob = TRUE)
knn20


## -------------------------------------------------------------------------------------------------------
knn100 = knn(train = old[,1:2],
            test = apps[,1:2],
            cl = old[,3], 
            k = 100, prob= TRUE)
knn100


## -------------------------------------------------------------------------------------------------------
set.seed(365)
test_ID = sample(1:nrow(Credit), size = 100)
TEST = Credit[test_ID,]
TRAIN = Credit[-test_ID, ]

#only select variables we want
knn_train = TRAIN %>% dplyr::select(Age, Rating, Income, 
                                    Cards, Education)
knn_test = TEST %>% dplyr::select(Age, Rating, Income, 
                                  Cards, Education) 


## -------------------------------------------------------------------------------------------------------
knn50 = knn(train = knn_train, 
            test = knn_test,
            cl = TRAIN$Quartile, 
            k = 50, prob = TRUE)
knn50



## -------------------------------------------------------------------------------------------------------
#Create Confusion Matrix
t = table(knn50, TEST$Quartile)
t

sum(diag(t))/nrow(TEST) #Classification Accuracy



## -------------------------------------------------------------------------------------------------------
knn_train_scale <- knn_train %>% scale()
knn_test_scale <- knn_test %>% scale()

knn50_scale = knn(train = knn_train_scale, 
            test = knn_test_scale,
            cl = TRAIN$Quartile, 
            k = 50, prob = TRUE)


t_scale = table(knn50_scale, TEST$Quartile)

sum(diag(t_scale))/nrow(TEST) #Classification Accuracy



## -------------------------------------------------------------------------------------------------------
knn_train = TRAIN %>% dplyr::select(Age, Rating, Income, Cards, 
                                    Education)
knn_test = TEST %>% dplyr::select(Age, Rating, Income, Cards, 
                                  Education) 

knn50 = FNN::knn.reg(train = knn_train, 
            test = knn_test,
            y = TRAIN$Utilization, 
            k = 50)

head(knn50$pred, n = 5)

rmse(TEST$Utilization, knn50$pred)


