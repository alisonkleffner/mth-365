## ----echo=FALSE, message=FALSE, warning = FALSE----------------------------------------------------
library(tidyverse)
library(mosaic)
library(infer)
library(caret)


## ----echo = FALSE----------------------------------------------------------------------------------
GSS <- read.csv("GSS2016.csv")
glimpse(GSS)


## --------------------------------------------------------------------------------------------------
GSS <- GSS %>% filter(YEAR==2016) %>% 
  filter(GRASS != 'Not applicable')


## --------------------------------------------------------------------------------------------------
GSS <- GSS %>%
mutate(LEGAL = ifelse(GRASS=='Legal', 'Legal', 'Not legal'))


## --------------------------------------------------------------------------------------------------
GSS$AGE <- as.numeric(GSS$AGE)


## --------------------------------------------------------------------------------------------------
set.seed(4)
test_id <- sample(1:nrow(GSS), size=round(0.2*nrow(GSS)))
TEST <- GSS[test_id,]
TRAIN <- GSS[-test_id,]


## --------------------------------------------------------------------------------------------------
TRAIN %>% group_by(LEGAL) %>% summarize(n=n())

## --------------------------------------------------------------------------------------------------
#install.packages('rpart')
library(rpart)
rpart(LEGAL~AGE, data=TRAIN, na.action = na.pass)


## ----message=FALSE, fig.height=8, fig.width=10, eval = FALSE---------------------------------------
#install.packages("rattle")
library(rattle)
tree <- rpart(LEGAL~AGE, data=TRAIN, na.action = na.pass)

fancyRpartPlot(tree)


## ----warning=FALSE, fig.height=6, fig.width=8, fig.align='center'----------------------------------
TRAIN %>% ggplot(aes(x=LEGAL, y=AGE)) +
geom_boxplot(aes(col=LEGAL)) +
  geom_hline(yintercept=69, col='black') 


## --------------------------------------------------------------------------------------------------
TRAIN <- TRAIN %>%
  mutate(Legal_Tree = predict(tree, type='class'))

confusion_train <- tally(Legal_Tree~LEGAL, data=TRAIN)
confusion_train


## --------------------------------------------------------------------------------------------------
TEST <- TEST %>%
  mutate(Legal_Tree = predict(tree, type='class', newdata = TEST))

confusion_test <- tally(Legal_Tree~LEGAL, data=TEST)
confusion_test


## --------------------------------------------------------------------------------------------------
sum(diag(confusion_train))/nrow(TRAIN)


## --------------------------------------------------------------------------------------------------
sum(diag(confusion_test))/nrow(TEST)


## ----message=FALSE---------------------------------------------------------------------------------
printcp(tree)


## ----eval = FALSE----------------------------------------------------------------------------------
rpart(LEGAL~AGE, data=TRAIN, na.action = na.pass, control = rpart.control(cp = 0.05))

tree2 <- rpart(AGE~POLVIEWS+MARITAL, data=TRAIN)
fancyRpartPlot(tree2)


## --------------------------------------------------------------------------------------------------
TEST <- TEST %>% filter(MARITAL != "No answer") 

predict(tree2, TEST , method = "anova") %>% head()


## ----message=FALSE---------------------------------------------------------------------------------
#install.packages('randomForest')
library(randomForest)

forest_grass <- randomForest(as.factor(LEGAL)~NEWSFROM+HAPPY+
                               RELIG+COURTS+ENERGY+EDUC+ENVIR+
                               POLVIEWS+PARTYID+REGION+INCOME+
                               SEX+DEGREE+AGE+MARITAL+BALLOT, 
                             data=TRAIN, na.action = na.omit,
                             ntree=201, mtry=4)

forest_grass


## --------------------------------------------------------------------------------------------------
TEST <- TEST %>%
  mutate(Legal_RF = predict(forest_grass, type='class', 
                            newdata = TEST)) 

TEST$Legal_RF[1:5]

confusion_test <- tally(Legal_RF~LEGAL, data=TEST)
sum(diag(confusion_test))/nrow(TEST)


## --------------------------------------------------------------------------------------------------
randomForest::importance(forest_grass) %>% as.data.frame() %>% 
  rownames_to_column() %>% arrange(desc(MeanDecreaseGini))


## ----eval = FALSE----------------------------------------------------------------------------------
tree4 <- rpart(LEGAL~AGE+REGION+POLVIEWS, data=TRAIN)
fancyRpartPlot(tree4)

## --------------------------------------------------------------------------------------------------
data("iris")

