## --------------------------------------------------------------------------------------------------------------
# This is a comment


## --------------------------------------------------------------------------------------------------------------
3+4 #Addition
5-2 #Subtraction
3*2 #Multiplication
9/3 #Division 


## --------------------------------------------------------------------------------------------------------------
sqrt(4) #square root
2^3 #exponent
exp(3) #exponential function
log(10) #natural log


## --------------------------------------------------------------------------------------------------------------
log(10, base = 10)
abs(-1) #absolute value
floor(3.7) #round down
ceiling(3.2) #round up


## --------------------------------------------------------------------------------------------------------------
round(3.2)
pi
sin(pi/2)
5%%3 #division remainder


## ----eval = FALSE----------------------------------------------------------------------------------------------
## 10(2+4)


## --------------------------------------------------------------------------------------------------------------
10*(2+4)


## --------------------------------------------------------------------------------------------------------------
x <- 5 


## --------------------------------------------------------------------------------------------------------------
log(x)


## --------------------------------------------------------------------------------------------------------------
y <- c(1, 5, 3, 2)


## --------------------------------------------------------------------------------------------------------------
y/2


## --------------------------------------------------------------------------------------------------------------
length(y)


## --------------------------------------------------------------------------------------------------------------
bulldogs <- c("american", "english", "french")
bulldogs


## --------------------------------------------------------------------------------------------------------------
length(bulldogs)
       
str(bulldogs)


## ----eval= FALSE-----------------------------------------------------------------------------------------------
install.packages("tidyverse")

## ----eval = FALSE----------------------------------------------------------------------------------------------
library(tidyverse)

## ----eval = FALSE----------------------------------------------------------------------------------------------
## getwd() #What is your working directory
## setwd() # Change Working directory


## ----eval=FALSE------------------------------------------------------------------------------------------------
data = read.csv("movie-2018.csv")

## ----linewidth=100---------------------------------------------------------------------------------------------
head(data, n = 3) #make sure data loads in correctly


## ----linewidth=140---------------------------------------------------------------------------------------------
glimpse(data)


## --------------------------------------------------------------------------------------------------------------
#To change this column to a number:
data$Average.audience <- as.numeric(data$Average.audience)


#Now Opening.Weekend is a number!
class(data$Average.audience)



## --------------------------------------------------------------------------------------------------------------
data[1,] #first row
data[,1] #first column 


## --------------------------------------------------------------------------------------------------------------
data[1:2,c(2,5,7)] #row: 1-2, column: 2,5,7


## --------------------------------------------------------------------------------------------------------------
head(data$Film)


## --------------------------------------------------------------------------------------------------------------
summary(data[,7:10])

nrow(data)

ncol(data)


## ----linewidth=100---------------------------------------------------------------------------------------------
table(data$Primary.Genre)


## --------------------------------------------------------------------------------------------------------------
mean(data$Domestic.Gross, na.rm = TRUE) #na.rm removes missing values
sd(data$Domestic.Gross, na.rm = TRUE)
summary(data$Domestic.Gross)
quantile(data$Domestic.Gross, c(0.025, 0.975), na.rm = TRUE) 


## ----eval = FALSE----------------------------------------------------------------------------------------------
help(head)


## ----eval = FALSE----------------------------------------------------------------------------------------------
?head


## ----echo=FALSE------------------------------------------------------------------------------------------------


