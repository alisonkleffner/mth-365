## ----echo=FALSE, message=FALSE, warning = FALSE-----------------------------------------------------------------------------------------------
library(tidyverse)
library(RColorBrewer)
library(mosaic)
library(infer)

## ---------------------------------------------------------------------------------------------------------------------------------------------
library(readxl)
happyplanet2016 <- read_excel("happyplanet2016.xlsx")
glimpse(happyplanet2016)


## ---------------------------------------------------------------------------------------------------------------------------------------------
happyplanet2 <- na.omit(happyplanet2016[,-8])

#scale and find covariance matrix as part of function
happy_pca <- prcomp(x=happyplanet2[,3:10], 
                    center=TRUE, scale=TRUE) 

summary(happy_pca) #proportion of variance explained


## ---------------------------------------------------------------------------------------------------------------------------------------------
happy_pca$rotation #loadings


## ----message=FALSE, fig.align='center', fig.width= 12, fig.height=5---------------------------------------------------------------------------
#install.packages("factoextra")
library(factoextra)
fviz_eig(happy_pca, main = "Scree plot: Happy Planet Index")


## ----eval = FALSE-----------------------------------------------------------------------------------------------------------------------------
fviz_pca_var(happy_pca, col.var = "contrib")


## ----message=FALSE----------------------------------------------------------------------------------------------------------------------------
#install.packages("devtools")
library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)


## ----eval = FALSE, fig.align='center', fig.height=8, fig.width=16-----------------------------------------------------------------------------
ggbiplot(happy_pca, groups=happyplanet2$Region, ellipse=TRUE)


## ----fig.align='center', fig.height=8, fig.width=19, eval = FALSE-----------------------------------------------------------------------------
ggbiplot(happy_pca, groups=happyplanet2$Region, ellipse=TRUE, choices=c(3,4))

