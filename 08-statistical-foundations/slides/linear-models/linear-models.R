## ----echo=FALSE, message=FALSE, warning = FALSE-----------------------------
library(tidyverse)
library(RColorBrewer)
library(mosaic)

## ----message=FALSE, echo = FALSE--------------------------------------------
x = seq(-100, 100)    # just a sequence of numbers
y = x + rnorm(length(x), 0, 50)      # generate linear association + noise

xy <- data.frame(x,y)

xy %>% ggplot(aes(x=x, y = y)) + geom_point() +
  geom_smooth(method = "lm", color = "orange", se = FALSE) +
  ggtitle("Linear Relationship") + theme(title = element_text(size = 24))


## ----message=FALSE, echo = FALSE--------------------------------------------
x = seq(-100, 100)    # just a sequence of numbers
y = x^2 + rnorm(length(x), 0, 1000)      # generate non-linear association + noise

xy <- data.frame(x,y)

xy %>% ggplot(aes(x=x, y = y)) + geom_point() +
  geom_smooth(method = "loess", color = "purple", se = FALSE) +
  geom_smooth(method = "lm", color = "orange", se = FALSE) +
  ggtitle("Non-Linear Relationship") + theme(title = element_text(size = 24))


## ----echo = FALSE-----------------------------------------------------------
library(nycflights13)
set.seed(14)
Chicago1000 <- flights %>%
  filter(dest %in% c('ORD', 'MDW'), !is.na(arr_delay)) %>% 
  sample_n(size=1000)

## ----eval = FALSE-----------------------------------------------------------
colnames(Chicago1000)


## ----message=FALSE, fig.align='center', fig.height=4, fig.width=9-----------
Chicago1000 %>% 
  ggplot(aes(x = hour, y = arr_delay)) + geom_point() + 
  geom_smooth(method = "lm") +
  theme(axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18))


## ---------------------------------------------------------------------------
model = lm(arr_delay ~ hour, data = Chicago1000)
summary(model)


## ----message=FALSE, warning=FALSE, echo=FALSE, fig.align='center', fig.height=5, fig.width=8----
Chicago1000 %>% 
  ggplot(aes(x = carrier, y = arr_delay)) + 
  geom_boxplot(aes(color = carrier)) +
  theme(axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18))


## ---------------------------------------------------------------------------
Chicago1000$carrier = relevel(as.factor(Chicago1000$carrier), 
                              ref = "UA")

model2 = lm(arr_delay ~ carrier, data = Chicago1000)
broom::tidy(model2)


## ---------------------------------------------------------------------------
model3 = aov(arr_delay ~ carrier, data = Chicago1000)
summary(model3)


## ----echo=FALSE, message = FALSE, eval = FALSE------------------------------
library(multcomp)
model5 = glht(model3, linfct = mcp(carrier = "Tukey"))
s <- summary(model5, test = adjusted("holm"))
broom::tidy(s)


## ----message=FALSE, fig.align='center', fig.height=4, fig.width=8-----------
Chicago1000 %>% ggplot(aes(x = hour, y = arr_delay)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  aes(color = carrier)


## ---------------------------------------------------------------------------
model4 = lm(arr_delay ~ hour + carrier, data = Chicago1000)


## ----echo = FALSE-----------------------------------------------------------
summary(model4)


## ---------------------------------------------------------------------------
AIC(model4)


## ---------------------------------------------------------------------------
BIC(model4)


## ----fig.align='center', fig.height=3.5, fig.width=8------------------------
plot(model4, which = 2)


## ----fig.align='center', fig.height=5, fig.width=8--------------------------
plot(model4, which = 1)


## ----fig.align='center', fig.height=5, fig.width=8--------------------------
plot(model4, which = 3)


## ----fig.align='center', fig.height=5, fig.width=8--------------------------
library(corrplot)
M <- cor(Chicago1000[,c(15:16)])
corrplot(M, method = "number", type = "upper")

