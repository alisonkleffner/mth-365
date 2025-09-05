library(tidyverse)
library(RColorBrewer)

# Slide 8 ----------------------------------------------------------------------

data <- data.frame(expand.grid(x = 1:6, y = 1:6), 
                   color = factor(sample(c(1, 2), 36, replace = TRUE)))

data$x <- data$x + rnorm(36, 0, .25)
data$y <- data$y + rnorm(36, 0, .25)
data$shape <- factor(c(rep(2, 15), 1, rep(2,20)))

ggplot(data, aes(x, y)) + 
  geom_point(aes(shape = shape), size = 5, colour = "#1B9E77") + 
  theme_void() + 
  theme(legend.position = "none")

# Slide 9 ----------------------------------------------------------------------

data$shape <- factor(c(rep(2, 25), 1, rep(2, 10)))

ggplot(data, aes(x, y)) + 
  geom_point(aes(colour = shape), size = 5, shape = I(19)) + 
  theme_void() + theme(legend.position = "none") + 
  scale_colour_brewer(palette="Dark2")

# Slide 10 ---------------------------------------------------------------------

## Dual-Encoded
  
data$shape <- factor(sample(c(1, 2), 36, replace = TRUE))
  
ggplot(data, aes(x, y)) + 
  geom_point(aes(colour = shape, shape = shape), size = 5) + 
  theme_void() + 
  theme(legend.position = "none") + 
  scale_colour_brewer(palette="Dark2")

## Separate Variables
  
data$shape <- factor(sample(c(1, 2), 36, replace = TRUE))
  
ggplot(data, aes(x, y)) + 
  geom_point(aes(colour = shape, shape = color), size = 5) + 
  theme_void() + 
  theme(legend.position = "none") + 
  scale_colour_brewer(palette="Dark2")

# Slide 11 ---------------------------------------------------------------------

diamonds_summary <- diamonds %>%
  group_by(cut) %>%
  summarise(Percent = n()/nrow(.) * 100)

ggplot(diamonds, aes(x = "", fill = cut)) + 
  geom_bar() +
  coord_polar(theta = "y") + 
  ggtitle("Pie Chart")

diamonds %>% 
  ggplot() + 
  geom_bar(aes(x=cut,fill = cut)) + 
  ggtitle("Bar Chart")

# Slide 14 ---------------------------------------------------------------------

## Qualitative schemes: no more than 7 colors

data <- data.frame(x = 1:7, 
                   blues = brewer.pal(7, "Blues"), 
                   set1 = brewer.pal(7, "Set1"), 
                   diverge = brewer.pal(7,"RdBu"))

qplot(xmin = x-.5, xmax = x+.5, ymin = 0, ymax = 1, data = data, geom = "rect", color = I("black"), fill = set1) + 
  scale_fill_identity() + 
  ylab("") + 
  xlab("") + 
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        rect = element_blank()) + 
  coord_fixed(ratio = 1) + 
  theme_void()

## Quantitative schemes: use color gradient with only one hue for positive values

qplot(xmin = x-.5, xmax = x+.5, ymin = 0, ymax = 1, data = data, geom = "rect", color = I("black"), fill = blues) + 
  scale_fill_identity() + 
  ylab("") + 
  xlab("") + 
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        rect = element_blank()) + 
  coord_fixed(ratio = 1) + 
  theme_void()

## Quantitative schemes: use color gradient with two hues for positive and negative values. 

qplot(xmin = x-.5, xmax = x+.5, ymin = 0, ymax = 1, data = data, geom = "rect", color = I("black"), fill = diverge) + 
  scale_fill_identity() + 
  ylab("") + 
  xlab("") + 
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        rect = element_blank()) + 
  coord_fixed(ratio = 1) + 
  theme_void()

# Slide 16 ---------------------------------------------------------------------

data(HairEyeColor)
HairEyeColor[,,1] #Male
HairEyeColor[,,2] #Female

newdata_male = as.data.frame(HairEyeColor[,,1]) 
newdata_female = as.data.frame(HairEyeColor[,,2]) 
newdata = rbind(newdata_male, newdata_female)
newdata = newdata %>% 
  mutate(Gender = rep(c("Male", "Female"), each  = 16))

hairData = newdata %>% 
  group_by(Hair) %>%
  summarise(Freq = sum(Freq))

hairData

# Slide 17 ---------------------------------------------------------------------

hairData %>% 
  ggplot(aes(x=Hair, y=Freq)) + 
  geom_col(aes(color = Hair))

hairData %>% 
  ggplot(aes(x=Hair, y=Freq)) +  
  geom_col(aes(fill = Hair))

# Slide 18 ---------------------------------------------------------------------

ggplot(hairData, aes(x = Hair, y = Freq)) + 
  geom_col(aes(fill = Hair)) +
  scale_fill_manual(breaks = c("Black", "Brown", "Red", "Blond"), #<<
                    values=c("black", "brown", "red", "yellow")) #<<

# Slide 19 ---------------------------------------------------------------------

cbPalette <- c("#000000","#E69F00","#56B4E9","#009E73","#F0E442")

ggplot(hairData, aes(x = Hair, y = Freq)) + 
  geom_col(aes(fill = Hair)) +
  scale_fill_manual(values=cbPalette)

# Slide 28 ---------------------------------------------------------------------

x = -5:10
y = x^2
xy = data.frame(x,y)
ggplot(xy, aes(x = x, y=y)) +
  geom_point() +
  xlab("X") + ylab(expression(X^2))#<<

# Slide 29 ---------------------------------------------------------------------

ggplot(iris, aes(x=Sepal.Length, y=Petal.Length, group=Species)) + 
  geom_point(aes(color = Species, shape = Species)) + 
  xlab("Sepal Length") + 
  ylab("Patal Length") +
  ggtitle("Relation between Sepal Length and Petal Length") #<<


