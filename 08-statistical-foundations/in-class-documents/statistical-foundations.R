## ----echo=FALSE, message=FALSE, warning = FALSE-----------------------------
library(tidyverse)
library(knitr)
library(RColorBrewer)
library(mosaic)
library(infer)

## ---------------------------------------------------------------------------
library(nycflights13)
Chicago <- flights %>%
  filter(dest %in% c('ORD', 'MDW'), !is.na(arr_delay))

mean(Chicago$arr_delay) # population value


## ---------------------------------------------------------------------------
Chicago %>% mutate(less105 = arr_delay<=105) %>% 
  group_by(less105) %>% 
  count() %>%
  mutate(pct = n / nrow(Chicago))


## ---------------------------------------------------------------------------
set.seed(365)
Sample100 <- Chicago %>% sample_n(size=100)


## ---------------------------------------------------------------------------
Sample100 %>% summarize(min=min(arr_delay),
                        q05=quantile(arr_delay, 0.05),
                        mean=mean(arr_delay),
                        median=median(arr_delay),
                        max=max(arr_delay), 
                        q95=quantile(arr_delay, 0.95),
                        sd=sd(arr_delay))


## ---------------------------------------------------------------------------
set.seed(10)
Sample100_2 <- Chicago %>% sample_n(size=100)

Sample100_2 %>% summarize(mean=mean(arr_delay),
            min=min(arr_delay),
            max=max(arr_delay), 
            sd=sd(arr_delay))


## ---------------------------------------------------------------------------
n <- 100
num_trials <- 500
chi_25_means <- 1:num_trials %>% 
    map_dfr(~Chicago %>% slice_sample(n = n) %>% 
        summarize(mean_arr_delay = mean(arr_delay))) %>% 
  mutate(n = n)

head(chi_25_means)


## ---------------------------------------------------------------------------
favstats(~mean_arr_delay, data=chi_25_means) #mosaic library


## ----fig.height=4.5, fig.width=8, fig.align='center'------------------------
ggplot(chi_25_means, aes(x=mean_arr_delay)) + 
  geom_density(fill='turquoise', alpha=0.5) + 
  labs(x='Approximate Sampling Distribution of Sample Mean')


## ---------------------------------------------------------------------------
favstats(~arr_delay, data=Sample100)


## ---------------------------------------------------------------------------
favstats(~mean_arr_delay, data=chi_25_means)


## ----means, message=FALSE, warning=FALSE, cache=TRUE------------------------
Means50 <- do(1000)*(Chicago %>% sample_n(size=50) %>% 
                       summarize(mean=mean(arr_delay)))
Means100 <- do(1000)*(Chicago %>% sample_n(size=100) %>% 
                        summarize(mean=mean(arr_delay)))
Means500 <- do(1000)*(Chicago %>% sample_n(size=500) %>% 
                        summarize(mean=mean(arr_delay)))


## ----message=FALSE, warning=FALSE, fig.align='center', fig.height=4, fig.width=8, echo=FALSE----
Means <- rbind(Means50 %>% mutate(n=50),
               Means100 %>% mutate(n=100),
               Means500 %>% mutate(n=500))


## ----message=FALSE, warning=FALSE, fig.align='center', fig.height=4, fig.width=8, echo=FALSE----
ggplot(dat=Means, aes(x=mean)) + 
  geom_density(aes(fill=as.factor(n))) + 
  facet_grid(~n)+xlab('Sample means') + guides(fill=FALSE)


## ---------------------------------------------------------------------------
f3 <- Chicago %>% sample_n(size=3) %>% dplyr::select(year,month,day)


## ----echo=FALSE-------------------------------------------------------------
f3 


## ---------------------------------------------------------------------------
f3 %>% slice_sample(n= 3, 
               replace = TRUE)


## ---------------------------------------------------------------------------
f3 %>% slice_sample(n= 3, 
               replace = TRUE)


## ----fig.height=4.5, fig.width=8, fig.align='center'------------------------
Bootstrap_Means <- Sample100 %>% 
  specify(response = arr_delay) %>%
  generate(reps = 500, type = "bootstrap") %>%
  calculate(stat = "mean")

ggplot(Bootstrap_Means, aes(x=stat))+
  geom_density(fill='turquoise', alpha=0.5)+labs(x='Bootstrap Means')



## ---------------------------------------------------------------------------
favstats(~stat, data=Bootstrap_Means)


## ---------------------------------------------------------------------------
favstats(~mean_arr_delay, data=chi_25_means)


## ----echo = FALSE, fig.align='center', fig.width=10, fig.height=5-----------
set.seed(4)
patriots_sim = do(10000)*nflip(100)

ggplot(patriots_sim) + 
  geom_histogram(aes(x=nflip), binwidth=1)


## ----echo = FALSE, fig.align='center', fig.width=10, fig.height=5-----------
#sum(patriots_sim >= 19)

patriots_sim <- patriots_sim %>% mutate(pvalue = ifelse(nflip < 62, "less", "greater"))

ggplot(patriots_sim) + 
  geom_histogram(aes(x=nflip, fill = pvalue), binwidth=1) +
  geom_vline(xintercept = 61.5) + 
  geom_text(x=64, y=500, label='P(>= 62 Wins) = 0.0111', colour='black', size=5) + theme(legend.position="none")



