## ----echo=FALSE, message=FALSE, warning = FALSE---------------------------------------------------------------
library(tidyverse)


## ----echo=FALSE-----------------------------------------------------------------------------------------------
library(dslabs)
gapminder_2015 <- gapminder %>% filter(year == 2015)
glimpse(gapminder_2015)


## -------------------------------------------------------------------------------------------------------------
gap_fle <- gapminder_2015 %>%
  filter(continent == "Americas") %>%
  filter(fertility != 'NA') %>%
  select(fertility, life_expectancy)


k <- 4 #must define number of clusters
set.seed(4)
kmeans_clusters <- kmeans(gap_fle, centers = k, nstart = 10)


## ----fig.align='center', fig.width=12, fig.height=5, fig.alt="Scatterplot of Fertility vs Life expectancy where shape of each observation is related to their cluster assignment. The overall relationship is moderately negative, with the four clusters grouping observations from top to bottom."----

kmeans_clusters$cluster <- as.factor(kmeans_clusters$cluster)


ggplot(gap_fle, aes(x=fertility, y=life_expectancy)) + 
  geom_point(aes(color=kmeans_clusters$cluster, pch=kmeans_clusters$cluster)) +
  labs(y = "Life Expectancy", x = "Fertility",
       color = "Cluster", shape = "Cluster")


## ----fig.width=12, fig.height=4.5, fig.align='center', fig.alt="Same scatterplot of Fertility vs Life expectancy as the last slide, but now there are only three clusters. The bottom cluster remains the same, but the top three clusters combine into two."----

k <- 3
kmeans_clusters <- kmeans(gap_fle, centers=k, nstart = 10)
kmeans_clusters$cluster <- as.factor(kmeans_clusters$cluster)

ggplot(gap_fle, aes(x=fertility, y=life_expectancy)) + 
  geom_point(aes(color=kmeans_clusters$cluster, pch=kmeans_clusters$cluster), size = 4) +
  labs(y = "Life Expectancy", x = "Fertility",
       color = "Cluster", shape = "Cluster")


## -------------------------------------------------------------------------------------------------------------
gap_fle <- gapminder_2015 %>%
  filter(continent == "Americas") %>%
  select(fertility, life_expectancy, country)

rownames(gap_fle) <- gap_fle$country

gap_dist <- dist(gap_fle[,1:2])


## ----echo=FALSE-----------------------------------------------------------------------------------------------
gap_dist_matrix <- gap_dist %>% as.matrix()
gap_dist_matrix[1:4, 1:4] #not needed for function - just to visualize distance matrix


## ----fig.width=12, fig.height=5, fig.align='center', fig.alt="Dendrogram for hierarchical cluster of countries using just fertility and life expectancy."----

clusters <- hclust(gap_dist, method = "complete")
plot(clusters, hang = -1, cex = 0.75)

## -------------------------------------------------------------------------------------------------------------
clus4 = cutree(clusters, 4)



## ----warning = FALSE, fig.width=13, fig.height=5, fig.align='center', echo = FALSE, fig.alt="Same scatterplot of Fertility vs Life expectancy as previous, but with only four clusters from hierarchcial clustering. The cluster pattern is similar to k-means with four clusters"----
gap_clusters <- cbind(gap_fle, clus4)

gap_clusters %>% 
  ggplot(aes(x = fertility, y = life_expectancy, color = as.factor(clus4), label=country)) + 
  geom_point(size = 3) + geom_text(hjust=0, vjust=0, size = 5) +
  labs(y = "Life Expectancy", x = "Fertility",
       color = "Cluster")


## -------------------------------------------------------------------------------------------------------------
data_scale = gapminder_2015 %>%
  filter(continent == "Americas") %>%
  filter(infant_mortality != 'NA') %>%
  select(infant_mortality, life_expectancy) %>%
  scale() %>% #<<
  as.data.frame()


## -------------------------------------------------------------------------------------------------------------

k <- 5
set.seed(4)
kmeans_clusters <- kmeans(data_scale, centers=k, nstart=10)

kmeans_clusters$cluster <- as.factor(kmeans_clusters$cluster)

ggplot(data_scale, aes(x=infant_mortality, y=life_expectancy)) + geom_point() + 
  aes(color=kmeans_clusters$cluster, pch=kmeans_clusters$cluster) +
  labs(y = "Life Expectancy", x = "Infant Mortality",
       color = "Cluster", shape = "Cluster")

