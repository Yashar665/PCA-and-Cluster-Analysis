# Import libraries & dataset ----

library(tidyverse)
library(h2o)
library(glue)
library(highcharter)
library(skimr)
df <- read_csv('C:/Users/Yashar/Desktop/Data Science Bootcamp/R programming/Week 9/CC GENERAL.csv')

# EDA ----

df %>% glimpse()
df %>% skim()
df %>% summary()
df <- df %>% drop_na()

# PCA with h2o ----

h2o.init()
df.h2o <- df %>% as.h2o()

model_pca <- df.h2o %>% 
  h2o.prcomp(transform = "STANDARDIZE",
             k = 4, 
             impute_missing = T,
             max_runtime_secs = 90)

model_pca %>% 
  h2o.predict(df.h2o) %>% 
  as_tibble() -> pca_pred


model_pca@model$model_summary %>% as.data.frame() -> table
table

table[2,] %>% 
  gather() %>% 
  hchart("bar", hcaes(x = key, y = value)) %>%
  hc_colors(colors = 'blue') %>%
  hc_xAxis(visible=T) %>%
  hc_yAxis(visible=T)


# Optimal Number of Clusters ----
pca_pred %>% 
  fviz_nbclust(kmeans, method = "wss") +
  labs(subtitle = "Elbow method")
# Elbow method: 7 clusters solution suggested

pca_pred %>% 
  fviz_nbclust(kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")
# Silhouette method: 3 clusters solution suggested



# Fitting K-Means to the data ----
set.seed(123)
kmeans <- pca_pred %>% kmeans(centers = 2)
y_kmeans <- kmeans$cluster %>% as_factor()

pca_pred %>% clusplot(y_kmeans,
                shade = TRUE,
                color = TRUE,
                labels = 2,
                plotchar = F,
                main = 'Clusters of customers')


# Visualize the result ----

g <- iris %>% 
  ggplot(aes(Sepal.Length,Petal.Length,
             color = y_kmeans)) +
  geom_point(aes(text = Species),size = 2) + 
  #facet_wrap(~ Species) +
  scale_x_continuous(breaks = seq(0,150,10)) +
  scale_y_continuous(breaks = seq(0,150,10)) +
  labs(x="Sepal Length", 
       y="Petal Length",
       title="Iris",
       subtitle="4 clusters")

g %>% ggplotly(tooltip = "text")
