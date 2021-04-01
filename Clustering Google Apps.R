library(tidyverse)
library(ca)
library(factoextra)
library(cluster)
library(igraph)

data = read.csv2("dataset/Google Play Store Apps/googleplaystore.csv", sep=",")
data %>% head


category_important = data %>% group_by(Category) %>% 
                              summarise (n = n()) %>% 
                              arrange(desc(n)) %>% 
                              mutate (per = round(cumsum(n)/sum(n),2)) %>%
                              filter(per<0.7)
category_important %>% head
contingency_table = data %>% 
          filter(Category == category_important$Category) %>% 
          select(Category, Content.Rating) %>%
          mutate(n=1) %>%
          group_by(Category, Content.Rating) %>%
          summarise(sum = sum(n)) %>%
          spread(Content.Rating, sum)

dplyr_if_else      <- function(x) { mutate_all(x, funs(if_else(is.na(.), 0, .))) }
contingency_table = dplyr_if_else (contingency_table)

contingency_table = data.frame(contingency_table)
rownames(contingency_table) = contingency_table$Category
contingency_table = contingency_table[,-1]
head(contingency_table)

chisq.test(contingency_table)

fviz_ca_biplot(ca(contingency_table), repel = TRUE, title="Comprehensive analysis of Contingency Table plot")

set.seed(123)
km.res <- kmeans(contingency_table, 3, 25)
fviz_cluster(km.res, data = contingency_table, palete=c("#2E9FDF","#00AFBB"), 
             ellipse.type="euclid", star.plot=TRUE, repel = TRUE, ggtheme = theme_minimal(),
             main="Perceptual Map of Category and Content Rating")

res.dist <- dist(contingency_table, method = "euclidean")
res.hc <- hclust( d = res.dist, method = "ward.D2")
fviz_dend(res.hc, k = 5, # Cut in four groups 
          k_colors = "jco", type = "phylogenic", 
          repel = TRUE, phylo_layout = "layout.gem")

set.seed(123)
km.res <- kmeans(contingency_table, 3, 25)
fviz_cluster(km.res, data = contingency_table, palete=c("#2E9FDF","#00AFBB"), 
             ellipse.type="convex", star.plot=TRUE, repel = TRUE, ggtheme = theme_minimal(),
             main="Perceptual Map of Category and Content Rating")

fviz_dend( res.hc, k = 3,  # Cut in four groups 
           cex = 0.5, # label size 
           k_colors = c("#2E9FDF", "#00AFBB", "#E7B800"), 
           color_labels_by_k = TRUE, # color labels by groups 
           rect = TRUE # Add rectangle around groups
           )
           
fviz_dend( res.hc, cex = 0.5, k = 3, k_colors = "jco", type = "circular")

fviz_dend( res.hc, k = 3, k_colors = "jco", type = "phylogenic", repel = TRUE, phylo_layout = "layout_with_drl")

fviz_dend( res.hc, k = 3, k_colors = "jco", type = "phylogenic", repel = TRUE, phylo_layout = "layout.mds")

fviz_dend( res.hc, k = 3, k_colors = "jco", type = "phylogenic", repel = TRUE, phylo_layout = "layout_with_lgl")

