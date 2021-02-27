
## Exploring results ##


library("tidyverse")
library("ggplot2")
source("utilities/multiplot.R")


# Data ----
complete <- readRDS("analysis/simulation/resdat/idcodes.rds") 
dat_c <- complete %>% select("id",  "nsize", "nitems", "sim", "rango")

res <- readRDS("analysis/clustering/resdata/hclust_sil_3f.rds")
res_c <- res %>% select("id", "method", "sil", "data_cl")

dat <- merge(res_c, dat_c, by = "id") %>% 
    mutate(idm = paste(id, method, sep = "_")) %>% 
    select(idm, id, method, sil, nsize, nitems, sim, rango)


# Plot - Sil by method----

##  by ID
ggplot(dat, aes(x = id, y = sil, color = method)) +
    geom_point(alpha = 0.5) + geom_line() +
    labs(x = "Sample ID", y = "Average Silhouette Coefficient") +
    scale_color_hue(name = "Method", labels = c("PCA", "t-SNE", "UMAP")) +
    theme_bw()

## avg sil distr - density
ggplot(dat, aes(x = sil, color = method, fill = method)) +
    geom_density(alpha = 0.2) +
    labs(y = "Density", x = "Average Silhouette Coefficient") +
    scale_color_hue(name = "Method", labels = c("PCA", "t-SNE", "UMAP")) +
    scale_fill_hue(name = "Method", labels = c("PCA", "t-SNE", "UMAP")) +
    theme_bw()


## avg sil - density by collinearity 

ggplot(dat, aes(x = sil, color = method, fill = method)) +
    geom_density(alpha = 0.2) +
    labs(y = "Density", x = "Average Silhouette Coefficient") +
    scale_color_hue(name = "Method", labels = c("PCA", "t-SNE", "UMAP")) +
    scale_fill_hue(name = "Method", labels = c("PCA", "t-SNE", "UMAP")) +
    facet_grid(rango~.)+
    theme_bw()






# Table - highest values per group----

dat %>% group_by(method) %>% 
    summarise(max.sil = round(max(sil), 2), 
              median.sil = round(median(sil), 2),
              mean.sil = round(mean(sil), 2), 
              min.sil = round(min(sil), 2))

# Table - best id values----

t <- dat %>% 
    arrange(desc(sil)) %>%
    mutate(sil = round(sil, 2)) %>% 
    head(50)



# Plot - Bestones ---- 

dat$data_cl <- res_c$data_cl
thebest <- dat %>% filter(idm %in% t$idm) %>% select(idm, data_cl, sil) %>% 
    mutate(plts = pmap(.l = list(idm, data_cl, sil),
                       .f = function(x, y, z){
                           ggplot(y, aes(x = V1, y = V2, color = cluster)) + 
                               geom_point() + theme_minimal() +
                               scale_color_discrete(guide = "none") +
                               labs(subtitle = paste(x, round(z, 2), sep = ", "))
                       }))

multiplot(thebest$plts[[1]], thebest$plts[[2]], thebest$plts[[3]], 
          thebest$plts[[4]], thebest$plts[[5]], thebest$plts[[6]], 
          thebest$plts[[7]], thebest$plts[[8]], thebest$plts[[9]], 
          thebest$plts[[10]], cols = 5)


# Table - worst id values----

w <- dat %>% 
    select(-data_cl) %>% 
    arrange(sil) %>%
    mutate(sil = round(sil, 2)) %>% 
    head(50)



# Plot - worstones ---- 

theworst <- dat %>% filter(idm %in% w$idm) %>% select(idm, data_cl, sil) %>% 
    mutate(plts = pmap(.l = list(idm, data_cl, sil),
                       .f = function(x, y, z){
                           ggplot(y, aes(x = y[,1], y = y[,2], color = cluster)) + 
                               geom_point() + theme_minimal() +
                               scale_color_discrete(guide = "none") +
                               labs(subtitle = paste(x, round(z, 2), sep = ", "), 
                                    x = "V1", y = "V2")
                       }))

multiplot(theworst$plts[[1]], theworst$plts[[2]], theworst$plts[[3]], 
          theworst$plts[[4]], theworst$plts[[5]], theworst$plts[[6]], 
          theworst$plts[[7]], theworst$plts[[8]], theworst$plts[[9]], 
          theworst$plts[[10]], cols = 5)















