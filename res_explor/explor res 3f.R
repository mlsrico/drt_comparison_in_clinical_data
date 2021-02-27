


## Exploration ##

library("tidyverse")
library("ggplot2")
library("ggthemes")
#library("multcomp")
source("utilities/multiplot.R")


# Data ----
complete <- readRDS("analysis/simulation/resdat/idcodes.rds") 
dat_c <- complete %>% select(id,  nsize, nitems, sim, rango)

res <- readRDS("analysis/clustering/resdata/hclust_sil_rand_3f.rds")
res_c <- res %>% select("id", "method", "sil", "rand", "data_cl")

dat <- merge(res_c, dat_c, by = "id") %>% 
    mutate(idm = paste(id, method, sep = "_")) %>% 
    select(idm, id, method, sil, rand, nsize, nitems, sim, rango, data_cl) %>% 
    mutate_at(vars(nsize, nitems, rango, method), ~as.factor(as.character(.))) %>% 
    mutate(sim = map(sim, function(x){
        paste(x[1], paste(x[2], x[3], sep = "_"), sep = "_") 
    })) %>% 
    mutate(sim = as.character(sim))

# Sil*Method ----

## plot
ggplot(dat, aes(x = method, y = sil)) +
    geom_boxplot(alpha = 0.5) + 
    labs(y = "Average Silhouette Coefficient", x = "") +
    theme_bw()

##anova 
res.aov <- aov(sil ~ method, data = dat)
summary(res.aov)

summary(glht(res.aov, linfct = mcp(method = "Tukey")))


# Rand*Method ----

## plot
ggplot(dat, aes(x = method, y = rand)) +
    geom_boxplot(alpha = 0.5) + 
    labs(y = "Rand index", x = "") +
    theme_bw()

##anova 
res.aov <- aov(sil ~ method, data = dat)
summary(res.aov)

summary(glht(res.aov, linfct = mcp(method = "Tukey")))




# Sil*Rand ----

## N size
ggplot(dat, aes(x = sil, y = rand, color = method)) +
    geom_point() + 
    labs(title = "Relationship between Rand Index and Silhouette Coefficient", 
         subtitle = "By method and N size") +
    facet_grid(nsize~.) +
    theme_bw()

## num items
ggplot(dat, aes(x = sil, y = rand, color = method)) +
    geom_point() + 
    labs(title = "Relationship between Rand Index and Silhouette Coefficient", 
         subtitle = "By method and number of items") +
    facet_grid(nitems~.) +
    theme_bw()

## Sim
ggplot(dat, aes(x = sil, y = rand, color = method)) +
    geom_point() + 
    labs(title = "Relationship between Rand Index and Silhouette Coefficient", 
         subtitle = "By method and P") +
    facet_grid(sim~.) +
    theme_bw()

## Rango
ggplot(dat, aes(x = sil, y = rand, color = method)) +
    geom_point() + 
    labs(title = "Relationship between Rand Index and Silhouette Coefficient", 
         subtitle = "By method and min <Rango>") +
    facet_grid(rango~.) +
    theme_bw()

# Sil distr ---- 

## N size
ggplot(dat, aes(x = sil, fill = method, color = method)) +
    geom_density(alpha = 0.5) + 
    labs(title = "Silhouette Coefficient Distribution", 
         subtitle = "By method and N size") +
    facet_grid(nsize~.) +
    theme_bw()

## num items
ggplot(dat, aes(x = sil, fill = method, color = method)) +
    geom_density(alpha = 0.5) + 
    labs(title = "Silhouette Coefficient Distribution", 
         subtitle = "By method and number of items") +
    facet_grid(nitems~.) +
    theme_bw()

## Sim
ggplot(dat, aes(x = sil, fill = method, color = method)) +
    geom_density(alpha = 0.5) + 
    labs(title = "Silhouette Coefficient Distribution", 
         subtitle = "By method and P") +
    facet_grid(sim~.) +
    theme_bw()

## Rango
ggplot(dat, aes(x = sil, fill = method, color = method)) +
    geom_density(alpha = 0.5) + 
    labs(title = "Silhouette Coefficient Distribution", 
         subtitle = "By method and min <Rango>") +
    facet_grid(rango~.) +
    theme_bw()


# rand distr ---- 

## N size
ggplot(dat, aes(x = rand, fill = method, color = method)) +
    geom_density(alpha = 0.5) + 
    labs(title = "Rand Index Distribution", 
         subtitle = "By method and N size") +
    facet_grid(nsize~.) +
    theme_bw()

## num items
ggplot(dat, aes(x = rand, color = method, fill = method)) +
    geom_density(alpha = 0.5) + 
    labs(title = "Rand Index Distribution", 
         subtitle = "By method and number of items") +
    facet_grid(nitems~.) +
    theme_bw()

## Sim
ggplot(dat, aes(x = rand, fill = method, color = method)) +
    geom_density(alpha = 0.5) + 
    labs(title = "Rand Index Distribution", 
         subtitle = "By method and P") +
    facet_grid(sim~.) +
    theme_bw()

## Rango
ggplot(dat, aes(x = rand, fill = method, color = method)) +
    geom_density(alpha = 0.5) + 
    labs(title = "Rand Index Distribution", 
         subtitle = "By method and min <Rango>") +
    facet_grid(rango~.) +
    theme_bw()