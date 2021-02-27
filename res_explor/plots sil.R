

## Plots sil ## 

library("tidyverse")
library("sjPlot")
source("utilities/multiplot.R")


# Data ----
complete <- readRDS("analysis/simulation/resdat/idcodes.rds") 
dat_c <- complete %>% select(id,  nsize, nitems, sim, rango)

res <- readRDS("analysis/clustering/resdata/hclust_allind_3f.rds")
res_c <- res %>% select("id", "method", "sil", "rand", "calhar", "dunni")

dat <- merge(res_c, dat_c, by = "id") %>% 
    mutate(idm = paste(id, method, sep = "_")) %>% 
    select(idm, id, method, sil, rand, calhar, dunni, nsize, nitems, sim, rango) %>% 
    mutate_at(vars(nsize, nitems, rango, method), ~as.factor(as.character(.))) %>% 
    mutate(sim = map(sim, function(x){
        paste(x[1], paste(x[2], x[3], sep = "_"), sep = "_") 
    })) %>% 
    mutate(sim = as.character(sim)) %>% 
    filter(nitems != 20)


vars <- dat %>% select(nsize, nitems, sim)

## Sil ----
plsil <- list()
plsil[[1]] <- ggplot(dat) +
        geom_density(aes(x = sil, fill = vars[,1], color = vars[,1]), alpha = 0.5) +
        facet_grid(method ~., scales = "free_y") +
        scale_fill_viridis_d(name = colnames(vars)[1]) +
        scale_color_viridis_d(name = colnames(vars)[1]) +
        theme_bw()
plsil[[2]] <- ggplot(dat) +
    geom_density(aes(x = sil, fill = vars[,2], color = vars[,2]), alpha = 0.5) +
    facet_grid(method ~., scales = "free_y") +
    scale_fill_viridis_d(name = colnames(vars)[2]) +
    scale_color_viridis_d(name = colnames(vars)[2]) +
    theme_bw()
plsil[[3]] <- ggplot(dat) +
    geom_density(aes(x = sil, fill = vars[,3], color = vars[,3]), alpha = 0.5) +
    facet_grid(method ~., scales = "free_y") +
    scale_fill_viridis_d(name = colnames(vars)[3]) +
    scale_color_viridis_d(name = colnames(vars)[3]) +
    theme_bw()


multiplot(plsil[[1]], plsil[[2]], plsil[[3]], cols = 3)
