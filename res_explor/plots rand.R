

## Plots rand ## 

library("tidyverse")
library("sjPlot")
source("utilities/multiplot.R")


# Data ----
complete <- readRDS("analysis/simulation/resdat/idcodes.rds") 
dat_c <- complete %>% select(id,  nsize, nitems, sim, rango)

res <- readRDS("analysis/clustering/resdata/hclust_sil_rand_3f.rds")
res_c <- res %>% select("id", "method", "sil", "rand")

dat <- merge(res_c, dat_c, by = "id") %>% 
    mutate(idm = paste(id, method, sep = "_")) %>% 
    select(idm, id, method, rand, rand, nsize, nitems, sim, rango) %>% 
    mutate_at(vars(nsize, nitems, rango, method), ~as.factor(as.character(.))) %>% 
    mutate(sim = map(sim, function(x){
        paste(x[1], paste(x[2], x[3], sep = "_"), sep = "_") 
    })) %>% 
    mutate(sim = as.character(sim))


vars <- dat %>% select(nsize, nitems, sim, rango)

## rand ----
plrand <- list()
plrand[[1]] <- ggplot(dat) +
        geom_density(aes(x = rand, fill = vars[,1], color = vars[,1]), alpha = 0.5) +
        facet_grid(method ~.) +
        scale_fill_viridis_d(name = colnames(vars)[1]) +
        scale_color_viridis_d(name = colnames(vars)[1]) +
        theme_bw()
plrand[[2]] <- ggplot(dat) +
    geom_density(aes(x = rand, fill = vars[,2], color = vars[,2]), alpha = 0.5) +
    facet_grid(method ~.) +
    scale_fill_viridis_d(name = colnames(vars)[2]) +
    scale_color_viridis_d(name = colnames(vars)[2]) +
    theme_bw()
plrand[[3]] <- ggplot(dat) +
    geom_density(aes(x = rand, fill = vars[,3], color = vars[,3]), alpha = 0.5) +
    facet_grid(method ~.) +
    scale_fill_viridis_d(name = colnames(vars)[3]) +
    scale_color_viridis_d(name = colnames(vars)[3]) +
    theme_bw()

plrand[[4]] <- ggplot(dat) +
    geom_density(aes(x = rand, fill = vars[,4], color = vars[,4]), alpha = 0.5) +
    facet_grid(method ~.) +
    scale_fill_viridis_d(name = colnames(vars)[4]) +
    scale_color_viridis_d(name = colnames(vars)[4]) +
    theme_bw()


multiplot(plrand[[1]], plrand[[2]], plrand[[3]], plrand[[4]], cols = 2)
