

## ANOVA ##


library("tidyverse")
library("sjPlot")
library("DescTools")
source("utilities/multiplot.R")
source("utilities/PlotAnova.R")


# Data ----
complete <- readRDS("analysis/simulation/resdat/idcodes.rds") 
dat_c <- complete %>% select(id,  nsize, nitems, sim, rango)

res <- readRDS("analysis/clustering/resdata/hclust_sil_rand_3f.rds")
res_c <- res %>% select("id", "method", "sil", "rand")

dat <- merge(res_c, dat_c, by = "id") %>%
    mutate(idm = paste(id, method, sep = "_")) %>% 
    select(idm, id, method, sil, rand, nsize, nitems, sim, rango) %>% 
    mutate_at(vars(nsize, nitems, rango, method), ~as.factor(as.character(.))) %>% 
    mutate(sim = map(sim, function(x){
        paste(x[1], paste(x[2], x[3], sep = "_"), sep = "_") 
    })) %>% 
    mutate(sim = as.character(sim)) %>% 
    filter(rango == "0") %>% filter(rand == 1)



Plot2WayANOVA(rand ~ method * nitems, dataframe = dat, plottype = "line")









