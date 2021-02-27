

## Table 1 ## 

library("tidyverse")
library("sjPlot")


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
    mutate(sim = as.character(sim))


# Table 0 ---- 

dat %>% group_by(method) %>% 
    summarise(min_sil = round(min(sil), 1), 
              mean_sil = round(mean(sil), 2), 
              sd_sil = round(sd(sil), 2),
              max_sil = round(max(sil), 1), 
              min_rand = round(min(rand), 1), 
              mean_rand = round(mean(rand), 2), 
              sd_rand = round(sd(rand), 2),
              max_rand = round(max(rand), 1)) 


# Table 1 ----

v <- which(colnames(dat) %in% c("method", "nsize", "nitems", "sim", "rango"))

lista <- list()
for(i in v){
    lista[[v]] <- dat %>% 
        group_by(dat[,v]) %>% 
        summarise(mean_sd_sil = paste(round(mean(sil), 2), 
                                      paste(round(sd(sil), 2), ")", sep = ""), 
                                      sep = " ("), 
                  mean_sd_rand = paste(round(mean(rand), 2), 
                                       paste(round(sd(rand), 2), ")", sep = ""), 
                                       sep = " ("))
    l <- compact(lista)
} ## Por alguna razón solo funciona a mano. 



# Table 2 ---- 

rbind(dat %>% filter(method == "pca") %>% 
          group_by(nsize) %>% 
          summarise(mean_sd_sil = paste(round(mean(sil), 2), 
                                        paste(round(sd(sil), 2), ")", sep = ""), 
                                        sep = " ("), 
                    mean_sd_rand = paste(round(mean(rand), 2), 
                                         paste(round(sd(rand), 2), ")", sep = ""), 
                                         sep = " (")) %>%
          select(-nsize), 
      dat %>% filter(method == "pca") %>% 
          group_by(nitems) %>% 
          summarise(mean_sd_sil = paste(round(mean(sil), 2), 
                                        paste(round(sd(sil), 2), ")", sep = ""), 
                                        sep = " ("), 
                    mean_sd_rand = paste(round(mean(rand), 2), 
                                         paste(round(sd(rand), 2), ")", sep = ""), 
                                         sep = " (")) %>%
          select(-nitems), 
      dat %>% filter(method == "pca") %>% 
          group_by(sim) %>% 
          summarise(mean_sd_sil = paste(round(mean(sil), 2), 
                                        paste(round(sd(sil), 2), ")", sep = ""), 
                                        sep = " ("), 
                    mean_sd_rand = paste(round(mean(rand), 2), 
                                         paste(round(sd(rand), 2), ")", sep = ""), 
                                         sep = " (")) %>%
          select(-sim))





























