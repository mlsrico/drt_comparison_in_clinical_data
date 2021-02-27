
## Clustering ## 

library(tidyverse)
library(parallelDist)
library(ggplot2)
library(cluster)
library(furrr)
library(ggthemes)
#library(mclust)
library(fpc)
library(clValid)
source("utilities/multiplot.R")

# Las funciones map() permiten aplicar funciones a todos los elementos
# de una columna. Al aplicarlo a un dataset anidado, lo que hago es aplicar
# una función a todos los datasets de una misma columna. 
# Con mutate lo que hago es crear un nuevo objeto en el que almacenar los resultados
# de estas funciones, o indicar que modifique una columna ya existente.
#
# Las funciones con future_ delante hacen lo mismo que map, solo que permiten el 
# procesamiento en paralelo. 

k <- 11 ## DEFINIR DE ANTEMANO

# Data -----

comp <- readRDS("analysis/dim reduction/resdat/dimred_data_11f.rds")

#comp <- comp0[6,]

dat <- comp %>% 
    mutate(m.dat = map(res.dat, function(df){
        df %>% dplyr::select(-clases, -method) %>% as.matrix()
    })) #crea otra columna: data como matriz sin clases 


# Hclust ---- 


ndat <- dat %>% 
    mutate(hcl.model = map(m.dat, function(x){
        d <- parallelDist::parDist(x, method = "euclidean") #calculo de distancia euclidea
        pfit <- hclust(d, method = "average") #clustering con metodo average
        return(pfit)
    }))

saveRDS(ndat, "analysis/clustering/resdata_out/dpfit_11f.rds")

# Clusters ----

ndat <- ndat %>% 
    mutate(data_cl = map2(.x = res.dat, .y = hcl.model, 
                          .f = function(x, y){
                              groups <- cutree(y, k = k) #cortamos en 3 clusters
                              df <- as.data.frame(x)
                              df$cluster <- as.factor(as.character(groups)) #guardamos los clusters en el df
                              return(df)})) 

saveRDS(ndat, "analysis/clustering/resdata_out/groups_11f.rds")

## Silhouette ---- 


ndat <- ndat %>% 
    mutate(sil = map2(.x = m.dat, .y = hcl.model, 
                             .f = function(x, y){
                                 d <- parallelDist::parDist(x, method = "euclidean") #distancia euclidea
                                 sil <- summary(silhouette(cutree(y, k = k), d)) #calculo de silueta
                                 return(sil$avg.width) #guardamos la media de la silueta para cada df
                             })) %>% 
    mutate(sil = as.numeric(sil)) # como numérico


saveRDS(ndat, "analysis/clustering/resdata_out/hclust_sil_11f.rds")


## Rand index ----


ndat <- ndat %>% 
    mutate(rand = map2(data_cl, id,  function(df,i){
        df <- df %>% mutate(clases = as.character(clases), 
                            cluster = as.character(cluster))
        res <- mclust::adjustedRandIndex(df$clases, df$cluster)
        print(i)
        return(res)
    })) %>% 
    mutate(rand = as.numeric(rand))


saveRDS(ndat, "analysis/clustering/resdata_out/hclust_sil_rand_11f.rds")

#adjustedRandIndex(test$uno, test$dos)


## Calinski-Harbaraz Index ----

ndat <- ndat %>% 
    mutate(calhar = map2(.x = m.dat, .y = hcl.model, function(x, y){
        res <- calinhara(x = x, clustering = cutree(y, k = k))
        return(res)
    })) %>% 
    mutate(calhar = as.numeric(calhar))

saveRDS(ndat, "analysis/clustering/resdata_out/hclust_sil_rand_ch_11f.rds")


## Dunn index ----

#plan(multiprocess)
ndat <- ndat %>% 
    mutate(dunni = map2(.x = m.dat, .y = hcl.model, function(x, y){
        res <- dunn(clusters = cutree(y, k = k), Data = x, method = y$dist.method)
        return(res)
    })) %>% 
    mutate(dunni = as.numeric(dunni))

saveRDS(ndat, "analysis/clustering/resdata/hclust_allind_11f.rds")



selincase <- ndat %>% dplyr::select(id, method, sil, rand, calhar, dunni)
#saveRDS(selincase, "analysis/clustering/resdata/hclust_allind_11f_sel.rds")

dir1 <- c("data/replicas/11f/11f")
dir2 <- c(".rds")
sdchar <- as.character(sdnum)

saveRDS(selincase, paste(dir1, paste(sdchar, dir2, sep = ""), sep = "_"))


