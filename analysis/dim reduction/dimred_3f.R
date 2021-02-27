
# Dimensionality reduction #

library(tidyverse)
library(psych)
library(GPArotation)
library(factoextra)
library(Rtsne)
library(ggplot2)
library(umap)
library(furrr)

# Data ----

set.seed(sdnum)
sel <- sample(x = 1:60, size = 5); sel

comp <- readRDS("analysis/simulation/resdat/ex_sim_data_3f.rds")
#comp <- comp[c(sel),]

n.ex <- comp %>% 
    dplyr::select(id, res.data) %>%
    mutate(res.mat = map(res.data, function(df){
        df %>% 
            dplyr::select(-clases) %>% 
            as.matrix() #transformo los datos a matriz y le quito la variable clases
    }))





# PCA ----

## pca res
n.ex <- n.ex %>% 
    mutate(classes.pca = map(res.mat, function(m){
        prcomp(m, #matrix dat sin clases
               scale = TRUE, rank. = 2) #aplicamos pca a 2 dim
    })) 

#saveRDS(n.ex, "analysis/dim reduction/resdat_out/ex_pcad_data_3f.rds")

## pca df
n.ex <- n.ex %>% 
    mutate(res.dat.pca = map2(.x = res.data, .y = classes.pca, function(df, r){
        res <- r$x %>% #extraemos dimensiones
            as.data.frame() %>% #transformamos a df
            mutate(clases = df$clases) %>% #incluimos la variable clases otra vez
            mutate(method = rep("pca", times = dim(df)[1])) #indicamos el metodo
        colnames(res) <- c("V1", "V2", "clases", "method") #para que tenga el mismo nombre que tsne y umap (comodidad)
        return(res)
    }))

## plot pca
#n.ex <- n.ex %>% 
#    mutate(res.plot.pca = map2(.x = res.dat.pca, .y = data, function(df, y){
#        N <- paste("N =", y$nsize)
#        n <- paste("n =", y$nitems)
#        P <- paste("P =", y$sim)
#        c <- paste("col =", paste(round(y$col), "%", sep = ""))
#        ggplot(df, aes(x = V1, y = V2, color = clases)) + 
#            geom_point() + #scatter plot
#            labs(title = paste(N, n, sep = "; "), 
#                 subtitle = paste(P, c, sep = "; ")) + ## labs con argumentos
#            theme_minimal()
#    }))

#saveRDS(n.ex, "analysis/dim reduction/resdat_out/ex_pcad_data_3f.rds")


# t-SNE ----

## tsne res 
#set.seed(2523)
plan(multiprocess)

n.ex <- n.ex %>% 
    mutate(res.dat.tsne = future_map2(.x = res.mat, .y = res.data, function(x, y){
        resdim <- Rtsne(x, #matrix dat sin clases
                        dims = 2, #aplicamos tsne a dos dimensiones
                        perplexity = 50, 
                        max_iter = 5000, 
                        pca = F, #sin que haga un pca inicial
                        #initial_dims = 2, 
                        check_duplicates = F) #ignorar duplicados (como variables vacías)-argumento que causaba problemas
        res <- resdim$Y %>% #extraemos dimensiones
            as.data.frame() %>% #transformamos a df
            mutate(clases = y$clases) %>% #incluimos la variable clases otra vez
            mutate(method = rep("tsne", times = dim(y)[1])) #indicamos el metodo
    }, .progress = T))






#n.ex <- n.ex %>% 
#    mutate(classes.tsne = future_map(res.mat, function(m){
#        Rtsne(m, #matrix dat sin clases
#              dims = 2, #aplicamos tsne a dos dimensiones
#              perplexity = 50, 
#              max_iter = 5000, 
#              pca = F, #sin que haga un pca inicial
#              #initial_dims = 2, 
#              check_duplicates = F) #ignorar duplicados (como variables vacías)-argumento que causaba problemas
#        
#    }, .progress = T)) 


#n.ex$classes.tsne <- lista1
#saveRDS(n.ex, "analysis/dim reduction/resdat_out/ex_tsned_data_3f.rds")

## tsne df
#n.ex <- n.ex %>% 
#    mutate(res.dat.tsne = map2(.x = res.data, .y = classes.tsne, function(df, r){
#        res <- r$Y %>% #extraemos dimensiones
#            as.data.frame() %>% #transformamos a df
#            mutate(clases = df$clases) %>% #incluimos la variable clases otra vez
#            mutate(method = rep("tsne", times = dim(df)[1])) #indicamos el metodo
#        return(res)
#    }))

## plot tsne
#n.ex <- n.ex %>% 
#    mutate(res.plot.tsne = map2(.x = res.dat.tsne, .y = data, function(df, y){
#        N <- paste("N =", y$nsize)
#        n <- paste("n =", y$nitems)
#        P <- paste("P =", y$sim)
#        c <- paste("col =", paste(round(y$col), "%", sep = ""))
#        ggplot(df, aes(x = V1, y = V2, color = clases)) + 
#            geom_point() + #scatter plot
#            labs(title = paste(N, n, sep = "; "), 
#                 subtitle = paste(P, c, sep = "; ")) + ## labs con argumentos
#            theme_minimal()
#    }))

#saveRDS(n.ex, "analysis/dim reduction/resdat_out/ex_tsned_data_3f.rds")

# UMAP ---- 
#set.seed(2523)

plan(multiprocess)

n.ex <- n.ex %>% 
    mutate(res.dat.umap = future_map2(.x = res.mat, .y = res.data, .f = function(x, y){
        dimred <- umap(d = x, #matriz dat sin clases
                    n_components = 2) #umap a 2 dim
        res <- dimred$layout %>% #extraemos las dimeniones
            as.data.frame() %>%  #transformamos a df
            mutate(clases = y$clases) %>% #incluimos la variable clases
            mutate(method = rep("umap", times = dim(y)[1])) #indicamos el metodo
        return(res)
    }, .progress = T))



## umap res
#plan(multiprocess)
#n.ex <- n.ex %>% 
#    mutate(classes.umap = future_map(res.mat, function(m){
#        umap(d = m, #matriz dat sin clases
#             n_components = 2) #umap a 2 dim
#    }, .progress = T)) 

#saveRDS(n.ex, "analysis/dim reduction/resdat_out/ex_umapd_data_3f.rds")

## umap df

#n.ex <- n.ex %>% 
#    mutate(res.dat.umap = map2(.x = res.data, .y = classes.umap, function(df, r){
#        res <- r$layout %>% #extraemos las dimeniones
#            as.data.frame() %>%  #transformamos a df
#            mutate(clases = df$clases) %>% #incluimos la variable clases
#            mutate(method = rep("umap", times = dim(df)[1])) #indicamos el metodo
#        return(res)
#    }))


## umap plot

#n.ex <- n.ex %>% 
#    mutate(res.plot.umap = map2(.x = res.dat.umap, .y = data, function(df, y){
#        N <- paste("N =", y$nsize)
#        n <- paste("n =", y$nitems)
#        P <- paste("P =", y$sim)
#        c <- paste("col =", paste(round(y$col), "%", sep = ""))
#        ggplot(df, aes(x = V1, y = V2, color = clases)) + 
#            geom_point() + #scatter plot
#            labs(title = paste(N, n, sep = "; "), 
#                 subtitle = paste(P, c, sep = "; ")) + ## labs con argumentos
#            theme_minimal()
#    }))




# Save data ----

## All
#saveRDS(n.ex, "analysis/dim reduction/resdat/allres_dimred_3f.rds")



## dimred
nombres <- c("id", "res.dat", "method")

n.pca <- n.ex %>% dplyr::select(id, res.dat.pca) 
n.pca$method <- rep("pca", times = dim(n.ex)[1])
colnames(n.pca) <- nombres

n.tsne <- n.ex %>% dplyr::select(id, res.dat.tsne) 
n.tsne$method <- rep("tsne", times = dim(n.ex)[1])
colnames(n.tsne) <- nombres

n.umap <- n.ex %>% dplyr::select(id, res.dat.umap) 
n.umap$method <- rep("umap", times = dim(n.ex)[1])
colnames(n.umap) <- nombres


all.dat <- rbind(n.pca, n.tsne, n.umap) %>% dplyr::select(id, method, res.dat)




saveRDS(all.dat, "analysis/dim reduction/resdat/dimred_data_3f.rds")










