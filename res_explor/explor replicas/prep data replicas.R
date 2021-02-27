
## Explorando las replicas

library(tidyverse)

lost_method_index <- c(rep(c("pca"), 60), rep(c("tsne"), 60), rep(c("umap"), 60))

# 3f ----
f3 <- list.files(path = "data/replicas/3f/")#[c(1,50:60)]

list_3f <- list()

for(i in 1:length(f3)){
    dir <- paste("data/replicas/3f/", f3[i], sep = "")
    list_3f[[i]] <- readRDS(dir)
    many <- dim(list_3f[[i]])[1]
    list_3f[[i]] <- list_3f[[i]] %>% as.data.frame %>% mutate(seednum = rep(i, times = many))
}

names3f <- str_replace(f3, ".rds", "")
names(list_3f) <- str_replace(names3f, "3f", "f3")

list_3f <- map(list_3f, function(x) x %>% as.data.frame %>% mutate(num_fact = rep(3, times = dim(x)[1])))

# lost method in first list
list_3f$f3_1 <- list_3f$f3_1 %>% 
    mutate(method = lost_method_index) %>% 
    select(id, method, sil, rand, calhar, dunni, seednum, num_fact) 


df_3f <- list_3f[[1]]
for(i in 2:length(list_3f)){
    df_3f <- rbind(df_3f, list_3f[[i]])
}



# 5f ----
f5 <- list.files(path = "data/replicas/5f/")#[c(1,50:60)]

list_5f <- list()

for(i in 1:length(f5)){
    dir <- paste("data/replicas/5f/", f5[i], sep = "")
    list_5f[[i]] <- readRDS(dir)
    many <- dim(list_5f[[i]])[1]
    list_5f[[i]] <- list_5f[[i]] %>% as.data.frame %>% mutate(seednum = rep(i, times = many))
}

names5f <- str_replace(f5, ".rds", "")
names(list_5f) <- str_replace(names5f, "5f", "f5")

list_5f <- map(list_5f, function(x) x %>% as.data.frame %>% mutate(num_fact = rep(5, times = dim(x)[1])))

# lost method in first list
list_5f$f5_1 <- list_5f$f5_1 %>% 
    mutate(method = lost_method_index) %>% 
    select(id, method, sil, rand, calhar, dunni, seednum, num_fact) 

df_5f <- list_5f[[1]]
for(i in 2:length(list_5f)){
    df_5f <- rbind(df_5f, list_5f[[i]])
}


# 8f ----
f8 <- list.files(path = "data/replicas/8f/")#[c(1,50:60)]

list_8f <- list()

for(i in 1:length(f8)){
    dir <- paste("data/replicas/8f/", f8[i], sep = "")
    list_8f[[i]] <- readRDS(dir)
    many <- dim(list_8f[[i]])[1]
    list_8f[[i]] <- list_8f[[i]] %>% as.data.frame %>% mutate(seednum = rep(i, times = many))
}

names8f <- str_replace(f8, ".rds", "")
names(list_8f) <- str_replace(names8f, "8f", "f8")

list_8f <- map(list_8f, function(x) x %>% as.data.frame %>% mutate(num_fact = rep(8, times = dim(x)[1])))

# lost method in first list
list_8f$f8_1 <- list_8f$f8_1 %>% 
    mutate(method = lost_method_index) %>% 
    select(id, method, sil, rand, calhar, dunni, seednum, num_fact) 

df_8f <- list_8f[[1]]
for(i in 2:length(list_8f)){
    df_8f <- rbind(df_8f, list_8f[[i]])
}


# 11f ----
f11 <- list.files(path = "data/replicas/11f/")#[c(1,50:60)]

list_11f <- list()

for(i in 1:length(f11)){
    dir <- paste("data/replicas/11f/", f11[i], sep = "")
    list_11f[[i]] <- readRDS(dir)
    many <- dim(list_11f[[i]])[1]
    list_11f[[i]] <- list_11f[[i]] %>% as.data.frame %>% mutate(seednum = rep(i, times = many))
}

names11f <- str_replace(f11, ".rds", "")
names(list_11f) <- str_replace(names11f, "11f", "f11")

list_11f <- map(list_11f, function(x) x %>% as.data.frame %>% mutate(num_fact = rep(11, times = dim(x)[1])))

# lost method in first list
list_11f$f11_1 <- list_11f$f11_1 %>% 
    mutate(method = lost_method_index) %>% 
    select(id, method, sil, rand, calhar, dunni, seednum, num_fact) 

df_11f <- list_11f[[1]]
for(i in 2:length(list_11f)){
    df_11f <- rbind(df_11f, list_11f[[i]])
}


# Combine ----

todas <- do.call("rbind", list(df_3f, df_5f, df_8f, df_11f)) %>% 
    mutate(method = factor(method, levels = c("pca", "tsne", "umap"))) %>% 
    mutate(num_fact = factor(as.character(num_fact), levels = c("3", "5", "8", "11")))


# Integrate id values ---- 

idcodes <- readRDS("analysis/simulation/resdat/codes/idcodes5f.rds") ## same for everyone
# nsize / nitems / col

ds.comp <- todas %>% 
    mutate(nsize = map_dbl(id, function(x) idcodes$nsize[which(idcodes$id==x)])) %>% 
    mutate(nitems = map_dbl(id, function(x) idcodes$nitems[which(idcodes$id==x)])) %>% 
    mutate(col = map_dbl(id, function(x) idcodes$col[which(idcodes$id==x)])) %>% 
    mutate_at(vars(nsize, nitems, col), ~as.character(.)) %>% 
    mutate(nsize = factor(nsize, levels = c("1000", "5000", "10000"))) %>% 
    mutate(nitems = factor(nitems, levels = c("20", "100", "200", "500"))) %>% 
    mutate(col = factor(col, levels = c("0", "5", "10", "20", "25")))
dim(ds.comp)


saveRDS(ds.comp, "data/replicas/all/all_reps.rds")






ds.comp %>% group_by(method, num_fact) %>% summarise_at(vars(sil, rand, calhar, dunni), mean) 

ds.comp <- ds.comp %>% mutate(calhar = calhar/1000)
ggplot(ds.comp)+
    geom_boxplot(aes(x = num_fact, y = sil, fill = method), alpha = 0.5) +
    theme_bw()




ds.comp %>% group_by(method, num_fact) %>%
    summarise(min = min(sil), max = max(sil), 
              mean = mean(sil), sd = sd(sil))



ggplot(ds.comp) +
    geom_density(aes(x = sil, fill = num_fact), alpha = 0.5) +
    facet_grid(method~.) +
    theme_minimal()


ggplot(ds.comp) +
    geom_density(aes(x = rand, fill = num_fact), alpha = 0.5) +
    facet_grid(method~.) +
    theme_minimal()


ggplot(ds.comp)+
    geom_boxplot(aes(x = num_fact, y = rand, fill = method), alpha = 0.5) +
    theme_bw()





