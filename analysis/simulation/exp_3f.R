
## Simulation ##


library(poLCA)
library(psych)
library(GPArotation)
library(tidyverse)
#library(doParallel)
library(purrr)
library(future)
library(furrr)
library(tictoc)

# Parameters ---- 

## Cores 
no_cores <- availableCores() - 1
plan(multicore, workers = no_cores)

## Values

set.seed(sdnum) #Seed
#set.seed(112) #Seed
N <- c(1000, 5000, 10000) #nsize
n <- c(20, 100, 200, 500) # numero de variables (nvar) 
#s <- list(c(0.1, 0.1, 0.8), #heterogeneo
#          c(0.1, 0.3, 0.6), #heterogeneo
#          c(0.33, 0.33, 0.33)) #homogeneo
s <- list(c(0.33, 0.33, 0.33)) #homogeneo
r <- c(0)
c <- c(0.01, 5, 10, 20, 25) #colinealidad (0.01 es para los que se quedan igual)

## Setup
ex <- expand.grid(nsize = N, nitems = n, sim = s, rango = r, col = c)
ex$id <- 1:dim(ex)[1]
ex0 <- ex %>% mutate(col = ifelse(col==0.01, 0, col)) %>% 
    dplyr::select(id, nsize, nitems, sim, col, rango)
saveRDS(ex0, "analysis/simulation/resdat/codes/idcodes3f.rds")

n.ex <- ex %>% group_by(id) %>% nest() #%>% filter(id%in%c(1:10))

#n.ex <- n.ex[c(1, 37),] #selecctionar una fila por cada col

# Simulation ---- 
#savelist <- list()
#for(z in 1:2){

## Probs

tic()
plan(sequential)
#plan(multiprocess)
n.ex <- n.ex %>% 
    dplyr::mutate(probs = future_map(data, function(x){
        probs <- list()
        for(i in 1:x$nitems){
            p <- runif(3, min = x$rango, max = 1.0)
            q <- 1-p
            probs[[i]] <- matrix(c(p[1],q[1], p[2],q[2], p[3],q[3]), ncol=2, byrow=TRUE)
        }
        return(probs)
    }))
toc()


## Sample size N, with probabilities of each class ----
tic()
#plan(sequential)
system.time(
    n.ex <- n.ex %>% 
        dplyr::mutate(simdat = map2(.x = data, .y = probs, .f = function(x, y){
            res <- poLCA.simdata(N = x$nsize, probs = y, P = x$sim)
            return(res)
        })) %>% 
        dplyr::mutate(res.data = future_map(simdat, function(x){
            BD <- x$dat
            BD$clases <- factor(as.character(x$trueclass))
            return(BD)
        }, .progress = TRUE)) 
)
toc()

## Add collinearity ----
#plan(multiprocess)
n.ex <- n.ex %>% 
    dplyr::mutate(res.data = map2(.x = data, .y = res.data, .f = function(x, y){
        df <- y %>% dplyr::select(-clases) #quito la variable clases
        n_col <- dim(df)[2] #numero de variables
        div <- (x$nsize/x$col)/10 #porcentaje de datos a sustituir
        PS <- x$nsize/div #numero de datos a sustituir
        if(PS > 1){
            for(i in 1:n_col){
                
                dato_sust <- sample(1:x$nsize, PS) #% de datos mising  
                df[dato_sust, i] <- 3-df[dato_sust, i] # Cambia el 2 a 1, y el 1 a 2
            }
        }
        df$clases <- y$clases #recuperar variable clases
        return(df)
    }))


## Save data ----
saveRDS(n.ex, "analysis/simulation/resdat/ex_sim_data_3f.rds")


only_data <- n.ex %>% dplyr::select(id, res.data)
id_data <- ex0 %>% dplyr::mutate(res.data = only_data$res.data) %>% as_tibble()


dir1 <- c("data/replicas/simulados/3f/datos_sim_3f")
dir2 <- c(".rds")
sdchar <- as.character(sdnum)

saveRDS(id_data, paste(dir1, paste(sdchar, dir2, sep = ""), sep = "_"))

#n.save <- n.ex %>% select(id, data, res.data)

#savelist[[z]] <- n.save
#saveRDS(savelist, "analysis/simulation/resdat/ex_sim_data_3f_midstep.rds")
#print(z)

#}

# Save list ----
#saveRDS(savelist, "analysis/simulation/resdat/ex_sim_data_3f_100rep.rds")

