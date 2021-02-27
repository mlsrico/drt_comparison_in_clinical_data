
# Run dim red and clustering scripts # 

## poner algún string que indique por donde va
        seed_vector <- c(17, 86) 
        # no tengo el 86 para 11f (fallo incomprensible), ejecutar cuando termine todo
        
for(i in seed_vector) {

sdnum <- i
print(paste("SEED:", sdnum, sep = " "))

## 3f----
print(paste("Simulation 3f, seed:", sdnum, sep = " "))
source("analysis/simulation/exp_3f.R")
rm(list=setdiff(ls(), "sdnum"))

print(paste("Dimred 3f, seed:", sdnum, sep = " "))
source("analysis/dim reduction/dimred_3f.R")
rm(list=setdiff(ls(), "sdnum"))

print(paste("Clustering 3f, seed:", sdnum, sep = " "))
source("analysis/clustering/hclust_3f.R")
rm(list=setdiff(ls(), "sdnum"))

## 5f ----

print(paste("Simulation 5f, seed:", sdnum, sep = " "))
source("analysis/simulation/exp_5f.R")
rm(list=setdiff(ls(), "sdnum"))

print(paste("Dimred 5f, seed:", sdnum, sep = " "))
source("analysis/dim reduction/dimred_5f.R")
rm(list=setdiff(ls(), "sdnum"))

print(paste("Clustering 5f, seed:", sdnum, sep = " "))
source("analysis/clustering/hclust_5f.R")
rm(list=setdiff(ls(), "sdnum"))

## 8f ----
print(paste("Simulation 8f, seed:", sdnum, sep = " "))
source("analysis/simulation/exp_8f.R")
rm(list=setdiff(ls(), "sdnum"))

print(paste("Dimred 8f, seed:", sdnum, sep = " "))
source("analysis/dim reduction/dimred_8f.R")
rm(list=setdiff(ls(), "sdnum"))

print(paste("Clustering 8f, seed:", sdnum, sep = " "))
source("analysis/clustering/hclust_8f.R")
rm(list=setdiff(ls(), "sdnum"))

## 11f ----
print(paste("Simulation 11f, seed:", sdnum, sep = " "))
source("analysis/simulation/exp_11f.R")
rm(list=setdiff(ls(), "sdnum"))

print(paste("Dimred 11f, seed:", sdnum, sep = " "))
source("analysis/dim reduction/dimred_11f.R")
rm(list=setdiff(ls(), "sdnum"))

print(paste("Clustering 11f, seed:", sdnum, sep = " "))
source("analysis/clustering/hclust_11f.R")
rm(list=setdiff(ls(), "sdnum"))

}
