


library(tidyverse)
library(cowplot)
#source("utilities/multiplot.R")
source("utilities/summarySE.R")

dat <- readRDS("data/replicas/all/all_reps.rds") %>% 
    mutate(calhar = calhar/10000) #%>% 
    #mutate(rand = ifelse(rand<0, 0, rand)) %>% 
    #mutate(sil_ln = log(sil), rand_ln = log(rand), calhar_ln = log(calhar), dunni_ln = log(dunni))
    #mutate_at(vars(sil, rand, calhar, dunni), log)

#colorval <- c("tomato", "deepskyblue", "chartreuse4")

# Line plot Silhouette ---- 

p1 <- dat %>% 
    ggplot(aes(x = num_fact, y = sil, group = method, 
               shape = method, linetype = method)) +
    stat_summary(fun = "mean", geom = "point", size = 2) +
    stat_summary(fun = "mean", geom = "line") +
    ylim(0,1) +
    labs(x = "Clusters", y = "Mean Silhouette", 
         shape = "", linetype = "") +
    scale_shape_discrete(breaks=c("pca","tsne","umap"), 
                         labels=c("PCA", "t-SNE", "UMAP"))+
    scale_linetype_discrete(breaks=c("pca","tsne","umap"), 
                            labels=c("PCA", "t-SNE", "UMAP"))+
    #theme_bw() +
    theme_cowplot(12)+
    theme(legend.position="top") ; p1


p2 <- dat %>% 
    ggplot(aes(x = nsize, y = sil, group = method, 
               shape = method, linetype = method)) +
    stat_summary(fun = "mean", geom = "point") +
    stat_summary(fun = "mean", geom = "line") +
    ylim(0,1) +
    labs(x = "Sample size", y = "Mean Silhouette", 
         shape = "", linetype = "") +
    scale_shape_discrete(breaks=c("pca","tsne","umap"), 
                         labels=c("PCA", "t-SNE", "UMAP"))+
    scale_linetype_discrete(breaks=c("pca","tsne","umap"), 
                            labels=c("PCA", "t-SNE", "UMAP"))+    
    #theme_bw() +
    theme_cowplot(12)+
    theme(legend.position="top")

p3 <- dat %>% 
    ggplot(aes(x = nitems, y = sil, group = method, 
               shape = method, linetype = method)) +
    stat_summary(fun = "mean", geom = "point") +
    stat_summary(fun = "mean", geom = "line") +
    ylim(0,1) +
    labs(x = "Variables", y = "Mean Silhouette", 
         shape = "", linetype = "") +
    scale_shape_discrete(breaks=c("pca","tsne","umap"), 
                         labels=c("PCA", "t-SNE", "UMAP"))+
    scale_linetype_discrete(breaks=c("pca","tsne","umap"), 
                            labels=c("PCA", "t-SNE", "UMAP"))+    
    #theme_bw() +
    theme_cowplot(12)+
    theme(legend.position="top")

p4 <- dat %>% 
    ggplot(aes(x = col, y = sil, group = method, 
               shape = method, linetype = method)) +
    stat_summary(fun = "mean", geom = "point") +
    stat_summary(fun = "mean", geom = "line") +
    ylim(0,1) +
    labs(x = "Noise", y = "Mean Silhouette",  
         shape = "", linetype = "") +
    scale_x_discrete(labels = c("0%", "5%", "10%", "20%", "25%")) +
    scale_shape_discrete(breaks=c("pca","tsne","umap"), 
                         labels=c("PCA", "t-SNE", "UMAP"))+
    scale_linetype_discrete(breaks=c("pca","tsne","umap"), 
                            labels=c("PCA", "t-SNE", "UMAP"))+    
    #theme_bw() +
    theme_cowplot(12)+
    theme(legend.position="top")



#multiplot(p1, p2, p3, p4, cols = 2)

res <- plot_grid(p1 + theme(legend.position="none"),
          p2 + theme(legend.position="none"),
          p3 + theme(legend.position="none"),
          p4 + theme(legend.position="none"),
          ncol = 2, nrow = 2, 
          labels = "AUTO")


# extract the legend from one of the plots
legend <- get_legend(
    p1 + 
        #guides(color = guide_legend(nrow = 1)) +
        theme(legend.position = "bottom")
)


# add the legend to the row we made earlier. Give it one-third of 
# the width of one plot (via rel_widths).
plot_grid(res, legend, ncol = 1, rel_heights = c(1, .1))



# Line plot Rand ----

p1 <- dat %>% 
    ggplot(aes(x = num_fact, y = rand, group = method, color = method)) +
    stat_summary(fun = "mean", geom = "point") +
    stat_summary(fun = "mean", geom = "line") +
    #ylim(0,1) +
    labs(x = "Number of clusters", y = "Mean Rand", color = "") +
    scale_color_manual(values = colorval, breaks=c("pca","tsne","umap"), labels=c("PCA", "t-SNE", "UMAP"))+
    theme_bw() +
    theme(legend.position="top") 




p2 <- dat %>% 
    ggplot(aes(x = nsize, y = rand, group = method, color = method)) +
    stat_summary(fun = "mean", geom = "point") +
    stat_summary(fun = "mean", geom = "line") +
    #ylim(0,1) +
    labs(x = "Sample size", y = "Mean Rand", color = "") +
    scale_color_manual(values = colorval, breaks=c("pca","tsne","umap"), labels=c("PCA", "t-SNE", "UMAP"))+
    theme_bw() +
    theme(legend.position="top")



p3 <- dat %>% 
    ggplot(aes(x = nitems, y = rand, group = method, color = method)) +
    stat_summary(fun = "mean", geom = "point") +
    stat_summary(fun = "mean", geom = "line") +
    #ylim(0,1) +
    labs(x = "Number of variables", y = "Mean Rand", color = "") +
    scale_color_manual(values = colorval, breaks=c("pca","tsne","umap"), labels=c("PCA", "t-SNE", "UMAP"))+
    theme_bw() +
    theme(legend.position="top")




p4 <- dat %>% 
    ggplot(aes(x = col, y = rand, group = method, color = method)) +
    stat_summary(fun = "mean", geom = "point") +
    stat_summary(fun = "mean", geom = "line") +
    #ylim(0,1) +
    labs(x = "Noise", y = "Mean Rand", color = "") +
    scale_x_discrete(labels = c("0%", "5%", "10%", "20%", "25%")) +
    scale_color_manual(values = colorval, breaks=c("pca","tsne","umap"), labels=c("PCA", "t-SNE", "UMAP"))+
    theme_bw() +
    theme(legend.position="top")



multiplot(p1, p2, p3, p4, cols = 2)






# Line plot calhar ----

p1 <- dat %>% 
    ggplot(aes(x = num_fact, y = calhar, group = method, color = method)) +
    stat_summary(fun = "mean", geom = "point") +
    stat_summary(fun = "mean", geom = "line") +
    #ylim(0,150) +
    labs(x = "Number of clusters", y = "Mean Calinski-Harabasz", color = "") +
    scale_color_manual(values = colorval, breaks=c("pca","tsne","umap"), labels=c("PCA", "t-SNE", "UMAP"))+
    theme_bw() +
    theme(legend.position="top") 




p2 <- dat %>% 
    ggplot(aes(x = nsize, y = calhar, group = method, color = method)) +
    stat_summary(fun = "mean", geom = "point") +
    stat_summary(fun = "mean", geom = "line") +
    #ylim(0,150) +
    labs(x = "Sample size", y = "Mean Calinski-Harabasz", color = "") +
    scale_color_manual(values = colorval, breaks=c("pca","tsne","umap"), labels=c("PCA", "t-SNE", "UMAP"))+
    theme_bw() +
    theme(legend.position="top")



p3 <- dat %>% 
    ggplot(aes(x = nitems, y = calhar, group = method, color = method)) +
    stat_summary(fun = "mean", geom = "point") +
    stat_summary(fun = "mean", geom = "line") +
    #ylim(0,150) +
    labs(x = "Number of variables", y = "Mean Calinski-Harabasz", color = "") +
    scale_color_manual(values = colorval, breaks=c("pca","tsne","umap"), labels=c("PCA", "t-SNE", "UMAP"))+
    theme_bw() +
    theme(legend.position="top")




p4 <- dat %>% 
    ggplot(aes(x = col, y = calhar, group = method, color = method)) +
    stat_summary(fun = "mean", geom = "point") +
    stat_summary(fun = "mean", geom = "line") +
    #ylim(0,120) +
    labs(x = "Noise", y = "Mean Calinski-Harabasz", color = "") +
    scale_x_discrete(labels = c("0%", "5%", "10%", "20%", "25%")) +
    scale_color_manual(values = colorval, breaks=c("pca","tsne","umap"), labels=c("PCA", "t-SNE", "UMAP"))+
    theme_bw() +
    theme(legend.position="top") 


multiplot(p1, p2, p3, p4, cols = 2)






# Line plot dunni ----

p1 <- dat %>% 
    ggplot(aes(x = num_fact, y = dunni, group = method, color = method)) +
    stat_summary(fun = "mean", geom = "point") +
    stat_summary(fun = "mean", geom = "line") +
    #ylim(0,1) +
    labs(x = "Number of clusters", y = "Mean Dunn Index", color = "") +
    scale_color_manual(values = colorval, breaks=c("pca","tsne","umap"), labels=c("PCA", "t-SNE", "UMAP"))+
    theme_bw() +
    theme(legend.position="top") 




p2 <- dat %>% 
    ggplot(aes(x = nsize, y = dunni, group = method, color = method)) +
    stat_summary(fun = "mean", geom = "point") +
    stat_summary(fun = "mean", geom = "line") +
    #ylim(0,1) +
    labs(x = "Sample size", y = "Mean Dunn Index", color = "") +
    scale_color_manual(values = colorval, breaks=c("pca","tsne","umap"), labels=c("PCA", "t-SNE", "UMAP"))+
    theme_bw() +
    theme(legend.position="top")



p3 <- dat %>% 
    ggplot(aes(x = nitems, y = dunni, group = method, color = method)) +
    stat_summary(fun = "mean", geom = "point") +
    stat_summary(fun = "mean", geom = "line") +
    #ylim(0,1) +
    labs(x = "Number of variables", y = "Mean Dunn Index", color = "") +
    scale_color_manual(values = colorval, breaks=c("pca","tsne","umap"), labels=c("PCA", "t-SNE", "UMAP"))+
    theme_bw() +
    theme(legend.position="top")




p4 <- dat %>% 
    ggplot(aes(x = col, y = dunni, group = method, color = method)) +
    stat_summary(fun = "mean", geom = "point") +
    stat_summary(fun = "mean", geom = "line") +
    #ylim(0,1) +
    labs(x = "Noise", y = "Mean Dunn Index", color = "") +
    scale_x_discrete(labels = c("0%", "5%", "10%", "20%", "25%")) +
    scale_color_manual(values = colorval, breaks=c("pca","tsne","umap"), labels=c("PCA", "t-SNE", "UMAP"))+
    theme_bw() +
    theme(legend.position="top")



multiplot(p1, p2, p3, p4, cols = 2)




test <- dat %>% mutate(nsize_nitems = as.factor(paste(nsize, nitems, sep = "_")))

test %>% filter(nsize=="10000") %>% 
ggplot(aes(x = sil, color = nsize_nitems)) +
    geom_density()+
    facet_grid(method~.)+
    theme_bw()


