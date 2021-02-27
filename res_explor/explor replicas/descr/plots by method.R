

library(ggthemes)
library(tidyverse)
source("utilities/multiplot.R")
source("utilities/summarySE.R")

dat <- readRDS("data/replicas/all/all_reps.rds") %>% 
    mutate(calhar = calhar/10000) %>% 
    mutate(rand = ifelse(rand<0, 0, rand)) %>% 
    #mutate(sil_ln = log(sil), rand_ln = log(rand), calhar_ln = log(calhar), dunni_ln = log(dunni)) %>% 
    mutate(method = factor(as.character(method), 
                           levels = c("pca", "tsne", "umap"),
                           labels = c("PCA", "t-SNE", "UMAP")))

colorval <- c("tomato", "deepskyblue", "chartreuse4")


# sil ---- 
med_sil <- dat %>% group_by(method) %>% 
    summarise(median = median(sil)) %>% 
    mutate(median_r = round(median, 2)) %>%
    data.frame

ann_sil <- data.frame(x1 = c(med_sil$median), x2 = c(med_sil$median_r),  
                   y1 = c(0, 0, 0), y2 = c(5000, 5000, 5000),
                   xflab = c(med_sil$median+0.06), yflab = c(4500,4500,4500),
                   xflab2 = c(med_sil$median), yflab2 = c(4500,4500,4500),
                   lab = as.character(med_sil$median_r),
                   method =  c("PCA", "t-SNE", "UMAP"))

p1 <-ggplot(dat, aes(x = sil)) +
    geom_histogram(alpha = 0.2, bins = 50, aes(color = method, fill = method)) + 
    scale_fill_manual(values = colorval) + 
    scale_color_manual(values = colorval) +
    theme_bw() +
    geom_segment(data = ann_sil, linetype = "dashed", 
                 aes(x = x1, xend = x1, 
                     y = y1, yend = y2, 
                     color = method)) +
    geom_segment(data = ann_sil, linetype = "dashed", 
                 aes(x = xflab, xend = xflab2, 
                     y = yflab2, yend = yflab2, 
                     color = method)) +
    geom_label(data = ann_sil, 
                 aes(label = med_sil$median_r, 
                     x = xflab, 
                     y = yflab, 
                     color = method)) +
    facet_grid(method ~.) +
    theme(legend.position = "top", 
          #text = element_text(family = "serif"), 
          title = element_text(size = 13), 
          legend.text = element_text(size = 11), 
          strip.text.y = element_text(size = 13, #face = "bold"
                                      )) +
    guides(color = "none", fill = "none") +
    labs(x = "Silhouette Coefficient", y = "Frequency")



    ## rand ---- 
med_rand <- dat %>% group_by(method) %>% 
    summarise(median = median(rand)) %>% 
    mutate(median_r = round(median, 2)) %>%
    data.frame

ann_rand <- data.frame(x1 = c(med_rand$median), x2 = c(med_rand$median_r),  
                   y1 = c(0, 0, 0), y2 = c(10500, 10500, 10500),
                   xflab = c(med_rand$median-0.06), yflab = c(8500,8500,8500),
                   xflab2 = c(med_rand$median), yflab2 = c(8500,8500,8500),
                   lab = as.character(med_rand$median_r),
                   method =  c("PCA", "t-SNE", "UMAP"))

p2<-ggplot(dat, aes(x = rand)) +
    geom_histogram(alpha = 0.2, bins = 50, aes(color = method, fill = method)) + 
    scale_fill_manual(values = colorval) + 
    scale_color_manual(values = colorval) +
    theme_bw() +
    geom_segment(data = ann_rand, linetype = "dashed", 
                 aes(x = x1, xend = x1, 
                     y = y1, yend = y2, 
                     color = method)) +
    geom_segment(data = ann_rand, linetype = "dashed", 
                 aes(x = xflab, xend = xflab2, 
                     y = yflab2, yend = yflab2, 
                     color = method)) +
    geom_label(data = ann_rand, 
               aes(label = med_rand$median_r, 
                   x = xflab, 
                   y = yflab, 
                   color = method)) +
    facet_grid(method ~.) +
    theme(legend.position = "top", 
          #text = element_text(family = "serif"), 
          title = element_text(size = 13), 
          legend.text = element_text(size = 11), 
          strip.text.y = element_text(size = 13, #face = "bold"
                                      )) +
    guides(color = "none", fill = "none") +
    labs(x = "Rand Index", y = "Frequency")

## dunn ---- 
med_dunni <- dat %>% group_by(method) %>% 
    summarise(median = median(dunni)) %>% 
    mutate(median_r = round(median, 2)) %>%
    data.frame

ann_dunni <- data.frame(x1 = c(med_dunni$median), x2 = c(med_dunni$median_r),  
                       y1 = c(0, 0, 0), y2 = c(25000, 25000, 25000),
                       xflab = c(med_dunni$median+0.7), yflab = c(22000,22000,22000),
                       xflab2 = c(med_dunni$median), yflab2 = c(22000,22000,22000),
                       lab = as.character(med_dunni$median_r),
                       method =  c("PCA", "t-SNE", "UMAP"))

p3<-ggplot(dat, aes(x = dunni)) +
    geom_histogram(alpha = 0.2, bins = 50, aes(color = method, fill = method)) + 
    scale_fill_manual(values = colorval) + 
    scale_color_manual(values = colorval) +
    theme_bw() +
    geom_segment(data = ann_dunni, linetype = "dashed", 
                 aes(x = x1, xend = x1, 
                     y = y1, yend = y2, 
                     color = method)) +
    geom_segment(data = ann_dunni, linetype = "dashed", 
                 aes(x = xflab, xend = xflab2, 
                     y = yflab2, yend = yflab2, 
                     color = method)) +
    geom_label(data = ann_dunni, 
               aes(label = med_dunni$median_r, 
                   x = xflab, 
                   y = yflab, 
                   color = method)) +
    facet_grid(method ~.) +
    theme(legend.position = "top", 
          #text = element_text(family = "serif"), 
          title = element_text(size = 13), 
          legend.text = element_text(size = 11), 
          strip.text.y = element_text(size = 13, #face = "bold"
                                      )) +
    guides(color = "none", fill = "none") +
    labs(x = "Dunn Index", y = "Frequency")


## calhar ---- 
med_calhar <- dat %>% group_by(method) %>% 
    summarise(median = median(calhar)) %>% 
    mutate(median_r = round(median, 2)) %>%
    data.frame

ann_calhar <- data.frame(x1 = c(med_calhar$median), x2 = c(med_calhar$median_r),  
                        y1 = c(0, 0, 0), y2 = c(25000, 25000, 25000),
                        xflab = c(med_calhar$median+40), yflab = c(20000,20000,20000),
                        xflab2 = c(med_calhar$median), yflab2 = c(20000,20000,20000),
                        lab = as.character(med_calhar$median_r),
                        method =  c("PCA", "t-SNE", "UMAP"))

p4<-ggplot(dat, aes(x = calhar)) +
    geom_histogram(alpha = 0.2, bins = 50, aes(color = method, fill = method)) + 
    scale_fill_manual(values = colorval) + 
    scale_color_manual(values = colorval) +
    theme_bw() +
    geom_segment(data = ann_calhar, linetype = "dashed", 
                 aes(x = x1, xend = x1, 
                     y = y1, yend = y2, 
                     color = method)) +
    geom_segment(data = ann_calhar, linetype = "dashed", 
                 aes(x = xflab, xend = xflab2, 
                     y = yflab2, yend = yflab2, 
                     color = method)) +
    geom_label(data = ann_calhar, 
               aes(label = med_calhar$median_r, 
                   x = xflab, 
                   y = yflab, 
                   color = method)) +
    facet_grid(method ~.) +
    theme(legend.position = "top", 
          #text = element_text(family = "serif"), 
          title = element_text(size = 13), 
          legend.text = element_text(size = 11), 
          strip.text.y = element_text(size = 13, #face = "bold"
                                      )) +
    guides(color = "none", fill = "none") +
    labs(x = "Calinski-Harabasz Index", y = "Frequency")


# Combinado ---- 


multiplot(p1, p2, p3, p4, cols = 2)

multiplot(p1, p2, cols = 2)

