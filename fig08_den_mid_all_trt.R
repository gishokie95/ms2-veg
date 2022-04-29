####################################################################################
##SEASONALITY OF SOUTHERN APPALACHIAN FIRE BEHAVIOR 
##R code for plotting midstory vegetation response by treatment
##Density, all by growth habit and DBH class
##JFSP Seasonality Project

##Matthew C. Vaughan, M.S. Student
##Department of Forestry and Environmental Conservation, Clemson University

##Last updated 03/07/21, R version 4.0.4
####################################################################################

##initial setup##

#set working directory#
setwd("G:/My Drive/MS-Clemson/JFSP Seasonality/Analysis/R/Treatment effects/Vegetation")

#access libraries#
library(readxl)
library(plyr)
library(dplyr)
library(ggplot2)
library(vegan)
library(metafor)
library(reshape)
library(gridExtra)
library(cowplot)
library(ggpubr)
library(scales)
library(tidyquant)
library(naniar)
library(zoo)
library(data.table)
library(forcats)

#import raw data from Excel worksheet#
den.mid.all <- read_excel("input/den_mid_all_p.xlsx")
den.mid.all <- data.frame(den.mid.all)

#assign list of variables by growth habit and DBH class#
grp.den.mid.all <- list("delta_001_shrb_perha",
                        "delta_002_shrb_perha",
                        "delta_003_shrb_perha",
                        "delta_all_shrb_perha",
                        
                        "delta_001_tree_perha",
                        "delta_002_tree_perha",
                        "delta_003_tree_perha",
                        "delta_all_tree_perha",
                        
                        "delta_001_xall_perha",
                        "delta_002_xall_perha",
                        "delta_003_xall_perha",
                        "delta_all_xall_perha")

##summarize response variables by mean and standard error##
#load summarySE function
#summarySE provides the standard deviation, standard error of the mean, and a default 95% confidence interval

#make list of summary tables by response variable#
delta.grp.den.mid.all.trt.lx <- lapply(grp.den.mid.all, function(var){
  df <- summarySE(den.mid.all, measurevar = var, groupvars = "Trt", na.rm = TRUE) #returns data frame
  df$hbt <- substr(var, start = 11, stop = 14)
  df$dbh <- substr(var, start = 7, stop = 9)
  df$mean_perha <- df[,3]
  df <- df[,c(1, 2, 7, 8, 9, 4, 5, 6)]
  assign(paste("delta.",
               substr(deparse(substitute(var)), start = 7, stop = 14),
               ".trt",
               sep=""),
         df)
})
#add significance comparsion letters
delta.grp.den.mid.all.trt.lx[[1]]$sig <- c("a", "b", "b") #001_shrb
delta.grp.den.mid.all.trt.lx[[2]]$sig <- NA
delta.grp.den.mid.all.trt.lx[[3]]$sig <- NA
delta.grp.den.mid.all.trt.lx[[4]]$sig <- c("a", "b", "c") #all_shrb
delta.grp.den.mid.all.trt.lx[[5]]$sig <- NA
delta.grp.den.mid.all.trt.lx[[6]]$sig <- c("a", "b", "b") #002_tree
delta.grp.den.mid.all.trt.lx[[7]]$sig <- NA
delta.grp.den.mid.all.trt.lx[[8]]$sig <- c("a", "ab", "b") #all_tree
delta.grp.den.mid.all.trt.lx[[9]]$sig <- c("a", "ab", "b") #001_xall
delta.grp.den.mid.all.trt.lx[[10]]$sig <- c("a", "b", "b") #002_xall
delta.grp.den.mid.all.trt.lx[[11]]$sig <- NA
delta.grp.den.mid.all.trt.lx[[12]]$sig <- c("a", "ab", "b") #all_xall

#bind to new data frame
delta.grp.den.mid.all.trt <- rbind(delta.grp.den.mid.all.trt.lx[[1]],
                                   delta.grp.den.mid.all.trt.lx[[2]],
                                   delta.grp.den.mid.all.trt.lx[[3]],
                                   delta.grp.den.mid.all.trt.lx[[4]],
                                   delta.grp.den.mid.all.trt.lx[[5]],
                                   delta.grp.den.mid.all.trt.lx[[6]],
                                   delta.grp.den.mid.all.trt.lx[[7]],
                                   delta.grp.den.mid.all.trt.lx[[8]],
                                   delta.grp.den.mid.all.trt.lx[[9]],
                                   delta.grp.den.mid.all.trt.lx[[10]],
                                   delta.grp.den.mid.all.trt.lx[[11]],
                                   delta.grp.den.mid.all.trt.lx[[12]])

#create a vector of growth habit/DBH class names for facet labels
hbt_names <- c(
  `shrb` = "Shrub",
  `tree` = "Tree",
  `xall` = "By DBH class"
)

##plot response variable by treatment as means with standard error and significance comparison##
plot.den.mid.all <- ggplot(delta.grp.den.mid.all.trt, aes(x = dbh, y = mean_perha, fill = Trt)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = mean_perha - se, ymax = mean_perha + se),
                size = 0.4,
                width = 0.2,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(label = sig, y = ifelse(mean_perha >= 0, mean_perha + se, mean_perha - se),
            vjust = ifelse(mean_perha >= 0, -0.625, 1.5)),
            size = 2.5,
            position = position_dodge(width = 0.9)) +
  facet_wrap(vars(fct_inorder(as.factor(hbt))),
             nrow = 1,
             labeller = as_labeller(hbt_names),
             strip.position = "bottom") +
  scale_x_discrete(limits = c("001", "002", "003", "all"),
                   labels = c("<3 cm", "3-6 cm", "6-10 cm", "All")) +
  #scale_y_continuous(labels = function(x)x*0.001) +
  scale_y_continuous(limits = c(-1800, NA), labels = comma) +
  scale_fill_manual(name = "Treatment",
                    breaks = c("c", "d", "g"),
                    labels = c("Unburned control", "Dormant season", "Growing season"),
                    values = c("gray50", "sienna4", "green4")) +
  #annotate("text", x = 12, y = 1000, label = "test") +
  labs(title = "Midstory",
       x = "Growth habit and DBH class",
       #y = "Mean \u0394 density (1,000 ha\u207B\u00B9)") +
       y = "Mean \u0394 density (per ha)") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", color = "black", size = 16),
        axis.text.x = element_text(size = 9, angle = 20, vjust = 0.7),
        axis.title.x = element_text(face = "bold", color = "black", size = 14),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(face = "bold", color = "black", size = 14),
        panel.spacing = unit(0, "cm"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(color = "grey20", size = 11),
        strip.switch.pad.wrap = unit(-0.225, "cm"),
        legend.position = "top",
        legend.title = element_text(color = "black", size = 12),
        legend.text = element_text(color = "black", size = 10),
        legend.key.width = unit(2, "line"),
        legend.background = element_rect(fill = "gray90"),
        legend.margin = margin(t = 4, r = 4, b = 4, l = 4),
        legend.box.margin = margin(t = -2.5, r = 0, b = 0, l = 0),
        legend.box.spacing = unit(0.15, "cm"))
plot.den.mid.all
#save to file
png("output/figures/ms 2/fig4n_den_mid_all_x_trt.png",
    width = 6.5,
    height = 4.5,
    units = "in",
    res = 600)
plot.den.mid.all
dev.off()