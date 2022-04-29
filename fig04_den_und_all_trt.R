####################################################################################
##SEASONALITY OF SOUTHERN APPALACHIAN FIRE BEHAVIOR 
##R code for plotting understory vegetation response by treatment
##Density, all by growth habit
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

#import raw data from Excel worksheet#
den.und.all <- read_excel("input/den_und_all_p.xlsx")
den.und.all <- data.frame(den.und.all)

#assign list of variables by growth habit#
hbt.den.und.all <- list("delta_forb_perha",
                        "delta_gram_perha",
                        "delta_shrb_perha",
                        "delta_tree_perha",
                        "delta_vine_perha",
                        "delta_totl_perha")

##summarize response variables by mean and standard error##
#load summarySE function
#summarySE provides the standard deviation, standard error of the mean, and a default 95% confidence interval

#make list of summary tables by response variable#
delta.hbt.den.und.all.trt.lx <- lapply(hbt.den.und.all, function(var){
  df <- summarySE(den.und.all, measurevar = var, groupvars = "Trt", na.rm = TRUE) #returns data frame
  df$hbt <- substr(var, start = 7, stop = 10)
  df$mean_perha <- df[,3]
  df <- df[,c(1, 2, 7, 8, 4, 5, 6)]
  assign(paste("delta.",
               substr(deparse(substitute(var)), start = 7, stop = 10),
               ".trt",
               sep=""),
         df)
})
#bind to new data frame
delta.hbt.den.und.all.trt <- rbind(delta.hbt.den.und.all.trt.lx[[1]],
                                   delta.hbt.den.und.all.trt.lx[[2]],
                                   delta.hbt.den.und.all.trt.lx[[3]],
                                   delta.hbt.den.und.all.trt.lx[[4]],
                                   delta.hbt.den.und.all.trt.lx[[5]],
                                   delta.hbt.den.und.all.trt.lx[[6]])

##plot response variable by treatment as means with standard error##
plot.den.und.all <- ggplot(delta.hbt.den.und.all.trt, aes(x = hbt, y = mean_perha, fill = Trt)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = mean_perha - se, ymax = mean_perha + se),
                size = 0.4,
                width = 0.2,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(label = sig, y = ifelse(mean_perha >= 0, mean_perha + se, mean_perha - se),
                vjust = ifelse(mean_perha >= 0, -0.625, 1.5)),
            size = 3,
            position = position_dodge(width = 0.9)) +
  scale_x_discrete(limits = c("forb", "gram", "vine", "shrb", "tree", "totl"),
                   labels = c("Forb", "Graminoid", "Vine", "Shrub", "Tree", "All")) +
  #scale_y_continuous(labels = function(x)x*0.00001) +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(name = "Treatment",
                    breaks = c("c", "d", "g"),
                    labels = c("Unburned control", "Dormant season", "Growing season"),
                    values = c("gray50", "sienna4", "green4")) +
  labs(title = "Understory",
       x = "Growth habit",
       #y = "Mean \u0394 density (100,000 ha\u207B\u00B9)") +
       y = "Mean \u0394 density (per ha)") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", color = "black", size = 16),
        axis.text.x = element_text(size = 11),
        axis.title.x = element_text(face = "bold", color = "black", size = 14),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(face = "bold", color = "black", size = 14),
        legend.position = c(0.225, 0.8),
        legend.background = element_rect(fill = "gray90"),
        legend.title = element_text(color = "black", size = 12),
        legend.text = element_text(color = "black", size = 10),
        legend.key.width = unit(2, "line"))
plot.den.und.all
#save to file
png("output/figures/ms 2/fig3n_den_und_all_x_trt.png",
    width = 6.5,
    height = 4,
    units = "in",
    res = 600)
plot.den.und.all
dev.off()