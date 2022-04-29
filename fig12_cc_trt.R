####################################################################################
##SEASONALITY OF SOUTHERN APPALACHIAN FIRE BEHAVIOR 
##R code for plotting change in canopy cover
##JFSP Seasonality Project

##Matthew C. Vaughan, M.S. Student
##Department of Forestry and Environmental Conservation, Clemson University

##Last updated 01/20/21, R version 4.0.3
####################################################################################

##initial setup##

#set working directory#
setwd("G:/My Drive/MS-Clemson/JFSP Seasonality/Analysis/R/Treatment effects/Canopy cover")

#access libraries#
library(readxl)
library(tidyr)
library(plyr)
library(dplyr)
library(metafor)
library(reshape)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(ggpubr)
library(scales)
library(data.table)

#import raw data from Excel worksheet#
cc.all <- read_excel("input/cc_all_p.xlsx")
cc.all <- data.frame(cc.all)

##summarize response variables##
#load summarySEminmax function
# minmax.cc.all <- summarySEminmax(cc.all,
#                                  measurevar = "mean_delta_cover_asinsqrt_prop",
#                                  groupvars = "Trt",
#                                  na.rm = TRUE)

#add significance comparsion letters
#minmax.cc.all$sig <- c("a", "b", "b")

##plot change in canopy cover by treatment as boxplot##
plot.cc <- ggplot(cc.all, aes(x = Trt, y = mean_delta_cover_asinsqrt_prop, color = Trt)) +
  geom_boxplot(size = 2, outlier.size = 2, width = 0.625) +
  annotate("text", x = 1, y = 0.13295580 + 0.025, label= "a") +
  annotate("text", x = 2, y = 0.05249273 + 0.025, label= "b") +
  annotate("text", x = 3, y = 0.06552249 + 0.025, label= "b") +
  scale_x_discrete(breaks = c("c", "d", "g"),
                   labels = c("Unburned control", "Dormant season", "Growing season")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_color_manual(values = c("gray50", "sienna4", "green4")) +
  labs(x = "Treatment", y = "Mean \u0394 canopy cover") +
  theme(axis.text.x = element_text(size = 11),
        axis.title.x = element_text(face = "bold", color = "black", size = 16),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(face = "bold", color = "black", size = 16),
        legend.position = "none")
plot.cc
#save to file
png("output/figures/ms 2/fig5_cc_x_trt.png", width = 5.25, height = 3.5, units = "in", res = 600)
plot.cc
dev.off()