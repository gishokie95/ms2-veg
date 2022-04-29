####################################################################################
##SEASONALITY OF SOUTHERN APPALACHIAN FIRE BEHAVIOR 
##R code for plotting understory vegetation response by treatment
##Density, woody by growth habit and stem origin
##JFSP Seasonality Project

##Matthew C. Vaughan, M.S. Student
##Department of Forestry and Environmental Conservation, Clemson University

##Last updated 07/27/21, R version 4.1.0
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
library(naniar)
library(data.table)
library(forcats)

#import raw data from Excel worksheet#
den.und.wdy <- read_excel("input/den_und_wdy_cat_p.xlsx")
den.und.wdy <- data.frame(den.und.wdy)

#assign list of variables by growth habit/life history category#
cat.den.und.wdy <- list("delta_grm_vine_perha",
                        "delta_est_vine_perha",
                        "delta_spr_vine_perha",
                        
                        "delta_grm_shrb_perha",
                        "delta_est_shrb_perha",
                        "delta_spr_shrb_perha",
                        
                        "delta_grm_tree_perha",
                        "delta_est_tree_perha",
                        "delta_spr_tree_perha",
                        
                        "delta_grm_totl_perha",
                        "delta_est_totl_perha",
                        "delta_spr_totl_perha")

##summarize response variables by mean and standard error##
#load summarySE function
#summarySE provides the standard deviation, standard error of the mean, and a default 95% confidence interval

#make list of summary tables by response variable#
delta.cat.den.und.wdy.trt.lx <- lapply(cat.den.und.wdy, function(var){
  df <- summarySE(den.und.wdy, measurevar = var, groupvars = "Trt", na.rm = TRUE) #returns data frame
  df$hbt <- substr(var, start = 11, stop = 14)
  df$cat <- substr(var, start = 7, stop = 9)
  df$mean_perha <- df[,3]
  df <- df[,c(1, 2, 7, 8, 9, 4, 5, 6)]
  assign(paste("delta.",
               substr(deparse(substitute(var)), start = 7, stop = 14),
               ".trt",
               sep=""),
         df)
})
#add significance comparsion letters
delta.cat.den.und.wdy.trt.lx[[1]]$sig <- NA
delta.cat.den.und.wdy.trt.lx[[2]]$sig <- NA
delta.cat.den.und.wdy.trt.lx[[3]]$sig <- NA
delta.cat.den.und.wdy.trt.lx[[4]]$sig <- NA
delta.cat.den.und.wdy.trt.lx[[5]]$sig <- NA
delta.cat.den.und.wdy.trt.lx[[6]]$sig <- NA
delta.cat.den.und.wdy.trt.lx[[7]]$sig <- NA
delta.cat.den.und.wdy.trt.lx[[8]]$sig <- NA
delta.cat.den.und.wdy.trt.lx[[9]]$sig <- c("b", "a", "a") #spr_tree
delta.cat.den.und.wdy.trt.lx[[10]]$sig <- NA
delta.cat.den.und.wdy.trt.lx[[11]]$sig <- NA
delta.cat.den.und.wdy.trt.lx[[12]]$sig <- NA
#bind to new data frame
delta.cat.den.und.wdy.trt <- rbind(delta.cat.den.und.wdy.trt.lx[[1]],
                                   delta.cat.den.und.wdy.trt.lx[[2]],
                                   delta.cat.den.und.wdy.trt.lx[[3]],
                                   delta.cat.den.und.wdy.trt.lx[[4]],
                                   delta.cat.den.und.wdy.trt.lx[[5]],
                                   delta.cat.den.und.wdy.trt.lx[[6]],
                                   delta.cat.den.und.wdy.trt.lx[[7]],
                                   delta.cat.den.und.wdy.trt.lx[[8]],
                                   delta.cat.den.und.wdy.trt.lx[[9]],
                                   delta.cat.den.und.wdy.trt.lx[[10]],
                                   delta.cat.den.und.wdy.trt.lx[[11]],
                                   delta.cat.den.und.wdy.trt.lx[[12]])

#create a vector of growth habit names for facet labels
hbt_names <- c(
  `vine` = "Vine",
  `shrb` = "Shrub",
  `tree` = "Tree",
  `totl` = "All"
)

##plot response variable by treatment as means with standard error##
plot.den.und.wdy <- ggplot(delta.cat.den.und.wdy.trt, aes(x = cat, y = mean_perha, fill = Trt)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = mean_perha - se, ymax = mean_perha + se),
                size = 0.4,
                width = 0.2,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(label = sig, y = ifelse(mean_perha >= 0, mean_perha + se, mean_perha - se),
                vjust = ifelse(mean_perha >= 0, -0.625, 1.5)),
            size = 3,
            position = position_dodge(width = 0.9)) +
  facet_wrap(vars(fct_inorder(as.factor(hbt))),
             nrow = 1,
             labeller = as_labeller(hbt_names),
             strip.position = "bottom") +
  scale_x_discrete(limits = c("grm", "est", "spr"),
                   labels = c("Germinant", "Established", "Sprout")) +
  #scale_y_continuous(labels = function(x)x*0.00001) +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(name = "Treatment",
                    breaks = c("c", "d", "g"),
                    labels = c("Unburned control", "Dormant season", "Growing season"),
                    values = c("gray50", "sienna4", "green4")) +
  labs(title = "Understory",
       x = "Woody by growth habit and stem origin",
       #y = "Mean \u0394 density (100,000 ha\u207B\u00B9)") +
       y = "Mean \u0394 density (per ha)") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", color = "black", size = 16),
        axis.text.x = element_text(size = 9, angle = 25, vjust = 0.625),
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
plot.den.und.wdy
#save to file
png("output/figures/ms 2/fig05_den_und_wdy_trt.png",
    width = 6.5,
    height = 4.5,
    units = "in",
    res = 600)
plot.den.und.wdy
dev.off()