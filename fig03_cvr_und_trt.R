####################################################################################
##SEASONALITY OF SOUTHERN APPALACHIAN FIRE BEHAVIOR 
##R code for plotting understory vegetation response by treatment
##Cover, multi-panel with all by growth habit, tree by group, and species of interest
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
library(naniar)
library(data.table)

#import raw data from Excel worksheet#
#(a) by growth habit
cvr.und.all <- read_excel("input/cvr_und_all_p.xlsx")
cvr.und.all <- data.frame(cvr.und.all)
#(b) tree by group
cvr.und.tree <- read_excel("input/cvr_und_tree_p.xlsx")
cvr.und.tree <- data.frame(cvr.und.tree)
#(c) red maple
cvr.und.arub <- read_excel("input/cvr_und_acerrubr_p.xlsx")
cvr.und.arub <- data.frame(cvr.und.arub)
#(d) mountain laurel
cvr.und.klat <- read_excel("input/cvr_und_kalmlati_p.xlsx")
cvr.und.klat <- data.frame(cvr.und.klat)

#assign list of variables by growth habit#
#(a) by growth habit
hbt.cvr.und.all <- list("delta_forb_asinsqrt_cvr",
                        "delta_gram_asinsqrt_cvr",
                        "delta_shrb_asinsqrt_cvr",
                        "delta_tree_asinsqrt_cvr",
                        "delta_vine_asinsqrt_cvr",
                        "delta_totl_asinsqrt_cvr")
#(b) tree by group
grp.cvr.und.tree <- list("delta_hickory_asinsqrt_cvr",
                         "delta_mesohwd_asinsqrt_cvr",
                         "delta_redoaks_asinsqrt_cvr",
                         "delta_whteoak_asinsqrt_cvr",
                         "delta_whtepin_asinsqrt_cvr",
                         "delta_yellpin_asinsqrt_cvr",
                         "delta_allothr_asinsqrt_cvr")
                         
##summarize response variables by mean and standard error##
#load summarySE function
#summarySE provides the standard deviation, standard error of the mean, and a default 95% confidence interval

#make list of summary tables by response variable#
#(a) by growth habit
delta.hbt.cvr.all.trt.lx <- lapply(hbt.cvr.und.all, function(var){
  df <- summarySE(cvr.und.all, measurevar = var, groupvars = "Trt", na.rm = TRUE) #returns data frame
  df$hbt <- substr(var, start = 7, stop = 10)
  df$mean_cvr <- df[,3]
  df <- df[,c(1, 2, 7, 8, 4, 5, 6)]
  assign(paste("delta.",
               substr(deparse(substitute(var)), start = 7, stop = 10),
               ".trt",
               sep=""),
         df)
})
#(b) tree by group
delta.grp.cvr.tree.trt.lx <- lapply(grp.cvr.und.tree, function(var){
  df <- summarySE(cvr.und.tree, measurevar = var, groupvars = "Trt", na.rm = TRUE) #returns data frame
  df$grp <- substr(var, start = 7, stop = 13)
  df$mean_cvr <- df[,3]
  df <- df[,c(1, 2, 7, 8, 4, 5, 6)]
  assign(paste("delta.",
               substr(deparse(substitute(var)), start = 7, stop = 13),
               ".trt",
               sep=""),
         df)
})
#(c) red maple
delta.cvr.arub.trt <- summarySE(cvr.und.arub,
                                measurevar = "delta_asinsqrt_cvr",
                                groupvars = "Trt",
                                na.rm = TRUE) #returns data frame
delta.cvr.arub.trt$spp <- "acerrubr"
delta.cvr.arub.trt$mean_cvr <- delta.cvr.arub.trt[,3]
delta.cvr.arub.trt <- delta.cvr.arub.trt[,c(1, 2, 7, 8, 4, 5, 6)]
#(d) mountain laurel
delta.cvr.klat.trt <- summarySE(cvr.und.klat,
                                measurevar = "delta_asinsqrt_cvr",
                                groupvars = "Trt",
                                na.rm = TRUE) #returns data frame
delta.cvr.klat.trt$spp <- "kalmlati"
delta.cvr.klat.trt$mean_cvr <- delta.cvr.klat.trt[,3]
delta.cvr.klat.trt <- delta.cvr.klat.trt[,c(1, 2, 7, 8, 4, 5, 6)]

#bind list elements to new data frame if needed#
#(a) by growth habit
delta.hbt.cvr.all.trt <- rbind(delta.hbt.cvr.all.trt.lx[[1]],
                               delta.hbt.cvr.all.trt.lx[[2]],
                               delta.hbt.cvr.all.trt.lx[[3]],
                               delta.hbt.cvr.all.trt.lx[[4]],
                               delta.hbt.cvr.all.trt.lx[[5]],
                               delta.hbt.cvr.all.trt.lx[[6]])
#(b) tree by group
delta.grp.cvr.tree.trt <- rbind(delta.grp.cvr.tree.trt.lx[[1]],
                                delta.grp.cvr.tree.trt.lx[[2]],
                                delta.grp.cvr.tree.trt.lx[[3]],
                                delta.grp.cvr.tree.trt.lx[[4]],
                                delta.grp.cvr.tree.trt.lx[[5]],
                                delta.grp.cvr.tree.trt.lx[[6]],
                                delta.grp.cvr.tree.trt.lx[[7]])

##plot response variable by treatment as means with standard error##
#(a) by growth habit#
plot.cvr.und.all <- ggplot(delta.hbt.cvr.all.trt, aes(x = hbt, y = mean_cvr, fill = Trt)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = mean_cvr - se, ymax = mean_cvr + se),
                size = 0.4,
                width = 0.2,
                position = position_dodge(width = 0.9)) +
  scale_x_discrete(limits = c("forb", "gram", "vine", "shrb", "tree", "totl"),
                   labels = c("Forb", "Graminoid", "Vine", "Shrub", "Tree", "All")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(name = "Treatment",
                    breaks = c("c", "d", "g"),
                    labels = c("Unburned control", "Dormant season", "Growing season"),
                    values = c("gray50", "sienna4", "green4")) +
  labs(x = "Growth habit") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 9, angle = 25, vjust = 0.625),
        axis.title.x = element_text(face = "bold", color = "black", size = 14,
                                    margin = margin(t = -7.5, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_blank(),
        legend.title = element_text(color = "black", size = 12),
        legend.text = element_text(color = "black", size = 10),
        legend.key.width = unit(2, "line"),
        legend.background = element_rect(fill = "gray90"),
        legend.margin = margin(t = 4, r = 4, b = 4, l = 4),
        legend.box.spacing = unit(0.15, "cm"))
plot.cvr.und.all

#(b) tree by group#
plot.cvr.und.tree <- ggplot(delta.grp.cvr.tree.trt, aes(x = grp, y = mean_cvr, fill = Trt)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = mean_cvr - se, ymax = mean_cvr + se),
                size = 0.4,
                width = 0.2,
                position = position_dodge(width = 0.9)) +
  scale_x_discrete(limits = c("hickory", "mesohwd", "redoaks", "whteoak",
                              "whtepin", "yellpin", "allothr"),
                   labels = c("Hickory", "Mesophytic\nhardwood", "Red oak", "White oak",
                              "White pine", "Yellow pine", "Other")) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(name = "Treatment",
                    breaks = c("c", "d", "g"),
                    labels = c("Unburned control", "Dormant season", "Growing season"),
                    values = c("gray50", "sienna4", "green4")) +
  labs(x = "Tree group") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 7.5, angle = 35, vjust = 0.7),
        axis.title.x = element_text(face = "bold", color = "black", size = 14,
                                    margin = margin(t = -7.5, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_blank(),
        legend.position = "none")
plot.cvr.und.tree

#(c) red maple#
plot.cvr.und.arub <- ggplot(delta.cvr.arub.trt, aes(x = spp, y = mean_cvr, fill = Trt)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = mean_cvr - se, ymax = mean_cvr + se),
                size = 0.6,
                width = 0.2,
                position = position_dodge(width = 0.9)) +
  scale_y_continuous(limits = c(-0.05, 0.22), labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(name = "Treatment",
                    breaks = c("c", "d", "g"),
                    labels = c("Unburned control", "Dormant season", "Growing season"),
                    values = c("gray50", "sienna4", "green4")) +
  labs(x = expression(bolditalic("Acer rubrum"))) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_text(color = "black", size = 14,
                                    margin = margin(t = -55, r = 0, b = 0, l = 0)),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_blank(),
        legend.position = "none")
plot.cvr.und.arub

#(d) mountain laurel#
plot.cvr.und.klat <- ggplot(delta.cvr.klat.trt, aes(x = spp, y = mean_cvr, fill = Trt)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = mean_cvr - se, ymax = mean_cvr + se),
                size = 0.6,
                width = 0.2,
                position = position_dodge(width = 0.9)) +
  scale_y_continuous(limits = c(-0.05, 0.22), labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(name = "Treatment",
                    breaks = c("c", "d", "g"),
                    labels = c("Unburned control", "Dormant season", "Growing season"),
                    values = c("gray50", "sienna4", "green4")) +
  labs(x = expression(bolditalic("Kalmia latifolia"))) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_text(face = "bold", color = "black", size = 14,
                                    margin = margin(t = -55, r = 0, b = 0, l = 0)),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none")
plot.cvr.und.klat

##arrange plots into multi-panel figure##
fig.cvr.und <- ggarrange(plot.cvr.und.all,
                         plot.cvr.und.tree,
                         plot.cvr.und.arub,
                         plot.cvr.und.klat,
                         ncol= 2,
                         nrow = 2,
                         align = "hv",
                         labels = c("a", "b", "c", "d"),
                         font.label = list(size = 14),
                         hjust = c(-5, -4.675, -5, -4.675),
                         vjust = 2,
                         common.legend = TRUE,
                         legend = "top") +
  theme(plot.margin = margin(t = 2.5, r = 0, b = -25, l = 0))
fig.cvr.und <- annotate_figure(fig.cvr.und,
                               top = text_grob("Understory",
                               face = "bold",
                               color = "black",
                               size = 16,
                               hjust = 0,
                               x = 0),
                               left = text_grob("Mean \u0394 cover (per m\u00B2)",
                               face = "bold",
                               color = "black",
                               size = 14,
                               rot = 90))
fig.cvr.und
#save to file
png("output/figures/ms 2/figx_tbl2_pnl_cvr_und_x_trt.png",
    width = 6.5,
    height = 6.5,
    units = "in",
    res = 600)
fig.cvr.und
dev.off()