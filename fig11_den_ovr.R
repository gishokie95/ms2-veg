####################################################################################
##SEASONALITY OF SOUTHERN APPALACHIAN FIRE BEHAVIOR 
##R code for plotting overstory vegetation response by treatment
##Density, by various groups
##JFSP Seasonality Project

##Matthew C. Vaughan, M.S. Student
##Department of Forestry and Environmental Conservation, Clemson University

##Last updated 03/08/21, R version 4.0.4
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
library(grid)
library(gridExtra)
library(cowplot)
library(ggpubr)
library(scales)
library(naniar)
library(data.table)
library(forcats)

#import raw data from Excel worksheet#
den.ovr.tre <- read_excel("input/den_ovr_tree_p.xlsx")
den.ovr.tre <- data.frame(den.ovr.tre)
den.ovr.all <- read_excel("input/den_ovr_all_p.xlsx")
den.ovr.all <- data.frame(den.ovr.all)
den.ovr.acerrubr <- read_excel("input/den_ovr_acerrubr_p.xlsx")
den.ovr.acerrubr <- data.frame(den.ovr.acerrubr)

#join data tables#
den.ovr <- den.ovr.tre %>%
  select(-(DID:UID)) %>%
  full_join(den.ovr.all, by = "PID") %>%
  select(-(DID:UID)) %>%
  full_join(den.ovr.acerrubr, by = "PID") %>%
  select(-(DID:UID)) %>%
  mutate(DID = substr(PID, start = 1, stop = 2),
         RID = substr(PID, start = 1, stop = 3),
         Trt = substr(PID, start = 4, stop = 4),
         UID = substr(PID, start = 1, stop = 4)) %>%
  relocate(DID:UID)

#assign list of variables by species/life history category#
tfg.den.ovr <- list("delta_hickory_perha",
                "delta_mesohwd_perha",
                "delta_redoaks_perha",
                "delta_whteoak_perha",
                # "delta_whtepin_perha",
                "delta_yellpin_perha",
                "delta_allothr_perha",
                
                "delta_xallovr_perha")
                
                # "delta_acrrubr_perha")

##summarize response variables by mean and standard error##
#load summarySE function
#summarySE provides the standard deviation, standard error of the mean, and a default 95% confidence interval

#make list of summary tables by response variable#
delta.tfg.den.ovr.trt.lx <- lapply(tfg.den.ovr, function(var){
  df <- summarySE(den.ovr, measurevar = var, groupvars = "Trt", na.rm = TRUE) #returns data frame
  df$tfg <- substr(var, start = 7, stop = 13)
  df$mean_perha <- df[,3]
  df <- df[,c(1, 2, 7, 8, 4, 5, 6)]
  assign(paste("delta.",
               substr(deparse(substitute(var)), start = 7, stop = 13),
               ".trt",
               sep=""),
         df)
})
#add significance comparsion letters
delta.tfg.den.ovr.trt.lx[[1]]$sig <- NA
delta.tfg.den.ovr.trt.lx[[2]]$sig <- NA
delta.tfg.den.ovr.trt.lx[[3]]$sig <- NA
delta.tfg.den.ovr.trt.lx[[4]]$sig <- NA
delta.tfg.den.ovr.trt.lx[[5]]$sig <- NA
delta.tfg.den.ovr.trt.lx[[6]]$sig <- NA
delta.tfg.den.ovr.trt.lx[[7]]$sig <- NA

#bind to new data frame
delta.tfg.den.ovr.trt <- rbind(delta.tfg.den.ovr.trt.lx[[1]],
                               delta.tfg.den.ovr.trt.lx[[2]],
                               delta.tfg.den.ovr.trt.lx[[3]],
                               delta.tfg.den.ovr.trt.lx[[4]],
                               delta.tfg.den.ovr.trt.lx[[5]],
                               delta.tfg.den.ovr.trt.lx[[6]],
                               delta.tfg.den.ovr.trt.lx[[7]])
#specify x-axis labels as factor levels
delta.tfg.den.ovr.trt$tfg <- fct_inorder(as.factor(delta.tfg.den.ovr.trt$tfg))

##plot response variable by treatment as means with standard error##
plot.den.ovr <- ggplot(delta.tfg.den.ovr.trt, aes(x = tfg, y = mean_perha, fill = Trt)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = mean_perha - se, ymax = mean_perha + se),
                size = 0.4,
                width = 0.2,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(label = sig, y = ifelse(mean_perha >= 0, mean_perha + se, mean_perha - se),
                vjust = ifelse(mean_perha >= 0, -0.625, 1.5)),
            size = 3,
            position = position_dodge(width = 0.9)) +
  scale_x_discrete(breaks = c("hickory", "mesohwd", "redoaks", "whteoak",
                              "yellpin", "allothr", "xallovr"),
                   labels = c("Hickory", "Mesophytic\nhardwood", "Red oak", "White oak",
                              "Yellow pine", "Other", "All")) +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(name = "Treatment",
                    breaks = c("c", "d", "g"),
                    labels = c("Unburned control", "Dormant season", "Growing season"),
                    values = c("gray50", "sienna4", "green4")) +
  labs(title = "Overstory",
       x = "Functional group",
       #y = "Mean \u0394 density (100,000 ha\u207B\u00B9)") +
       y = "Mean \u0394 density (per ha)") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", color = "black", size = 16),
        axis.text.x = element_text(size = 9, angle = 0, vjust = 0.5),
        axis.title.x = element_text(face = "bold", color = "black", size = 14),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(face = "bold", color = "black", size = 14),
        legend.position = "top",
        legend.title = element_text(color = "black", size = 12),
        legend.text = element_text(color = "black", size = 10),
        legend.key.width = unit(2, "line"),
        legend.background = element_rect(fill = "gray90"),
        legend.margin = margin(t = 4, r = 4, b = 4, l = 4),
        legend.box.margin = margin(t = -2.5, r = 0, b = 0, l = 0),
        legend.box.spacing = unit(0.15, "cm"))
plot.den.ovr
#save to file
png("output/figures/ms 2/figx_tbl6_den_ovr_x_trt.png",
    width = 6.5,
    height = 4,
    units = "in",
    res = 600)
plot.den.ovr
dev.off()