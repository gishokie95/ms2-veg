####################################################################################
##SEASONALITY OF SOUTHERN APPALACHIAN FIRE BEHAVIOR 
##R code for plotting midstory vegetation response by treatment
##Density, tree by group and DBH class
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
library(grid)
library(gridExtra)
library(cowplot)
library(ggpubr)
library(scales)
library(naniar)
library(data.table)
library(forcats)

#import raw data from Excel worksheet#
den.mid.tre <- read_excel("input/den_mid_tree_wdbh_p.xlsx")
den.mid.tre <- data.frame(den.mid.tre)

#assign list of variables by group/life history category#
dbh.den.mid.tre <- list("delta_001_hickory_perha",
                        "delta_002_hickory_perha",
                        "delta_003_hickory_perha",
                        "delta_all_hickory_perha",
                        
                        "delta_001_mesohwd_perha",
                        "delta_002_mesohwd_perha",
                        "delta_003_mesohwd_perha",
                        "delta_all_mesohwd_perha",
                        
                        "delta_001_redoaks_perha",
                        "delta_002_redoaks_perha",
                        "delta_003_redoaks_perha",
                        "delta_all_redoaks_perha",
                        
                        "delta_001_whteoak_perha",
                        "delta_002_whteoak_perha",
                        "delta_003_whteoak_perha",
                        "delta_all_whteoak_perha",
                        
                        "delta_001_whtepin_perha",
                        "delta_002_whtepin_perha",
                        "delta_003_whtepin_perha",
                        "delta_all_whtepin_perha",
                        
                        "delta_001_yellpin_perha",
                        "delta_002_yellpin_perha",
                        "delta_003_yellpin_perha",
                        "delta_all_yellpin_perha",
                        
                        "delta_001_allothr_perha",
                        "delta_002_allothr_perha",
                        "delta_003_allothr_perha",
                        "delta_all_allothr_perha")

##summarize response variables by mean and standard error##
#load summarySE function
#summarySE provides the standard deviation, standard error of the mean, and a default 95% confidence interval

#make list of summary tables by response variable#
delta.dbh.den.mid.tre.trt.lx <- lapply(dbh.den.mid.tre, function(var){
  df <- summarySE(den.mid.tre, measurevar = var, groupvars = "Trt", na.rm = TRUE) #returns data frame
  df$tfg <- substr(var, start = 11, stop = 17)
  df$dbh <- substr(var, start = 7, stop = 9)
  df$mean_perha <- df[,3]
  df <- df[,c(1, 2, 7, 8, 9, 4, 5, 6)]
  assign(paste("delta.",
               substr(deparse(substitute(var)), start = 7, stop = 17),
               ".trt",
               sep=""),
         df)
})
#add significance comparsion letters
delta.dbh.den.mid.tre.trt.lx[[1]]$sig <- NA
delta.dbh.den.mid.tre.trt.lx[[2]]$sig <- NA
delta.dbh.den.mid.tre.trt.lx[[3]]$sig <- NA
delta.dbh.den.mid.tre.trt.lx[[4]]$sig <- NA
delta.dbh.den.mid.tre.trt.lx[[5]]$sig <- NA
delta.dbh.den.mid.tre.trt.lx[[6]]$sig <- c("a", "b", "c") #002_mesohwd
delta.dbh.den.mid.tre.trt.lx[[7]]$sig <- NA
delta.dbh.den.mid.tre.trt.lx[[8]]$sig <- c("a", "ab", "b") #all_mesohwd
delta.dbh.den.mid.tre.trt.lx[[9]]$sig <- NA
delta.dbh.den.mid.tre.trt.lx[[10]]$sig <- c("a", "b", "ab") #002_redoaks
delta.dbh.den.mid.tre.trt.lx[[11]]$sig <- NA
delta.dbh.den.mid.tre.trt.lx[[12]]$sig <- c("a", "ab", "b") #all_redoaks
delta.dbh.den.mid.tre.trt.lx[[13]]$sig <- NA
delta.dbh.den.mid.tre.trt.lx[[14]]$sig <- c("a", "b", "ab") #002_whteoak
delta.dbh.den.mid.tre.trt.lx[[15]]$sig <- NA
delta.dbh.den.mid.tre.trt.lx[[16]]$sig <- NA
delta.dbh.den.mid.tre.trt.lx[[17]]$sig <- NA
delta.dbh.den.mid.tre.trt.lx[[18]]$sig <- NA
delta.dbh.den.mid.tre.trt.lx[[19]]$sig <- NA
delta.dbh.den.mid.tre.trt.lx[[20]]$sig <- NA
delta.dbh.den.mid.tre.trt.lx[[21]]$sig <- NA
delta.dbh.den.mid.tre.trt.lx[[22]]$sig <- NA
delta.dbh.den.mid.tre.trt.lx[[23]]$sig <- NA
delta.dbh.den.mid.tre.trt.lx[[24]]$sig <- NA
delta.dbh.den.mid.tre.trt.lx[[25]]$sig <- NA
delta.dbh.den.mid.tre.trt.lx[[26]]$sig <- c("a", "ab", "b") #002_allothr
delta.dbh.den.mid.tre.trt.lx[[27]]$sig <- NA
delta.dbh.den.mid.tre.trt.lx[[28]]$sig <- NA

#bind to new data frame
delta.dbh.den.mid.tre.trt <- rbind(delta.dbh.den.mid.tre.trt.lx[[1]],
                                   delta.dbh.den.mid.tre.trt.lx[[2]],
                                   delta.dbh.den.mid.tre.trt.lx[[3]],
                                   delta.dbh.den.mid.tre.trt.lx[[4]],
                                   delta.dbh.den.mid.tre.trt.lx[[5]],
                                   delta.dbh.den.mid.tre.trt.lx[[6]],
                                   delta.dbh.den.mid.tre.trt.lx[[7]],
                                   delta.dbh.den.mid.tre.trt.lx[[8]],
                                   delta.dbh.den.mid.tre.trt.lx[[9]],
                                   delta.dbh.den.mid.tre.trt.lx[[10]],
                                   delta.dbh.den.mid.tre.trt.lx[[11]],
                                   delta.dbh.den.mid.tre.trt.lx[[12]],
                                   delta.dbh.den.mid.tre.trt.lx[[13]],
                                   delta.dbh.den.mid.tre.trt.lx[[14]],
                                   delta.dbh.den.mid.tre.trt.lx[[15]],
                                   delta.dbh.den.mid.tre.trt.lx[[16]],
                                   delta.dbh.den.mid.tre.trt.lx[[17]],
                                   delta.dbh.den.mid.tre.trt.lx[[18]],
                                   delta.dbh.den.mid.tre.trt.lx[[19]],
                                   delta.dbh.den.mid.tre.trt.lx[[20]],
                                   delta.dbh.den.mid.tre.trt.lx[[21]],
                                   delta.dbh.den.mid.tre.trt.lx[[22]],
                                   delta.dbh.den.mid.tre.trt.lx[[23]],
                                   delta.dbh.den.mid.tre.trt.lx[[24]],
                                   delta.dbh.den.mid.tre.trt.lx[[25]],
                                   delta.dbh.den.mid.tre.trt.lx[[26]],
                                   delta.dbh.den.mid.tre.trt.lx[[27]],
                                   delta.dbh.den.mid.tre.trt.lx[[28]])

#create a vector of growth habit names for facet labels
tfg_names <- c(`hickory` = "Hickory",
               `mesohwd` = "Mesophytic hardwood",
               `redoaks` = "Red oak",
               `whteoak` = "White oak",
               `whtepin` = "White pine",
               `yellpin` = "Yellow pine",
               `allothr` = "Other")

#specify x-axis labels as factor levels
delta.dbh.den.mid.tre.trt$dbh <- fct_inorder(as.factor(delta.dbh.den.mid.tre.trt$dbh))

##plot response variable by treatment as means with standard error##
plot.den.mid.tre <- ggplot(delta.dbh.den.mid.tre.trt, aes(x = dbh, y = mean_perha, fill = Trt)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = mean_perha - se, ymax = mean_perha + se),
                size = 0.4,
                width = 0.2,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(label = sig, y = ifelse(mean_perha >= 0, mean_perha + se, mean_perha - se),
                vjust = ifelse(mean_perha >= 0, -0.625, 1.5)),
            size = 3,
            position = position_dodge(width = 0.9)) +
  scale_x_discrete(breaks = c("001", "002", "003", "all"),
                   labels = c("<3 cm", "3-6 cm", "6-10 cm", "All")) +
  scale_y_continuous(limits = c(-715, NA), labels = comma) +
  scale_fill_manual(name = "Treatment",
                    breaks = c("c", "d", "g"),
                    labels = c("Unburned control", "Dormant season", "Growing season"),
                    values = c("gray50", "sienna4", "green4")) +
  facet_wrap(vars(fct_inorder(as.factor(tfg))),
             nrow = 4,
             ncol = 2,
             scales = "free_x",
             labeller = as_labeller(tfg_names),
             strip.position = "bottom") +
  labs(title = "Midstory",
       x = "Tree by group and DBH class",
       #y = "Mean \u0394 density (100,000 ha\u207B\u00B9)") +
       y = "Mean \u0394 density (per ha)") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", color = "black", size = 16),
        axis.text.x = element_text(size = 8, angle = 0, vjust = 0),
        axis.title.x = element_text(face = "bold", color = "black", size = 14),
        axis.text.y = element_text(size = 9),
        axis.title.y = element_text(face = "bold", color = "black", size = 14),
        #panel.spacing = unit(0, "cm"),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(color = "grey20", size = 12),
        #legend.position = "none",
        legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 12),
        legend.key.width = unit(2, "line"),
        legend.background = element_rect(fill = "gray90"),
        legend.margin = margin(t = 4, r = 4, b = 4, l = 4))
plot.den.mid.tre
grid.draw(shift_legend(plot.den.mid.tre))
#save to file
png("output/figures/ms 2/figx_tbl5a_pnl_den_mid_tre_x_trt.png",
    width = 6.5,
    height = 9,
    units = "in",
    res = 600)
grid.draw(shift_legend(plot.den.mid.tre))
dev.off()