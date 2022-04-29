####################################################################################
##SEASONALITY OF SOUTHERN APPALACHIAN FIRE BEHAVIOR 
##R code for plotting midstory vegetation response by treatment
##Density, management species of interest by DBH class
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
den.mid.acerrubr <- read_excel("input/den_mid_acerrubr_p.xlsx")
den.mid.acerrubr <- data.frame(den.mid.acerrubr)
den.mid.kalmlati <- read_excel("input/den_mid_kalmlati_p.xlsx")
den.mid.kalmlati <- data.frame(den.mid.kalmlati)

#join data tables#
den.mid.spp <- den.mid.acerrubr %>%
  select(-(DID:UID)) %>%
  full_join(den.mid.kalmlati, by = "PID") %>%
  select(-(DID:UID)) %>%
  mutate(DID = substr(PID, start = 1, stop = 2),
         RID = substr(PID, start = 1, stop = 3),
         Trt = substr(PID, start = 4, stop = 4),
         UID = substr(PID, start = 1, stop = 4)) %>%
  relocate(DID:UID)

#assign list of variables by species/life history category#
dbh.den.mid.spp <- list("delta_001_acerrubr_perha",
                        "delta_002_acerrubr_perha",
                        "delta_003_acerrubr_perha",
                        "delta_all_acerrubr_perha",
                        
                        "delta_001_kalmlati_perha",
                        "delta_002_kalmlati_perha",
                        "delta_003_kalmlati_perha",
                        "delta_all_kalmlati_perha")

##summarize response variables by mean and standard error##
#load summarySE function
#summarySE provides the standard deviation, standard error of the mean, and a default 95% confidence interval

#make list of summary tables by response variable#
delta.dbh.den.mid.spp.trt.lx <- lapply(dbh.den.mid.spp, function(var){
  df <- summarySE(den.mid.spp, measurevar = var, groupvars = "Trt", na.rm = TRUE) #returns data frame
  df$spp <- substr(var, start = 11, stop = 18)
  df$dbh <- substr(var, start = 7, stop = 9)
  df$mean_perha <- df[,3]
  df <- df[,c(1, 2, 7, 8, 9, 4, 5, 6)]
  assign(paste("delta.",
               substr(deparse(substitute(var)), start = 7, stop = 18),
               ".trt",
               sep=""),
         df)
})
#add significance comparsion letters
delta.dbh.den.mid.spp.trt.lx[[1]]$sig <- c("a", "ab", "b") #001_acerrubr
delta.dbh.den.mid.spp.trt.lx[[2]]$sig <- c("a", "b", "b") #002_acerrubr
delta.dbh.den.mid.spp.trt.lx[[3]]$sig <- NA
delta.dbh.den.mid.spp.trt.lx[[4]]$sig <- c("a", "a", "b") #all_acerrubr
delta.dbh.den.mid.spp.trt.lx[[5]]$sig <- c("a", "ab", "b") #001_kalmlati
delta.dbh.den.mid.spp.trt.lx[[6]]$sig <- NA
delta.dbh.den.mid.spp.trt.lx[[7]]$sig <- NA
delta.dbh.den.mid.spp.trt.lx[[8]]$sig <- NA

#bind to new data frame
delta.dbh.den.mid.spp.trt <- rbind(delta.dbh.den.mid.spp.trt.lx[[1]],
                                   delta.dbh.den.mid.spp.trt.lx[[2]],
                                   delta.dbh.den.mid.spp.trt.lx[[3]],
                                   delta.dbh.den.mid.spp.trt.lx[[4]],
                                   delta.dbh.den.mid.spp.trt.lx[[5]],
                                   delta.dbh.den.mid.spp.trt.lx[[6]],
                                   delta.dbh.den.mid.spp.trt.lx[[7]],
                                   delta.dbh.den.mid.spp.trt.lx[[8]])

#create a vector of species names for facet labels
spp_names <- c(`acerrubr` = "Acer rubrum",
               `kalmlati` = "Kalmia latifolia")

#specify x-axis labels as factor levels
delta.dbh.den.mid.spp.trt$dbh <- fct_inorder(as.factor(delta.dbh.den.mid.spp.trt$dbh))

##plot response variable by treatment as means with standard error##
plot.den.mid.spp <- ggplot(delta.dbh.den.mid.spp.trt, aes(x = dbh, y = mean_perha, fill = Trt)) +
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
  scale_y_continuous(labels = comma) +
  scale_fill_manual(name = "Treatment",
                    breaks = c("c", "d", "g"),
                    labels = c("Unburned control", "Dormant season", "Growing season"),
                    values = c("gray50", "sienna4", "green4")) +
  facet_wrap(vars(fct_inorder(as.factor(spp))),
             nrow = 4,
             ncol = 2,
             scales = "free_x",
             labeller = as_labeller(spp_names),
             strip.position = "bottom") +
  labs(title = "Midstory",
       x = "Species by DBH class",
       #y = "Mean \u0394 density (100,000 ha\u207B\u00B9)") +
       y = "Mean \u0394 density (per ha)") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", color = "black", size = 16),
        axis.text.x = element_text(size = 9, angle = 0, vjust = 0),
        axis.title.x = element_text(face = "bold", color = "black", size = 14),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(face = "bold", color = "black", size = 14),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(face = "italic", color = "grey20", size = 11),
        #strip.switch.pad.wrap = unit(-0.15, "cm"),
        legend.position = "top",
        legend.title = element_text(color = "black", size = 12),
        legend.text = element_text(color = "black", size = 10),
        legend.key.width = unit(2, "line"),
        legend.background = element_rect(fill = "gray90"),
        legend.margin = margin(t = 4, r = 4, b = 4, l = 4),
        legend.box.margin = margin(t = -2.5, r = 0, b = 0, l = 0),
        legend.box.spacing = unit(0.15, "cm"))
plot.den.mid.spp
#save to file
png("output/figures/ms 2/figx_tbl5b_den_mid_spp_x_trt.png",
    width = 6.5,
    height = 4.5,
    units = "in",
    res = 600)
plot.den.mid.spp
dev.off()