####################################################################################
##SEASONALITY OF SOUTHERN APPALACHIAN FIRE BEHAVIOR 
##R code for plotting understory vegetation response by treatment
##Density, management species of interest by stem origin
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
library(grid)
library(gridExtra)
library(cowplot)
library(ggpubr)
library(scales)
library(naniar)
library(data.table)
library(forcats)

#import raw data from Excel worksheet#
den.und.acerrubr <- read_excel("input/den_und_acerrubr_p.xlsx")
den.und.acerrubr <- data.frame(den.und.acerrubr)
den.und.kalmlati <- read_excel("input/den_und_kalmlati_p.xlsx")
den.und.kalmlati <- data.frame(den.und.kalmlati)

#join data tables#
den.und.spp <- den.und.acerrubr %>%
  select(-(DID:UID)) %>%
  full_join(den.und.kalmlati, by = "PID") %>%
  select(-(DID:UID)) %>%
  mutate(DID = substr(PID, start = 1, stop = 2),
         RID = substr(PID, start = 1, stop = 3),
         Trt = substr(PID, start = 4, stop = 4),
         UID = substr(PID, start = 1, stop = 4)) %>%
  relocate(DID:UID)

#assign list of variables by species/life history category#
cat.den.und.spp <- list("delta_grm_acerrubr_perha",
                        "delta_est_acerrubr_perha",
                        "delta_spr_acerrubr_perha",
                        "delta_all_acerrubr_perha",
                        
                        "delta_grm_kalmlati_perha",
                        "delta_est_kalmlati_perha",
                        "delta_spr_kalmlati_perha",
                        "delta_all_kalmlati_perha")

##summarize response variables by mean and standard error##
#load summarySE function
#summarySE provides the standard deviation, standard error of the mean, and a default 95% confidence interval

#make list of summary tables by response variable#
delta.cat.den.und.spp.trt.lx <- lapply(cat.den.und.spp, function(var){
  df <- summarySE(den.und.spp, measurevar = var, groupvars = "Trt", na.rm = TRUE) #returns data frame
  df$spp <- substr(var, start = 11, stop = 18)
  df$cat <- substr(var, start = 7, stop = 9)
  df$mean_perha <- df[,3]
  df <- df[,c(1, 2, 7, 8, 9, 4, 5, 6)]
  assign(paste("delta.",
               substr(deparse(substitute(var)), start = 7, stop = 18),
               ".trt",
               sep=""),
         df)
})
#add significance comparsion letters
delta.cat.den.und.spp.trt.lx[[1]]$sig <- NA
delta.cat.den.und.spp.trt.lx[[2]]$sig <- c("a", "ab", "b") #est_acerrubr
delta.cat.den.und.spp.trt.lx[[3]]$sig <- NA
delta.cat.den.und.spp.trt.lx[[4]]$sig <- NA
delta.cat.den.und.spp.trt.lx[[5]]$sig <- NA
delta.cat.den.und.spp.trt.lx[[6]]$sig <- NA
delta.cat.den.und.spp.trt.lx[[7]]$sig <- NA
delta.cat.den.und.spp.trt.lx[[8]]$sig <- NA

#bind to new data frame
delta.cat.den.und.spp.trt <- rbind(delta.cat.den.und.spp.trt.lx[[1]],
                                   delta.cat.den.und.spp.trt.lx[[2]],
                                   delta.cat.den.und.spp.trt.lx[[3]],
                                   delta.cat.den.und.spp.trt.lx[[4]],
                                   delta.cat.den.und.spp.trt.lx[[5]],
                                   delta.cat.den.und.spp.trt.lx[[6]],
                                   delta.cat.den.und.spp.trt.lx[[7]],
                                   delta.cat.den.und.spp.trt.lx[[8]])

#create a vector of species names for facet labels
spp_names <- c(`acerrubr` = "Acer rubrum",
               `kalmlati` = "Kalmia latifolia")

#specify x-axis labels as factor levels
delta.cat.den.und.spp.trt$cat <- fct_inorder(as.factor(delta.cat.den.und.spp.trt$cat))

##plot response variable by treatment as means with standard error##
plot.den.und.spp <- ggplot(delta.cat.den.und.spp.trt, aes(x = cat, y = mean_perha, fill = Trt)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = mean_perha - se, ymax = mean_perha + se),
                size = 0.4,
                width = 0.2,
                position = position_dodge(width = 0.9)) +
  geom_text(aes(label = sig, y = ifelse(mean_perha >= 0, mean_perha + se, mean_perha - se),
                vjust = ifelse(mean_perha >= 0, -0.625, 1.5)),
            size = 3,
            position = position_dodge(width = 0.9)) +
  scale_x_discrete(breaks = c("grm", "est", "spr", "all"),
                   labels = c("Germinant", "Established", "Sprout", "All")) +
  #scale_y_continuous(labels = function(x)x*0.00001) +
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
  labs(title = "Understory",
       x = "Species by stem origin",
       #y = "Mean \u0394 density (100,000 ha\u207B\u00B9)") +
       y = "Mean \u0394 density (per ha)") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", color = "black", size = 16),
        axis.text.x = element_text(size = 9, angle = 15, vjust = 0.625),
        axis.title.x = element_text(face = "bold", color = "black", size = 14),
        axis.text.y = element_text(size = 11),
        axis.title.y = element_text(face = "bold", color = "black", size = 14),
        strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(face = "italic", color = "grey20", size = 11),
        strip.switch.pad.wrap = unit(-0.15, "cm"),
        legend.position = "top",
        legend.title = element_text(color = "black", size = 12),
        legend.text = element_text(color = "black", size = 10),
        legend.key.width = unit(2, "line"),
        legend.background = element_rect(fill = "gray90"),
        legend.margin = margin(t = 4, r = 4, b = 4, l = 4),
        legend.box.margin = margin(t = -2.5, r = 0, b = 0, l = 0),
        legend.box.spacing = unit(0.15, "cm"))
plot.den.und.spp
#save to file
png("output/figures/ms 2/fig07_den_und_spp_trt.png",
    width = 6.5,
    height = 4.5,
    units = "in",
    res = 600)
plot.den.und.spp
dev.off()