# Authors: Joy Kumagai 
# Date Updated: 08/14/2020

#################### Upload Data ###############
data <- read.csv("Data/Satellite.csv")

#################### Packages #################
library(tidyverse)
library(ggpubr)
library(cowplot)

################### Functions ################

################### Figures ##################
colnames(data)[1] <- "ID"
plot1_data <- data.frame(data$Area_CONABIO, data$Area_GMW_calculated, data$ID) 
colnames(plot1_data) <- c("CONABIO", "GMW", "ID")
plot1_data <- plot1_data %>% 
  gather(key="key", value="area", CONABIO:GMW)


my_data <- data.frame(data$Area_total, data$Area_overlap, data$Area_CONABIO, data$Area_GMW_calculated)
colnames(my_data) <- c("Area_total", "Area_overlap", "Area_CONABIO", "Area_GMW")
plot1 <- ggplot(data=my_data, aes(x=Area_CONABIO, y=Area_GMW)) + 
  geom_point(alpha=0.5) +
  geom_abline(intercept = 0, color="blue", linetype="dashed", size=1) +
  geom_smooth(method = "lm") +
  labs(x="CONABIO Area (ha)", y="GMW Area (ha)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "darkgrey", size=0.3)) +
  ylim(0,100)
plot1

################## Drone figures ###############
data_drone <- read.csv("Data/Drone_4.1.csv")
# Clean Data
colnames(data_drone)[1] <- "UID"
data_drone <- data_drone %>% 
  select(UID:Area_ha_GMW) 

d2 <- data_drone %>% 
  select(UID, TotalFishnetArea_1, Area_ha_drone:Area_ha_GMW) %>% 
  rename(CONABIO = Area_ha_CONABIO, 
         GMW = Area_ha_GMW) %>% 
  gather(key = "key", value = "Area", CONABIO:GMW) %>% 
  mutate(P_difference = (Area_ha_drone - Area)/TotalFishnetArea_1) 

plot2 <- d2 %>% 
  ggplot(aes(x=Area_ha_drone, y=Area, color=key)) +
  geom_point(alpha=0.7) +
  geom_abline(intercept = 0, color="black", linetype="dashed", size=1) +
  scale_color_manual(values = c("royalblue2", "orange2")) +
  ylim(0,1.001) +
  labs(x="Drone Area (ha)", y="Satellite Area (ha)") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "darkgrey", size=0.3),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text=element_text(size=10)) +
  annotate("rect", 
           xmin= 0.575, xmax = 1.01,
           ymin = 0.10, ymax = 0.235,
           alpha = .15) +
  annotate("text", 
           x = 0.8,
           y = .2,
           label = "RMSE = 0.127", 
           color = "dark blue") +
  annotate("text", 
           x = 0.8, 
           y = .14, 
           label = "RMSE = 0.224", 
           color = "dark orange")
plot2
################## Export Figures ##############

png("Outputs/Figures_final/Figure_4.png", width = 8, height = 4, unit="in", res = 600)
ggarrange(plot1, plot2, common.legend = T, labels = c("A", "B"), legend = "right")
dev.off()

