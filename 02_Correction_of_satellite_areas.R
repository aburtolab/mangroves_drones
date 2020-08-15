# Authors: Joy Kumagai, Fabio Favoretto 
# Date Updated: 08/14/2020



# Loading libraries -------------------------------------------------------

library(tidyverse)
library(furrr)
library(gt)


# Data upload -------------------------------------------------------------

data <- read.csv("Data/convolution108_v2.4_sites_all.csv")



# Data analysis -----------------------------------------------------------


data <- data %>% 
  # Step 1 - Filter areas less than 99%, 555.8 m2 was the total area of each cell
  filter(total_area > 555.8*.99) %>% 
  # Step 2 - Calculate percent coverage of mangrove and non mangrove area
  mutate(mangrove_percent = (mangrove_area/total_area)*100, 
         nonmangrove_percent = (nonmangrove_area/total_area)*100) 

  
# Step 3 - Summarize by Grid code and calculate correction factors
summary <- data %>% 
  group_by(gridcode) %>% 
  summarise(count = n(),
            mangrove_p_a = mean(mangrove_percent), 
            nonmangrove_p_a = mean(nonmangrove_percent)) %>% 
  mutate(correction_factor = mangrove_p_a/100)

summary[1,5] <- 0 # this is to prevent overestimation over areas with no mangroves



# Correction of the area estimation ---------------------------------------

corrected_areas <- summary$correction_factor*summary$count*555.8
residual_error <- (sum(corrected_areas) - sum(data$mangrove_area))/sum(data$mangrove_area)
abs(residual_error) # absolute value of the residual error after correction (i.e. difference between mangrove area measured by drone and corrected satellite areas)



# Validation: splitting into training and modeling data ------------------

## Step 1 - Split data in half and use correction factors on other half

## We want to repeat this process 1000 times to have an average residual error between these different 1000 scenarios

n <- 1000 
residual_errors <- vector(,n) #creates an empty vectors to be filled by the 1000 residual errors 


# this is for parallel processing analysis to speed up the 1000 loops
future::plan(multiprocess)

# Parallel processing of the 1000 random scenarios

residual_errors <- future_map_dbl(1:n, function(x){
  set.seed(x)
  sites_scramble <- sample(unique(data$Site), 14, replace = F)
  sites_a <- sites_scramble[1:7] # these sites will be used to create correction factors
  sites_b <- sites_scramble[8:14] # these sites will be used to check the correction factors
  
  # Calculate Correction Factors
  data_A <- data %>% 
    filter(Site %in% sites_a) %>% 
    group_by(gridcode) %>% 
    summarise(count_a = n(),
              mangrove_p_a = mean(mangrove_percent), 
              nonmangrove_p_a = mean(nonmangrove_percent)) %>% 
    mutate(correction_factor = mangrove_p_a/100)
  data_A[1,5] <- 0 # this is to prevent overestimation over areas with no mangroves
  
  # Apply Correction Factors 
  data_B <- data %>% 
    filter(Site %in% sites_b) %>% 
    group_by(gridcode) %>% 
    summarise(count_b = n(), 
              total_mangrove = sum(mangrove_area)) %>% 
    full_join(data_A, by = "gridcode") %>% 
    mutate(corrected_areas = correction_factor*count_b*555.8)
  
  residual_errors <- ((sum(data_B$corrected_areas, na.rm=TRUE) - sum(data_B$total_mangrove, na.rm = TRUE))/sum(data_B$total_mangrove, na.rm = TRUE))*100 
  
})


# Summary statistic of the residual errors
data.frame(
  `Residual error metric` = c("Mean", "Standard Deviation", "Standard Error"),
  Values = c(abs(round(mean(residual_errors), 2)), 
             round(sd(residual_errors),2), 
             round(sd(residual_errors/sqrt(length(residual_errors))),2))) %>% 
  gt()



# Plotting the residual errors as absolute values

as.data.frame(residual_errors) %>% 
  ggplot(aes(x=abs(residual_errors)))+
  geom_density(fill="darkgreen", alpha=.33)+
  geom_vline(aes(xintercept=0), size=2)+
  labs(x="Residual error (m)", y="Density", title = "Residual error distribution over 1000 trials")+
  xlim(0,25)+
  theme_classic()+
  theme(panel.border = element_rect(fill=NA))


ggsave("figs/Figure_residual_errors.jpeg", width=8, height=4, dpi=600)

# Gridcode value comparisons ----------------------------------------------

fig_data <- summary[,1:4]
colnames(fig_data) <- c("gridcode", "Count", "Mangrove", "Non-mangrove")
fig_data$gridcode <- as.factor(fig_data$gridcode)

fig_data_long <- fig_data %>% 
  gather(key = "category", value = "value", Mangrove:`Non-mangrove`)  # wide to long

fig_data_long$category <- fct_rev(fig_data_long$category)

# Plotting the values by gridcode

fig_data_long %>% 
  ggplot(aes(x = gridcode, y = value, fill = category)) +
  geom_rect(xmin = .5, xmax = 18.5, ymin = 90, ymax = 110, fill = "white") +
  geom_col(color = "grey20") +
  scale_fill_manual(values = c("beige", "green3")) +
  labs(y = "Average Coverage (%)", x = "Gridcode") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  annotate("text", x = c(1:18), y = rep(96, 18), label = fig_data$Count)

## saving the image
ggsave("figs/Figure_convolution.jpeg", width=8, height=4, dpi=600)





# END OF SCRIPT -----------------------------------------------------------


