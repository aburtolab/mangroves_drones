# Authors: Joy Kumagai, Fabio Favoretto 
# Date Updated: 08/14/2020

##### Load Packages #####
library(tidyverse)

##### Upload Data #####
data <- read.csv("Data/convolution108_v2.4_sites_all.csv")

##### Analysis #####
# Step 1 - Filter areas less than 99%, 555.8 m2 was the total area of each cell
limit_99 <- 555.8*.99
data <- data %>% 
  filter(total_area > limit_99)

# Step 2 - Calculate percent coverage of mangrove and non mangrove area
data <- data %>% 
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

##### Validation ##### 
## Method 1 - Compare calculated area to original area

original_areas <- summary$correction_factor*summary$count*555.8
accuracy <- sum(original_areas) / sum(data$mangrove_area)
accuracy # 99.97%

## Method 2 - Split data in half and use correction factors on other half
## Do it 1000 times
n <- 1000
method2 <- vector(,n)
set.seed(1)
for (i in 1:n) {
print(i)
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
  mutate(estimated_areas = correction_factor*count_b*555.8)

accuracy_2 <- ((sum(data_B$estimated_areas, na.rm=TRUE) - sum(data_B$total_mangrove, na.rm = TRUE))/sum(data_B$total_mangrove, na.rm = TRUE))*100 

method2[i] <- accuracy_2
}

hist(method2) # Distribution of errors

range(method2)

sd(method2)/sqrt(length(method2)) # standard error

abs(mean(method2))

as.data.frame(method2) %>% 
  ggplot(aes(x=method2))+
  geom_density(fill="darkgreen", alpha=.33)+
  geom_vline(aes(xintercept=0), size=2)+
  labs(x="Error values", y="Density", title = "Area error distribution over 1000 trials")+
  theme_classic()+
  theme(panel.border = element_rect(fill=NA))




##### Figure #####
fig_data <- summary[,1:4]
colnames(fig_data) <- c("gridcode", "Count", "Mangrove", "Non-mangrove")
fig_data$gridcode <- as.factor(fig_data$gridcode)

fig_data_long <- fig_data %>% 
  gather(key = "category", value = "value", Mangrove:`Non-mangrove`)  # wide to long

fig_data_long$category <- fct_rev(fig_data_long$category)
plot5 <- fig_data_long %>% 
  ggplot(aes(x = gridcode, y = value, fill = category)) +
  geom_rect(xmin = .5, xmax = 18.5, ymin = 90, ymax = 110, fill = "white") +
  geom_col(color = "grey20") +
  scale_fill_manual(values = c("beige", "green3")) +
  labs(y = "Average Coverage (%)", x = "Gridcode") +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "bottom") +
  annotate("text", x = c(1:18), y = rep(96, 18), label = fig_data$Count)

plot5

##### Export #####
#png("Outputs/Figures_final/Figure5_convolution_updated.png", width=8, height=4, units = "in", res=600)
#plot5
#dev.off()
