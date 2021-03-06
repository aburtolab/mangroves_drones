#### Joy Kumagai
#### Date: Oct. 30th 2020
#### K fold cross validation (repeated) 
#### Drones and Mangroves Project 

#### Load Packages and Data ####
library(tidyverse)
data <- read.csv("Data/convolution108_v2.4_sites_all.csv")

##### Declare Functions
# RMSE
F.rmse <- function(m, o){
  sqrt(mean((m - o)^2, na.rm=T))
}

# Standard error
F.std <- function(x) sd(x)/sqrt(length(x))

# ------------------------------------------------------------------------------
#####  K fold cross validation #####
### Step 1 - Prepare data: 

data <- data %>% 
  # Filter areas less than 99%, 555.8 m2 was the total 
  filter(total_area > 555.8*.99) %>% 
  # Calculate percent coverage of mangrove and non mangrove area
  mutate(mangrove_percent = (mangrove_area/total_area)*100, 
         nonmangrove_percent = (nonmangrove_area/total_area)*100) 

### Step 2 - Choose k value 
sites <- unique(data$Site) 
k <- 10

### Step 3 - Group the data (not based on sites)
set.seed(2) # for reproducibility 
n <- length(data$FID_Fishnet)
ID <- 1:n
groups <- list() # list to store the groups
data$ID <- ID

for (i in 1:k) { # Assigns groups
  a <- sample(ID, size = n/k, replace = F) # samples from available rows of the data that haven't been put in a group yet
  ID <- ID[which(is.na(match(ID, a)))] # updates the available rows of the data by removing 
  groups[[i]] <- a # store the groups
}

accuracy_tot <- c()
rmse_tot <- c()

for (j in 1:k) { # loop that will train and test the model for each group 
  # Define test and training groups 
  test_samples <- data[groups[[j]], ]
  train_samples <- data[which(is.na(match(data$ID, test_samples$ID))),]
  
  # Calculate correction factors
  data_train <- train_samples %>% 
    group_by(gridcode) %>% 
    summarise(count_train = n(),
              mangrove_p_a = mean(mangrove_percent),
              .groups = "keep") %>% 
    mutate(correction_factor = mangrove_p_a/100) # calculates correction factors
  data_train[1,4] <- 0 # this is to prevent overestimation over areas with no mangroves
  
  # Apply calculation factors
  data_test <- test_samples %>% # filter data that we are using to test the model 
    group_by(gridcode) %>% 
    summarise(count_test = n(),
              total_mangrove = sum(mangrove_area), # real mangrove area
              .groups = "keep") %>% 
    full_join(data_train, by = "gridcode") %>% # join correction factors 
    mutate(estimated_areas = correction_factor*count_test*555.8) # uses correction factor to estimates area
  
  accuracy <- ((sum(data_test$estimated_areas, na.rm=TRUE) - sum(data_test$total_mangrove, na.rm = TRUE))/sum(data_test$total_mangrove, na.rm = TRUE))*100 # Calculates accuracy 
  accuracy_tot <- c(accuracy_tot, accuracy)
  rmse_test <- F.rmse(m = data_test$estimated_areas, o = data_test$total_mangrove) # calculates RMSE
  rmse_tot <- c(rmse_tot, rmse_test)
}
data.frame(accuracy_tot, rmse_tot)
# ------------------------------------------------------------------------------
##### Repeated K fold cross validation ##### 

# Initializing variables 
df <- data.frame(accuracy_tot = double(),
                 rmse_tot = double()) 
k <- 10

for (ii in 1:1000) { # does the same as above but 100 times 

  # split into groups
  ID <- 1:n
  groups <- list()
  data$ID <- ID
  for (i in 1:k) {
    a <- sample(ID, size = n/k, replace = F)
    ID <- ID[which(is.na(match(ID, a)))]
    groups[[i]] <- a
  }

  # Train and Test Model for each group
  accuracy_tot <- c()
  rmse_tot <- c()
  
  for (j in 1:k) {
    # Define test and training groups 
    test_samples <- data[groups[[j]], ]
    train_samples <- data[which(is.na(match(data$ID, test_samples$ID))),]
    
    # Calculate correction factors
    data_train <- train_samples %>% 
      group_by(gridcode) %>% 
      summarise(count_train = n(),
                mangrove_p_a = mean(mangrove_percent),
                .groups = "keep") %>% 
      mutate(correction_factor = mangrove_p_a/100) # calculates correction factors
    data_train[1,4] <- 0 # this is to prevent overestimation over areas with no mangroves
    
    # Apply calculation factors
    data_test <- test_samples %>% # filter data that we are using to test the model 
      group_by(gridcode) %>% 
      summarise(count_test = n(),
                total_mangrove = sum(mangrove_area), # real mangrove area
                .groups = "keep") %>% 
      full_join(data_train, by = "gridcode") %>% # join correction factors 
      mutate(estimated_areas = correction_factor*count_test*555.8) # uses correction factor to estimates area
    
    accuracy <- ((sum(data_test$estimated_areas, na.rm=TRUE) - sum(data_test$total_mangrove, na.rm = TRUE))/sum(data_test$total_mangrove, na.rm = TRUE))*100 
    accuracy_tot <- c(accuracy_tot, accuracy)
    rmse_test <- F.rmse(m = data_test$estimated_areas, o = data_test$total_mangrove)
    rmse_tot <- c(rmse_tot, rmse_test)
    
    df_a <- data.frame(accuracy_tot, rmse_tot)
  }

  df <- rbind(df, df_a)
  print(ii)
}

###### Review of Analysis #####
hist(df$rmse_tot)

mean(abs(df$accuracy_tot)) # on average the estimated areas area is 1.7% different than the true area (drone identified)
sd(df$accuracy_tot) # standard deviation

plot_accuracy <- df %>% 
  ggplot(aes(x = accuracy_tot)) +
  geom_histogram(color = "black", 
                 fill = "grey", 
                 bins = 20) +
  theme_minimal() +
  labs(x = "Accuracy of estimated mangrove extent (%)",
       y = "Count") +
  theme(axis.line = element_line(colour = "darkgrey", 
                                 size=0.3)) +
  scale_x_continuous(breaks = c(-6,-4,-2,0,2,4,6))

##### Export #####
png("Outputs/Figures/k_fold_accuracy.png", width = 6, height = 4, unit="in", res = 600)
plot_accuracy
dev.off()