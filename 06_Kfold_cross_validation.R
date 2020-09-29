#### Joy Kumagai
#### Date: Sep. 29th 2020
#### K fold cross validation (grouped and repeated) 
#### Drones and Mangroves - post review

#### Load Packages and Data ####
library(tidyverse)
# library(caret) # needs to be explored more
data <- read.csv("Data/convolution108_v2.4_sites_all.csv")

##### Declare Functions
F.rmse <- function(m, o){
  sqrt(mean((m - o)^2, na.rm=T))
}

# ------------------------------------------------------------------------------
##### Grouped K fold cross validation #####
### Step 1 - Prepare data: 

data <- data %>% 
  # Filter areas less than 99%, 555.8 m2 was the total 
  filter(total_area > 555.8*.99) %>% 
  # Calculate percent coverage of mangrove and non mangrove area
  mutate(mangrove_percent = (mangrove_area/total_area)*100, 
         nonmangrove_percent = (nonmangrove_area/total_area)*100) 

### Step 2 - Choose k value 
sites <- unique(data$Site) # 14 unique group (site) values, to have equal number of groups k should equal 7
k <- 7

### Step 3 - Split into groups
set.seed(1)

groups <- list()
shuffled <- sample(sites, size = length(sites), replace = F) # problematic need to develop these groups better
groups[[1]] <- shuffled[1:2]
groups[[2]] <- shuffled[3:4]
groups[[3]] <- shuffled[5:6]
groups[[4]] <- shuffled[7:8]
groups[[5]] <- shuffled[9:10]
groups[[6]] <- shuffled[11:12]
groups[[7]] <- shuffled[13:14]

### Step 4 - Train and Test Model for each group
accuracy_tot <- c()
rmse_tot <- c()

for (j in 1:length(groups)) {
  # Define test and training groups
  test_sites <- groups[[j]]
  train_sites <- sites[sites != test_sites]
  
  # Histogram (U shaped distribution of mangrove area, more sights with more mangrove or very little mangrove)
  #try <- data %>% 
  #  filter(Site %in% train_sites)
  #hist(try$mangrove_area)
  
  # Calculate correction factors
  data_train <- data %>% 
    filter(Site %in% train_sites) %>% # filter data that we are using to train the model
    group_by(gridcode) %>% 
    summarise(count_train = n(),
              mangrove_p_a = mean(mangrove_percent),
              .groups = "keep") %>% 
    mutate(correction_factor = mangrove_p_a/100) # calculates correction factors
  data_train[1,4] <- 0 # this is to prevent overestimation over areas with no mangroves
  
  # Apply calculation factors
  data_test <- data %>% 
    filter(Site %in% test_sites) %>% # filter data that we are using to test the model 
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
}
data.frame(accuracy_tot, rmse_tot)

# ------------------------------------------------------------------------------
##### Repeated grouped K fold cross validation #####

# Initializing variables 
df <- data.frame(accuracy_tot = double(),
                 rmse_tot = double()) 
k <- 7

for (ii in 1:100) {

  # split into groups
  groups <- list()
  for (i in 1:k) {
    groups[[i]] <- sample(sites, size = length(sites)/k, replace = F)
  }

  # Train and Test Model for each group
  accuracy_tot <- c()
  rmse_tot <- c()
  
  for (j in 1:length(groups)) {
    # Define test and training groups
    test_sites <- groups[[j]]
    train_sites <- sites[sites != test_sites]
  
    # Calculate correction factors
    data_train <- data %>% 
      filter(Site %in% train_sites) %>% # filter data that we are using to train the model
      group_by(gridcode) %>% 
      summarise(count_train = n(),
              mangrove_p_a = mean(mangrove_percent),
              .groups = "keep") %>% 
      mutate(correction_factor = mangrove_p_a/100) # calculates correction factors
    data_train[1,4] <- 0 # this is to prevent overestimation over areas with no mangroves
  
    # Apply calculation factors
    data_test <- data %>% 
      filter(Site %in% test_sites) %>% # filter data that we are using to test the model 
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

df
hist(df$accuracy_tot)
hist(df$rmse_tot)
