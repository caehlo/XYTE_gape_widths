library(tidyverse)
library(ggplot2)
library(truncnorm)
library(dplyr)

#summary stats to create distributions for nonnatives
func_data <- data.frame(Species = c('PYOL', 'MISA', 'MIDO', 'MOSA'),
                        mu = c(376, 279, 296, 371), 
                        sigma = c(400, 169, 256, 196),
                        min = 100,
                        max = c(1000, 700, 700, 1000))

#function to create nonnative size distributions
generate_TLs <- function(n, mu, sigma, species, min, max) {
  set.seed(123)  # Set the seed for reproducibility
  
  # Generate random Total Lengths with a log-normal distribution
  total_lengths <- rtruncnorm(n, a=min, b=max, mean = mu, sd = sigma)
  
  # Create a dataframe
  fish_data <- data.frame(
    Species = species,
    TotalLength = total_lengths
  )
  
  return(fish_data)
}

# Define the data for each zone
zone_data <- data.frame(
  Zone = c('2-1', '2-1', '2-3', '2-3', '3-1', '3-1', '3-2', '3-2'),
  Species = c('MOSA', 'MIDO', 'MIDO', 'MOSA', 'MOSA', 'MIDO', 'MISA', 'PYOL'),
  Count = c(1000, 500, 1000, 500, 1000, 500, 1000, 500)
)


# Initialize an empty list to store the results
result_list <- list()

# Loop through each zone and generate Total Length data
for (i in 1:nrow(zone_data)) {
  zone <- zone_data$Zone[i]
  species <- zone_data$Species[i]
  count <- zone_data$Count[i]
  
  # Extract the parameters for the species from func_data
  species_params <- func_data[func_data$Species == species, ]
  
  # Generate Total Length data for the current zone and species
  fish_data <- generate_TLs(n = count, mu = species_params$mu, sigma = species_params$sigma, 
                            species = species, min = species_params$min, max = species_params$max)
  
  # Add the zone information
  fish_data$Zone <- zone
  
  # Append the results to the list
  result_list <- append(result_list, list(fish_data))
}

# Combine the resulting dataframes into one dataframe
dist <- do.call(rbind, result_list)

#read in XYTE scan data
data <- readRDS('For_model.rds')

#create empty dataframe for results
calc <- data.frame(Mean = numeric(),
                   Variance = numeric(),
                   n = numeric())
#loop to calculate mean, variance, and number of nonnatives that can consume each individual XYTE
  #This is based on the minimum size TL of each predator that can consume each individual XYTE dependent on Zone
for(i in 1:nrow(data)){
  filtered <- filter(dist, Zone == data$ZONE[i] & (Species == data$Primary[i] | Species == data$Secondary[i]) & 
           (TotalLength >= data$TL_1[i]) | TotalLength >= data$TL_2[i]) #filters distribution by individual XYTE
  mean <- mean(filtered$TotalLength) #calculates mean of resulting distribution
  var <- var(filtered$TotalLength) #calcualtes variance of resulting distribution
  n <- nrow(filtered) #calculates the number of nonnatives in resulting distribution
  temp <- data.frame(mean, var, n) #puts the three parameters into a temporary folder
  calc <- rbind(calc, temp) #binds to the empty dataframe
}

data <- cbind(data, calc) #binds the results to the XYTE dataframe

write_rds(data, 'final_dataset.rds')
boxplot_mean <- ggplot(data, aes(x = factor(Survivor), y = mean)) + 
  geom_boxplot(aes(fill = ZONE)) + labs(x = 'Survived', y = 'Mean Predator TL (mm)')
boxplot_var <- ggplot(data, aes(x = factor(Survivor), y = var)) + 
  geom_boxplot(aes(fill = ZONE)) + labs(x = 'Survived', y = 'Variance Predator TL (mm)')
boxplot_n <- ggplot(data, aes(x = factor(Survivor), y = n)) + 
  geom_boxplot(aes(fill = ZONE)) + labs(x = 'Survived', y = 'Number of Predators (mm)')

