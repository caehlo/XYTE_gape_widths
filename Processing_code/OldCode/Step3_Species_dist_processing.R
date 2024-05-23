library(tidyverse)
library(ggplot2)
library(truncnorm)
library(dplyr)
library(ggpubr)

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

hist <- ggplot(dist, aes(x = TotalLength, fill = Species)) + 
  geom_histogram() + facet_grid(Species ~ Zone) + labs()
hist

distsummary <- dist %>% group_by(Zone, Species) %>% 
  summarize(n = n(), 
            MeanTL = mean(TotalLength),
            Variance = var(TotalLength))

#read in XYTE scan data
data <- readRDS('Processing_code/For_model.rds')

datatest1 <- data %>%
  ungroup() %>%
  select(Release_Date, PIT, Primary, ZONE, TL_1) %>%
  left_join(dist, by = c("Primary" = "Species", "ZONE" = "Zone"), relationship = "many-to-many") %>%
  filter(TotalLength >= TL_1) %>%
  group_by(Release_Date, PIT, ZONE) %>%
  summarise(PrimaryMeanTL = mean(TotalLength),
            PrimaryVarianceTL = var(TotalLength),
            PrimaryN = n()) %>%
  ungroup()

datatest2 <- data %>%
  ungroup() %>%
  select(Release_Date, PIT, Secondary, ZONE, TL_2) %>%
  left_join(dist, by = c("Secondary" = "Species", "ZONE" = "Zone"), relationship = "many-to-many") %>%
  filter(TotalLength >= TL_2) %>%
  group_by(Release_Date, PIT, ZONE) %>%
  summarise(SecondaryMeanTL = mean(TotalLength),
            SecondaryVarianceTL = var(TotalLength),
            SecondaryN = n()) %>%
  ungroup()

datadump <- data %>%
  left_join(datatest1 %>% 
               select(Release_Date, PIT, ZONE, PrimaryMeanTL, PrimaryVarianceTL, PrimaryN),
             by = c("Release_Date", "PIT", "ZONE")) %>%
  left_join(datatest2 %>% 
               select(Release_Date, PIT, ZONE, SecondaryMeanTL, SecondaryVarianceTL, SecondaryN),
             by = c("Release_Date", "PIT", "ZONE"))
               
               
  
  
  
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
data <- readRDS('Processing_code/final_dataset.rds')
boxplot_mean <- ggplot(data, aes(x = factor(Survivor), y = mean)) + 
  geom_boxplot(aes(fill = ZONE)) + labs(x = 'Survived', y = 'Mean Predator TL (mm)') +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.title.y = element_text(size=9),
        plot.margin = margin(t = 0.1, r = 0.1, b = 0.3, l = 0.2, unit = 'cm'), axis.text.y = element_text(size = 8))
boxplot_var <- ggplot(data, aes(x = factor(Survivor), y = var)) + 
  geom_boxplot(aes(fill = ZONE)) + labs(x = 'Survived', y = 'Predator TL variance (mm)') +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.title.y = element_text(size=9),
        plot.margin = margin(t = 0.1, r = 0.1, b = 0.3, l = 0.1, unit = 'cm'), axis.text.y = element_text(size = 8))
boxplot_n <- ggplot(data, aes(x = factor(Survivor), y = n)) + 
  geom_boxplot(aes(fill = ZONE)) + labs(x = 'Survived', y = 'Number of Predators (mm)') +
  theme(axis.title.y = element_text(size=9), plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = 0.2, unit = 'cm'),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 8))

all <- ggarrange(boxplot_mean, boxplot_var, boxplot_n, ncol = 1, nrow = 3, 
                 common.legend = TRUE, legend = 'bottom')
all
