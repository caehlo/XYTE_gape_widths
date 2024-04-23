library(readxl)
library(tidyverse)

flathead <- read_csv("rawData/flathead.csv") %>% 
  mutate(Date = as.Date(Date, "%m/%d/%Y")) %>%
  mutate(PredMass = .000006*(TL^3.0997)) #length-weight relationship from Granfors (2014)

others <- read_excel("rawData/Fish.xlsx") %>% mutate(Date = as.Date(substr(Date,1,6), "%y%m%d")) %>%
  mutate(Species = case_when(Species == 'Largemouth Bass' ~ 'MISA',
                             Species == 'Smallmouth Bass' ~ 'MIDO',
                             Species == 'Striped Bass' ~ 'MOSA')) %>%
  mutate(Reach = as.numeric(gsub('Reach', '', Reach)), Agency = 'USBR') %>%
  rename('TL' = 'Total Length', 'Mass' = 'Weight') %>%
  filter(Reach %in% c('2', '3'), !(Species == 'MISA' & TL > 650))


species = c('MIDO', 'MOSA', 'MISA')
dataList = list()

for(i in species){
  temp <- others %>% filter(Species == i)
  model <- glm(Mass ~ TL, temp, family = poisson(link = "log"))
  temp$PredMass <- predict(model, newdata = temp, type = 'response')
  dataList[[i]] <- temp
}
withMass <- bind_rows(dataList)


nonnatives <- bind_rows(withMass, flathead)
saveRDS(nonnatives, file = 'nonnatives.rds')
ggplot(nonnatives, aes(x = TL, color = Species)) + geom_point(aes(y = Mass)) + geom_line(aes(y = PredMass)) +
  facet_wrap(~Species, scales = 'free')
