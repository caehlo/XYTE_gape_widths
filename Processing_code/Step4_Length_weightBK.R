library(readxl)
library(tidyverse)
library(gridExtra)
library(ggplot2)

flathead <- read_csv("raw_data/flathead.csv") %>% 
  mutate(Date = as.Date(Date, "%m/%d/%Y")) %>%
  mutate(PredMass = .000006*(TL^3.0997)) #length-weight relationship from Granfors (2014)

# USGS data set Mississippi river data 1983 - 2023 
# https://www.umesc.usgs.gov/cgi-bin/ltrmp/fish/fish_query.pl
lt_flathead <- read.csv("raw_data/ltrm_fish_data_flathead_lw.csv") %>%
  rename(TL = length) %>%
  mutate(Species = "PYOL", Mass = as.integer(weight),
         Date = as.Date(sdate,  "%m/%d/%Y"), 
         Reach = NA, Agency = "USGS") %>%
  select(Date, Species, TL, Mass, Reach, Agency)

# USFWS data from Lower Colorado River E. Rasset (2023)
usfws_flathead <- read.csv("raw_data/flathead_usfws_lw.csv") %>%
  rename(TL = Length, Mass = Weight) %>%
  mutate(Species = "PYOL",
         Date = as.Date(Date,  "%m/%d/%Y"),
         Reach = 3) %>%
  select(Date, Species, TL, Mass, Reach, Agency)


others <- read_excel("raw_data/Fish.xlsx") %>% mutate(Date = as.Date(substr(Date,1,6), "%y%m%d")) %>%
  mutate(Species = case_when(Species == 'Largemouth Bass' ~ 'MISA',
                             Species == 'Smallmouth Bass' ~ 'MIDO',
                             Species == 'Striped Bass' ~ 'MOSA')) %>%
  mutate(Reach = as.numeric(gsub('Reach', '', Reach)), Agency = 'USBR') %>%
  rename('TL' = 'Total Length', 'Mass' = 'Weight') %>%
  filter(Reach %in% c('2', '3'), !(Species == 'MISA' & TL > 650))

nonnatives <- rbind(usfws_flathead, others) %>%
  filter(!is.na(TL), !is.na(Mass), TL >= 150) %>%
  mutate(LogTL = log(TL), LogMass = log(Mass))

dataList = list()
coefList = list()

for(i in unique(nonnatives$Species)){
  temp <- nonnatives %>% filter(Species == i)
  model <- lm(LogMass ~ LogTL, temp)
  temp$LogPredMass <- predict(model, newdata = temp, type = 'response')
  dataList[[i]] <- temp
  coefList[[i]] <- coef(model)
}

withMass <- bind_rows(dataList) %>%
  mutate(PredMass = exp(LogPredMass))

LWCoeff <- bind_rows(coefList, .id = "Species")
LWCoeff <- LWCoeff %>% 
  rename(SlopeTL = LogTL, LogIntercept = `(Intercept)`) %>%
  mutate(Intercept = exp(LogIntercept))

rm(dataList, coefList)

# nonnatives <- bind_rows(withMass, flathead)
saveRDS(nonnatives, file = 'nonnatives.rds')
saveRDS(LWCoeff, file = 'nonnativeslw.rds')

ggplot(withMass, aes(x = TL, color = Species)) + geom_point(aes(y = Mass)) + geom_line(aes(y = PredMass)) +
  facet_wrap(~Species, scales = 'free')

ggplot(withMass, aes(x = LogTL, color = Species)) + geom_point(aes(y = LogMass)) + geom_line(aes(y = LogPredMass)) +
  facet_wrap(~Species, scales = 'free')
