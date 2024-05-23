library(dplyr)
library(ggplot2)
library(tidyverse)
library(purrr)

# create a grid of Razorback TL and Predator TL by Zone to calculate dorso-ventral heights and gape widths
GW <- expand.grid(XYTE_TL = seq(300,600,1), SPECIES = c('MOSA', 'PYOL', 'MISA', 'MIDO'), PRED_TL = seq(100,1000,1))

# each equation corresponds to a gape-width line equation for 4 different predators: 
# MOSA(striped bass), PYOL(Flathead Catfish), MISA(Largemouth Bass), MIDO(Smallmouth Bass)
equations <- list(
  'MOSA' = function(PRED_TL) -1.502 + (0.125 * PRED_TL) - (0.000005 * PRED_TL^2),
  'PYOL' = function(PRED_TL) (123 * 2.718^(0.0008*PRED_TL)) - 119.3,
  'MISO' = function(PRED_TL) (0.14*PRED_TL) - 5.59,
  'MIDO' = function(PRED_TL) (0.13*PRED_TL) - 1.05
)

# apply equation to each Predator TL by Species, calculate dorsoventral height of XYTE, and
# calculate gapewidth to dorsoventral height ratio called GW_index
GW <- GW %>% mutate(GW = mapply(function(SPECIES, PRED_TL) equations[[SPECIES]](PRED_TL), SPECIES, PRED_TL)) %>%
  mutate(DVH = (0.14*XYTE_TL)-5.59) %>%
  mutate(GW_index = GW/DVH)

#filter for GW's between 1.19 and 1.21 (a GW_index of 1 means DV height and GW are equal so giving it some wiggle room)
  #summarize by XYTE_TL, Species, and minimum predator TL at a gape width of 1.2.
GW <- GW %>% filter(between(GW_index, 1.19, 1.21)) %>% 
  group_by(XYTE_TL, SPECIES) %>% 
  summarize(min_PRED_TL = min(PRED_TL))

#a set of 'gathers' to make joining to XYTE dataset simpler  
GW <- GW %>% mutate(Secondary = SPECIES, Sec_min_TL = min_PRED_TL) %>% 
  rename(Primary = SPECIES, Pri_min_TL = 3) %>% gather(key = 'Predator', value = 'Species', c(2,4)) %>%
  gather(key = 'Type', value = 'Min_TL', 2:3) %>% select(-Type)

#read in preprocessed XYTE data which has several columns including a binomial survivor column
XYTE <- readRDS('Processing_code/XYTE_scan_data.rds')

#create new columns for primary and secondary predators by zone
XYTE <- XYTE %>% filter(ZONE %in% c('3-1', '3-2', '2-1', '2-3')) %>% 
  mutate(Primary = case_when(ZONE == '3-1' ~ 'MOSA', 
                             ZONE == '3-2' ~ 'PYOL', 
                             ZONE == '2-1' ~ 'MOSA', 
                             ZONE == '2-3' ~ 'MIDO')) %>%
  mutate(Secondary = case_when(ZONE == '3-1' ~ 'MIDO',
                              ZONE == '3-2' ~ 'MISA',
                              ZONE == '2-1' ~ 'MIDO',
                              ZONE == '2-3' ~ 'MOSA')) %>% 
  gather(key = 'Predator', value = 'Species', Primary, Secondary)

#join the XYTE and GW columns 
temp <- XYTE %>% left_join(GW, by = c('TL' = 'XYTE_TL', 'Predator', 'Species')) %>% 
  group_by(Release_Date, PIT, TL, MSCP_REACH, ZONE, ReleaseSeason, Species, Min_TL) %>% 
  summarize(Survivor = sum(Survivor)) %>% mutate(Survivor = if_else(Survivor >0,1,0)) %>%
  filter(TL >299)

#several spreads and gathers to make processing easier
temp2 <- temp %>% spread(key = Species, value = Min_TL) 

temp2 <- temp2 %>%
  mutate(Primary = case_when(ZONE == '3-1' ~ 'MOSA', 
                             ZONE == '3-2' ~ 'PYOL', 
                             ZONE == '2-1' ~ 'MOSA', 
                             ZONE == '2-3' ~ 'MIDO')) %>%
  mutate(Secondary = case_when(ZONE == '3-1' ~ 'MIDO',
                               ZONE == '3-2' ~ 'MISA',
                               ZONE == '2-1' ~ 'MIDO',
                               ZONE == '2-3' ~ 'MOSA'))
temp2 <- temp2 %>% mutate(TL_1 = case_when(ZONE == '3-1' ~ coalesce(MOSA, PYOL),
                                           ZONE == '3-2' ~ coalesce(MOSA, PYOL),
                                           ZONE == '2-1' ~ coalesce(MOSA, PYOL),
                                           ZONE == '2-3' ~ coalesce(MIDO, MISA))) %>%
  mutate(TL_2 = case_when(ZONE == '3-1' ~ coalesce(MIDO, MISA),
                          ZONE == '3-2' ~ coalesce(MIDO, MISA),
                          ZONE == '2-1' ~ coalesce(MIDO, MISA),
                          ZONE == '2-3' ~ coalesce(MOSA, PYOL)))

#shore up final dataset and save
final <- temp2 %>% select(-MISA, -MIDO, -PYOL, -MOSA)
write_rds(final, 'For_model.rds')



