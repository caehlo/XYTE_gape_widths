source("Processing_code/XYTE_body_depth.R")
source("Processing_code/Length_weightBK.R")
source("Processing_code/Gape_widthBK.R")
source("Processing_code/Edible_sizeBK.R")
library(betareg)
nonnatives <- readRDS('Processing_code/nonnatives.rds') %>%
  select(1,2,3,5) %>%
  filter(!is.na(TL)) %>%
  rowwise() %>%
  mutate(Mass = lw_nonnatives(Species, TL)) %>%
  ungroup() %>%
  group_by(Species, Reach) %>%
  mutate(prop = prop.table(Mass)) %>%
  mutate(edible = bd_tl_2024(1 * calculate_gape(
    Species, TL))) %>% 
  arrange(Species, Reach, edible) %>%
  mutate(cumprop = cumsum(prop)) %>%
  ungroup()

model <- glm(cumprop ~ edible * Species * Reach, 
             data = nonnatives, 
             family = binomial(link = "logit"))
summary(model)

nonnatives$pred <- predict(model, newdata = nonnatives, type = 'response')
ggplot(nonnatives, aes(x = edible, color = factor(Reach))) + 
  #geom_point(aes(y = cumprop), size = 0.5, alpha = 0.5) + 
  geom_line(aes(y = pred), linewidth = 1) +
  facet_wrap(~Species)
