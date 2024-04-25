# Razorback sucker body depth vs TL relationship
# B. Kesner 4/12/2024

# Equation reference
# Razorback Hump - Portz, D.E. and H.M. Tyus. 2004. 
# Fish humps in two Colorado River fishes: a morphological response to cyprinid predation? 
# Environmental Biology of Fishes 71:233-245.
library(stats)
library(dplyr)
library(ggplot2)

tl_bd_2004 <- function(TL){
  BodyDepth <- round(0.196*TL + 2.517, 1)
  return(BodyDepth)
}

# 1984 data from Bozek et al. 1984? but raw data not reported.
# 2005 data measured by B. Kesner from ASU museum specimens 
XYTEBodyDepth <- 
  read.csv("raw_data/RazorbackSuckerBodyDepth.csv")

TLBD_model <- lm(BD_mm ~ TL_mm, data = XYTEBodyDepth)

XYTEBodyDepth$Residuals <- residuals(TLBD_model)
XYTEBodyDepth$PredDepth <- round(predict(TLBD_model, type = 'response'), 1)

coefBD <- bind_rows(coef(TLBD_model)) %>%
  rename(SlopeBD = TL_mm, InterceptBD = `(Intercept)`)

tl_bd_2024 <- function(TL){
  BodyDepth <- round(coefBD$SlopeBD*TL + coefBD$InterceptBD, 1)
  return(BodyDepth)
}

BD_plot <- ggplot(XYTEBodyDepth, aes(x = TL_mm, y = BD_mm, color = Source)) +
  geom_point() +
  stat_function(fun = tl_bd_2004, color = "black") + 
  stat_function(fun = tl_bd_2024, color = "white") 

BD_residuals_plot <- ggplot(XYTEBodyDepth, aes(x = TL_mm, y = Residuals, color = Source)) +
  geom_point() + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Total length (mm)", y = "Residuals", title = "Residual Plot")

BD_plot

BD_residuals_plot


