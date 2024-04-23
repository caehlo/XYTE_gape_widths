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
  BodyDepth <- 0.196*TL + 2.517
  return(BodyDepth)
}

# 1984 data from Bozek et al. 1984? but raw data not reported.
# 2005 data measured by B. Kesner from ASU museum specimens 
XYTEBodyDepth <- 
  read.csv("Processing_code/data/RazorbackSuckerBodyDepth.csv")

TLBD_model <- BD_mm ~ TL_mm
TLBD_model_transformed <- log(BD_mm) ~ TL_mm
TLBD_results <- glm(TLBD_model, data = XYTEBodyDepth)
TLBD_t_results <- glm(TLBD_model_transformed, data = XYTEBodyDepth)

XYTEBodyDepth$Residuals <- residuals(TLBD_results)
XYTEBodyDepth$TResiduals <- residuals(TLBD_t_results)

# Shapiro-Wilk test for normality
shapiro.test(XYTEBodyDepth$Residuals)
shapiro.test(XYTEBodyDepth$TResiduals)

qqnorm(XYTEBodyDepth$Residuals)
qqline(XYTEBodyDepth$Residuals)

qqnorm(XYTEBodyDepth$TResiduals)
qqline(XYTEBodyDepth$TResiduals)

Intercept2024 <- coef(TLBD_results)[1] %>% as.double()
Slope2024 <- coef(TLBD_results)[2] %>% as.double()

Intercept2024T <- coef(TLBD_t_results)[1] %>% as.double()
Slope2024T <- coef(TLBD_t_results)[2] %>% as.double()

tl_bd_2024T <- function(TL){
  BodyDepth <- Slope2024T*TL + Intercept2024T
  return((BodyDepth)^exp())
}

tl_bd_2024 <- function(TL){
  BodyDepth <- Slope2024*TL + Intercept2024
  return(BodyDepth)
}

BD_plot <- ggplot(XYTEBodyDepth, aes(x = TL_mm, y = BD_mm, color = Source)) +
  geom_point() +
  stat_function(fun = tl_bd_2004, color = "black") + 
  stat_function(fun = tl_bd_2024T, color = "red") +
  stat_function(fun = tl_bd_2024, color = "white") 

BD_residuals_plot <- ggplot(XYTEBodyDepth, aes(x = TL_mm, y = TResiduals, color = Source)) +
  geom_point() + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Total length (mm)", y = "Residuals", title = "Residual Plot")

BD_plot

BD_residuals_plot

ggplot(your_data, aes(x = x, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Predictor (x)", y = "Residuals", title = "Residual Plot")
