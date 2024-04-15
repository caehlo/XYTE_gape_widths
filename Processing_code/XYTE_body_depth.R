# Razorback sucker body depth vs TL relationship
# B. Kesner 4/12/2024

# Equation reference
# Razorback Hump - Portz, D.E. and H.M. Tyus. 2004. 
# Fish humps in two Colorado River fishes: a morphological response to cyprinid predation? 
# Environmental Biology of Fishes 71:233-245.

tl_bd_2004 <- function(TL){
  BodyDepth <- 0.196*TL + 2.517
  return(BodyDepth)
}

# 1984 data from Bozek et al. 1984? but raw data not reported.
# 2005 data measured by B. Kesner from ASU museum specimens 
XYTEBodyDepth <- 
  read.csv("Processing_code/data/RazorbackSuckerBodyDepth.csv")

TLBD_model <- BD_mm ~ TL_mm
TLBD_results <- glm(TLBD_model, data = XYTEBodyDepth)

XYTEBodyDepth$Residuals <- resid(TLBD_results, type = "pearson")

Intercept2024 <- coef(TLBD_results)[1] %>% as.double()
Slope2024 <- coef(TLBD_results)[2] %>% as.double()

tl_bd_2024 <- function(TL){
  BodyDepth <- Slope2024*TL + Intercept2024
  return(BodyDepth)
}

BD_plot <- ggplot(XYTEBodyDepth, aes(x = TL_mm, y = BD_mm, color = Source)) +
  geom_point() +
  stat_function(fun = tl_bd_2004, color = "blue", size = 1) + 
  stat_function(fun = tl_bd_2024, color = "red", size = 1) 

BD_residuals_plot <- ggplot(XYTEBodyDepth, aes(x = TL_mm, y = Residuals, color = Source)) +
  geom_point() + 
  geom_smooth(method="lm", se=FALSE, color="black")
BD_plot

