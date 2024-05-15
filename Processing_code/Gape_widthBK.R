library(dplyr)
library(ggplot2)
library(purrr)

# each equation corresponds to a gape-width line equation for 4 different predators: 
# MOSA(striped bass), PYOL(Flathead Catfish), MISA(Largemouth Bass), MIDO(Smallmouth Bass)
equations <- list(MOSA = function(TL) {-1.502 + (0.125 * TL) - (0.000005 * TL^2)},
                  PYOL = function(TL) {(123 * 2.718^(0.0008*TL)) - 119.3},
                  MISA = function(TL) {(0.14*TL) - 5.59}, 
                  MIDO = function(TL) {(0.13*TL) - 1.05})


# Create an empty ggplot object
gw_plot <- ggplot(data.frame(TL = seq(100,1500,5)), aes(x = TL)) +
  stat_function(fun = equations$MOSA, aes(colour = "Striped bass (MOSA)")) +
  stat_function(fun = equations$PYOL, aes(colour = "Flathead catfish (PYOL)")) +
  stat_function(fun = equations$MISA, aes(colour = "Largemouth bass (MISA)")) +
  stat_function(fun = equations$MIDO, aes(colour = "Smallmouth bass (MIDO)")) +
  scale_colour_manual("Species", values = c("red", "green", "orange", "black")) +
  labs(x = "Total length (mm)", y = "Gape (mm)", title = "Predator gape width")

gw_plot
# Add each function to the plot


# inverse function script to inverse Gape width formulas
inverse <- function(f, lower, upper){
  function(y){
    uniroot(function(x){f(x) - y}, lower = lower, upper = upper, tol=1e-3)[1]
  }
}

# Instead of list of equations and a dataframe, generate a function with inputs
# Species, TL, GWIndex. This will output the maximum XYTE TL that can be ingested based on data.

calculate_gape <- function(species, TL) {
  gape_value <- case_when(
    species == "MISA" ~ ((0.14*TL) - 5.59),
    species == "MOSA" ~ -1.502 + (0.125 * TL) - (0.000005 * TL^2),
    species == "MIDO" ~ (0.13*TL) - 1.05,
    species == "PYOL" ~ (123 * 2.718^(0.0008*TL)) - 119.3,
    TRUE ~ NA_real_)
  return(gape_value)
}


min_ratio = .8
max_ratio = 1.0

edible_size <- bd_tl_2024(min_ratio * calculate_gape("MISA", 800))
edible_size



