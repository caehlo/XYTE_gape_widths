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


GW_index_min = 1.0
GW_index_max = 1.2



