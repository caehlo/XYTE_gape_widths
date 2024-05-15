# Output script that should be turned into presentation Markdown maybe

source("Processing_code/XYTE_body_depth.R")
source("Processing_code/Length_weightBK.R")
source("Processing_code/Gape_widthBK.R")

# Establish a minimum and maximum proportion of gape width that matches 
# body depth of XYTE.  1.0 means that a predator can ingest a XYTE that 
# has a body depth equal to or less than the gape width of that predator
min_prop = .8
max_prop = 1.0

Species <- c("MOSA", "MISA", "MIDO", "PYOL")
# Test edible size function
edible_size <- bd_tl_2024(min_prop * calculate_gape("MISA", 800))
edible_size

edible_size_df <- expand.grid(Species = Species, TL = seq(100,1400,10)) %>%
  mutate(edible_min_prop = bd_tl_2024(min_prop * calculate_gape(
    edible_size_df$Species, edible_size_df$TL)))

Edible_size_plot <- ggplot(edible_size_df, aes(x = TL, color = Species)) + 
                    geom_line(aes(y = edible_min_prop))
Edible_size_plot
