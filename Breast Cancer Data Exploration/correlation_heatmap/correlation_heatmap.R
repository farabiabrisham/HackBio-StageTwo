#           HackBio 
# Data Vizualisation Internship 2026
# Author: Farabi Abrisham Ahmed

# Data Visualisation in R

# Breast Cancer Data Exploration
# Correlation Heatmap

library(pheatmap)

# Import data locally
# Testing some of the correlations:

cor(x = breast_cancer_wisconsin$radius_mean,
    y = breast_cancer_wisconsin$radius_mean)
cor(x = breast_cancer_wisconsin$radius_mean,
    y = breast_cancer_wisconsin$texture_mean)
cor(x = breast_cancer_wisconsin$radius_mean,
    y = breast_cancer_wisconsin$perimeter_mean)
cor(x = breast_cancer_wisconsin$radius_mean,
    y = breast_cancer_wisconsin$area_mean)

# Assigning the features

features <- breast_cancer_wisconsin[, c(
  "radius_mean",
  "texture_mean",
  "perimeter_mean",
  "area_mean",
  "smoothness_mean",
  "compactness_mean"
)]

# Making sure the data from csv is numeric

features <- data.frame(lapply(features, as.numeric))

# Computing the 6x6 correlation matrix 

cor_mat <- cor(features, use = "complete.obs")

# Coding the colour palette:

blue_palette <- colorRampPalette(c("white",
                                   "lightblue",
                                   "steelblue",
                                   "royalblue")
                                 ) (100)

# Coding the pheatmap() function

pheatmap(mat = cor_mat,
         border_color = 'black',
         legend = T,
         cluster_rows = F,
         cluster_cols = F,
         display_numbers = TRUE,
         color = blue_palette,
         number_format = "%.1f",
         number_color = "black"
)


