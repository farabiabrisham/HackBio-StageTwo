#           HackBio 
# Data Vizualisation Internship 2026
# Author: Farabi Abrisham Ahmed

# Data Visualisation in R

# Breast Cancer data Exploration
# Data set: Wisconsin

library(dplyr)

breast_cancer_wisconsin # import data locally

plot(x    = breast_cancer_wisconsin$radius_mean,
     y    = breast_cancer_wisconsin$texture_mean,
     xlab = 'radius_mean',
     ylab = 'texture_mean',
     xlim = c(7, 28),
     ylim = c(10, 40),
     main = 'Breast Cancer Data Scatter Plot',
     col  = as.factor(breast_cancer_wisconsin$diagnosis),
     pch  = 1,
     cex  = 0.7
)

legend("topright", 
       legend = c("Malignant", "Benign"),
       col = c(2, 1),
       pch = 1,
       cex = 0.7)







