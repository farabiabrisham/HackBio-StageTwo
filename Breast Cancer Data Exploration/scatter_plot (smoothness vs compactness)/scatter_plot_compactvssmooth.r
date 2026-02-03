#           HackBio 
# Data Vizualisation Internship 2026
# Author: Farabi Abrisham Ahmed

# Data Visualisation in R

# Breast Cancer Data Exploration
# Scatteplot (Smoothness vs Compactness)

# Import data locally

# Coding the Scatter Plot:

plot(x    = breast_cancer_wisconsin$smoothness_mean,
     y    = breast_cancer_wisconsin$compactness_mean,
     xlab = 'Smoothness Mean',
     ylab = 'Compactness Mean',
     xlim = c(0.06, 0.165),
     ylim = c(0.0125, 0.35),
     main = 'Compactness vs Smoothness',
     pch  = 1,
     col = as.factor(breast_cancer_wisconsin$diagnosis),
     cex  = 0.725
)

legend("topleft", 
       legend = c("Malignant", "Benign"),
       col = c(2, 1),
       pch = 1,
       cex = 0.700)














