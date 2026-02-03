#           HackBio 
# Data Vizualisation Internship 2026
# Author: Farabi Abrisham Ahmed

# Data Visualisation in R

# Breast Cancer data Exploration
# Density Plot (area distribution)

# Import data locally

# Split area_mean by diagnosis

area_M <- breast_cancer_wisconsin$area_mean[breast_cancer_wisconsin$diagnosis == "M"]
area_B <- breast_cancer_wisconsin$area_mean[breast_cancer_wisconsin$diagnosis == "B"]

d_M <- density(area_M)
d_B <- density(area_B)

# Code the density plot:

plot(d_M,
     main = "Density Plot: area_mean (M vs B)",
     xlab = "area_mean",
     ylab = "Density",
     ylim = c(0, 0.003125),
     yaxs = 'i',
     lwd  = 2)

lines(d_B, lwd = 2)

# Fill under the curves (transparent colors)

polygon(d_M$x, d_M$y,
        col = rgb(0, 0, 1, 0.25),  # blue w/ transparency
        border = NA)

polygon(d_B$x, d_B$y,
        col = rgb(1, 0.5, 0, 0.25), # orange w/ transparency
        border = NA)

# Draw the outline lines on top

lines(d_M, lwd = 2, col = "blue")
lines(d_B, lwd = 2, col = "orange")

# Legend

legend("topright",
       legend = c("M", "B"),
       col = c("blue", "orange"),
       fill = c(rgb(0, 0, 1, 0.25), rgb(1, 0.5, 0, 0.25)),
       bty = "n")










