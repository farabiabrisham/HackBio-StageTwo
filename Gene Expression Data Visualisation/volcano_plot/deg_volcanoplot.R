#           HackBio 
# Data Vizualisation Internship 2026
# Author: Farabi Abrisham Ahmed

# Data Visualisation in R

# Gene Expression Data
# Volcano Plot

# import hbr_uhr_deg_chr22 .csv file locally

plot(x    = hbr_uhr_deg_chr22$log2FoldChange,
     y    = hbr_uhr_deg_chr22$`-log10PAdj`,
     xlab = 'log2 Fold Change',
     ylab = '-log10(Adjusted P-value)',
     col  = as.factor(hbr_uhr_deg_chr22$significance),
     main = 'Volcano Plot: HBR vs UHR',
     cex  = 0.7,
     pch  = 19
     )

abline(v = c(-2, 2), lty = 2, col = "black")
abline(h = c(1, 0), lty = 2, col = "black")

legend("topright", 
       legend = c("Down regulated", "Not Significant", "Up regulated"),
       col = c(1, 2, 3),
       pch = 19,
       cex = 0.7)










