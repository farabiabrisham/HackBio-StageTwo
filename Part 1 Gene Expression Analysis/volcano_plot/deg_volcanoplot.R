#           HackBio 
# Data Vizualisation Internship 2026
# Author: Farabi Abrisham Ahmed

# Data Visualisation in R

# Gene Expression Analysis
# Volcano Plot

plot(x    = hbr_uhr_deg_chr22$log2FoldChange,
     y    = hbr_uhr_deg_chr22$`-log10PAdj`,
     xlab = 'log2 Fold Change',
     ylab = '-log10(Adjusted P-value)',
     col  = as.factor(hbr_uhr_deg_chr22$significance),
     main = 'Volcano Plot: HBR vs UHR',
     cex  = 0.5,
     pch  = 19
     )

abline(v = c(-1, 1), lty = 2, col = "red")


legend("topright", 
       legend = c("Upregulated", "Downregulated", "Not significant"),
       col = c("green", "pink", "black"),
       pch = 19,
       cex = 0.7)
