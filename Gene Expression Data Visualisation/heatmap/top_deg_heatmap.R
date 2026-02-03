#           HackBio 
# Data Vizualisation Internship 2026
# Author: Farabi Abrisham Ahmed

# Data Visualisation in R

# Gene Expression Data
# Heatmap

library(pheatmap)

colnames(hbr_uhr_top_deg_normalized_counts[,2:7])

pheatmap(mat = hbr_uhr_top_deg_normalized_counts[, 2:7],
         border_color = 'black',
         legend = T,
         labels_row = hbr_uhr_top_deg_normalized_counts$...1,
         fontsize_row = 4.5,
         color = blues9,
         cluster_rows = T,
         cluster_cols = T
)





