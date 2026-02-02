#           HackBio 
# Data Vizualisation Internship 2026
# Author: Farabi Abrisham Ahmed

# Data Visualisation in R

# Which plot type would be best for story-telling?
#Always ask this question.

## Categorical Plots

isolation_origin_freq <- table(bacteria_data$Isolation.origin)
print(isolation_origin_freq)

# Barchart

barplot(height = isolation_origin_freq,
        width = c(1, 1, 1, 1),
        names.arg = c('Animal', 'Blood', 'Feces', 'Urine'),
        legend = T,
        las = 2,
        ylim = c(0,20),
        col = c(2:5)
        )

# Piechart

pie(isolation_origin_freq,
    col =   c(2:5),
    label = c('Animal', 'Blood', 'Feces', 'Urine'),
    radius= 1.05,
    clockwise = T,
    lty = 1
)

# Stacked Barchart

multiple_cat <- table(bacteria_data$Phenotype, bacteria_data$Isolation.origin)
print(multiple_cat)
barplot(multiple_cat,
        width = c(1, 1, 1, 1),
        names.arg = c('Animal', 'Blood', 'Feces', 'Urine'),
        legend = T,
        las = 2,
        ylim = c(0, 20),
        col = c(3:2)
        )
# Boxplot

boxplot(carb_fit ~ Isolation.origin, data = bacteria_data)

# Point plots

plot(C1 ~ C2, data = bacteria_data)       # C2 against C1

plot(bacteria_data$C1, bacteria_data$C2,
     col = as.factor(bacteria_data$labels)
     )                                    # Colors points by group

# Introduction to Boxplots

boxplot(bacteria_data$carb_fit,
        notch = T,
        ylim  = c(0, 1.5),
        main  = 'My First Boxplot',
        ylab  = 'Fitnees',
        xlab  = 'Carbenicillin',
        col  = 3
        )

# Density plot

plot( density(x = bacteria_data$carb_fit)
     ) # For probability estimation

# Scatter plot

plot(C1 ~ C2,
     data = bacteria_data)

plot(x    = bacteria_data$C1,
     y    = bacteria_data$C2,
     xlab = 'PC1',
     ylab = 'PC2',
     xlim = c(0, 10),
     ylim = c(0, 10),
     main = 'PC Plot',
     col  = as.factor(bacteria_data$labels),
     pch  = 19,
     cex  = 0.725
     )

# Heatmaps

library(pheatmap)

colnames(bacteria_data[, 8:13])

pheatmap(mat = bacteria_data[, 8:13],
         border_color = 'black',
         legend = T,
         labels_row = bacteria_data$sample_id,
         fontsize_row = 4,
         cluster_rows = T,
         cluster_cols = T
         )





