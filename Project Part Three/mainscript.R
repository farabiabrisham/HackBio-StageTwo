#           HackBio 
# Data Vizualisation Internship 2026
# Author: Farabi Abrisham Ahmed

# Data Visualisation in R
# Part Three Stage Two

install.packages('readxl')
install.packages('ggplot2')
install.packages('pheatmap')
install.packages('igraph')

#use this function to create transparent colors
transparent_color <- function(color, percent = 50, name = NULL) {
  #      color = color name
  #    percent = % transparency
  #       name = an optional name for the color
  
  ## Get RGB values for named color
  rgb.val <- col2rgb(color)
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100 - percent) * 255 / 100,
               names = name)
  
  ## Save the color
  invisible(t.col)
}

hb_pal <- c("#4e79a7", 
            "#8cd17d", 
            "#e15759", 
            "#fabfd2", 
            "#a0cbe8", 
            "#59a14f", 
            "#b07aa1", 
            "#ff9d9a", 
            "#f28e2b", 
            "#f1ce63",
            "#79706e",
            "#d4a6c8",
            "#e9e9e9",
            "#ffbe7d",
            "#bab0ac",
            "#9d7660",
            "#d37295",
            "#86bcb6",
            "#362a39",
            "#cd9942")

#test the color pallete
plot(1:length(hb_pal), 1:length(hb_pal), col = hb_pal, pch = 19)


## Cell-type ratio distributions (1)

df_a <- a_Table_1

df_a$new_ratio <- as.numeric(df_a$new_ratio)
df_a$cell_type <- as.factor(df_a$cell_type)


boxplot(new_ratio ~ cell_type,
        data = df_a,
        ylim = c(0, 0.5),      # match the panel scale (adjust if needed)
        las  = 1,              # rotate x labels (vertical)
        outline = TRUE,        # show outliers
        pch = 1,               # outlier point style
        col = hb_pal[1:length(levels(df_a$cell_type))],
        ylab = "Ratio",
        xlab = "",
        main = "cell_type_ratio_distributions")


## Half-life vs alpha-life scatter (2)

library(readr)
b_Table_1 <- read_csv("Desktop/Internships and Workshops/HackBio DataViz/3. Stage Two/hb_stage2/csv files/b-Table 1.csv", 
                      col_types = cols(alpha = col_number(), 
                                       half_life = col_number(), ...4 = col_skip(), 
                                       ...5 = col_skip()))


colnames(b_Table_1)                                     # Check column names
head(b_Table_1)

b_Table_1$log2_half_life <- log2(b_Table_1$half_life)   # Make log2 variables
b_Table_1$log2_alpha     <- log2(b_Table_1$alpha)

x_cut <- 2.5      # assign cutoff values x_cut
y_cut <- -3.5     # assign cutoff values y_cut

b_Table_1$group <- "ns" 
b_Table_1$group[b_Table_1$log2_half_life < x_cut & b_Table_1$log2_alpha > y_cut] <- "#8cd17d"
b_Table_1$group[b_Table_1$log2_half_life >= x_cut] <- "#4e79a7"
b_Table_1$group[b_Table_1$log2_half_life >= x_cut & b_Table_1$log2_alpha > y_cut] <- "#e15759"

plot(b_Table_1$log2_half_life, b_Table_1$log2_alpha,
     pch = 16, cex = 0.7,
     col = pt_col,
     xlab = "log2(Half Life)",
     ylab = "log2(Alpha Life)",
     main = 'Half-life vs alpha-life scatter')

abline(v = x_cut, lty = 2, lwd = 2)
abline(h = y_cut, lty = 2, lwd = 2)

legend("topright",
       legend = c("Ccr2", "Camp"),
       col    = c("#8cd17d", "#4e79a7"),
       pch    = 16,
       pt.cex = 1,
       bty    = "n")


# Why log2?
# Compresses wide ranges (half-life/alpha often span orders of magnitude), so the scatter becomes readable.
# Turns multiplicative differences into additive differences: doubling/halving becomes ±1 on log2 scale.
# Makes it easier to see regimes and apply simple linear thresholds (vertical/horizontal cut lines).

# What do the four quadrants mean?
# Top-left (shorter half-life, higher alpha-life): fast turnover but relatively higher alpha component → “rapid / more dynamic” regime.
# Top-right (longer half-life, higher alpha-life): stable transcripts with higher alpha → “stable-high” regime.
# Bottom-left (shorter half-life, lower alpha-life): unstable + low alpha → “unstable-low” regime.
# Bottom-right (longer half-life, lower alpha-life): stable overall but low alpha → “stable-low / slow-changing” regime.


## Heatmap across cell types and time (3)

library(pheatmap)

df_c <- c_Table_1

# Use gene names as rownames & keep only numeric expression columns
rownames(df_c) <- df_c$genes
mat_c <- as.matrix(df_c[, -1])

# Make sure the matrix is numeric
mat_c <- apply(mat_c, 2, as.numeric)
rownames(mat_c) <- rownames(df_c)

col_names <- colnames(mat_c)

cell_type <- sub("n[0-9]+h$", "", col_names)      # remove the trailing n00h, n02h, ...
time      <- sub("^.*n([0-9]+h)$", "\\1", col_names)  # keep only 00h/02h/...

annotation_col <- data.frame(
  CellType = cell_type,
  Time     = time
)
rownames(annotation_col) <- col_names

heat_cols <- colorRampPalette(c("#e15759", "white", "#4e79a7"))(100)
max_abs <- max(abs(mat_c), na.rm = TRUE)
bk <- seq(-max_abs, max_abs, length.out = 101)

pheatmap(
  mat = mat_c,
  color = heat_cols,
  breaks = bk,
  border_color = "grey",
  legend = TRUE,
  annotation_col = annotation_col,
  cluster_rows = T,
  cluster_cols = F,
  show_colnames = F,
  show_rownames = F,
  main = 'Heatmap across cell types and time'
)

# Why cluster genes but not time?
# Clustering genes helps in discovering gene modules: groups of genes with similar temporal patterns or cell-type responses which is biologically meaningful because co-varying genes often reflect shared regulation or pathways.
# Not clustering time keeps the timeline in true order (00h → 72h). If we cluster timepoints, the heatmap may reorder them (e.g., 72h next to 02h), which can confuse the interpretation of progression over time.


## Pathway enrichment heatmap (4)

library(pheatmap)
library(readr)
d_1_Table_1 <- read_csv("Desktop/Internships and Workshops/HackBio DataViz/3. Stage Two/hb_stage2/csv files/d_1-Table 1.csv", 
                        col_types = cols(n00h = col_number(), 
                                         n02h = col_number(), n06h = col_number(), 
                                         n12h = col_number(), n24h = col_number(), 
                                         n48h = col_number()))

d1 <- d_1_Table_1
d1 <- as.data.frame(d1)
rownames(d1) <- d1$pathway

time_cols <- c("n00h","n02h","n06h","n12h","n24h","n48h","n72h")
time_cols <- time_cols[time_cols %in% colnames(d1)]  # keeps only existing ones

mat <- as.matrix(d1[, time_cols])

max_abs <- max(abs(mat), na.rm = TRUE)
breaks  <- seq(-max_abs, max_abs, length.out = 101)
pal     <- colorRampPalette(c("red", "white", "royalblue"))(100)

pheatmap(mat,
         cluster_rows = FALSE,
         cluster_cols = FALSE,
         show_rownames = TRUE,
         show_colnames = TRUE,
         border_color = "grey60",
         color = pal,
         breaks = breaks,
         legend = TRUE,
         fontsize_row = 9,
         main = 'Pathway enrichment heatmap')

# Why no clustering?
# Pathways might be intentionally ordered (by relevance, curated grouping, or paper narrative).
# Clustering could reorder them and weaken interpretation.

# Why diverging palette? 
# Enrichment scores often have direction (up vs down).
# Centering at 0 makes “positive vs negative” immediately visible

## Bubble plot of kinetic regimes (5)

library(readr)
e_Table_1 <- read_csv("Desktop/Internships and Workshops/HackBio DataViz/3. Stage Two/hb_stage2/csv files/e-Table 1.csv")
e_Table_1
table(e_Table_1$stage)

cex_pts <- sqrt(e_Table_1$count) / sqrt(max(e_Table_1$count)) * 3

col_stage <- c("6h"  = hb_pal[8],    # pink-ish
               "72h" = hb_pal[18])   # teal-ish

scale_factor <- 3
cex_pts <- sqrt(e_Table_1$count) / sqrt(max(e_Table_1$count)) * scale_factor

par(mar = c(5, 5, 2, 6), xpd = NA)

plot(e_Table_1$half_life, e_Table_1$alpha,
     type = "n",
     xlab = "Half Life",
     ylab = "Alpha Life")

points(e_Table_1$half_life, e_Table_1$alpha,
       pch = 16,
       col = col_stage[e_Table_1$stage],
       cex = cex_pts)

legend("topright",
       legend = names(col_stage),
       col    = col_stage,
       pch    = 16,
       pt.cex = 1.2,
       bty    = "o",
       bg     = "white",
       title  = "stage")

size_vals <- c(10, 20, 30)
size_vals <- size_vals[size_vals <= max(e_Table_1$count)]
size_cex  <- sqrt(size_vals) / sqrt(max(e_Table_1$count)) * scale_factor

legend("bottomright",
       legend = size_vals,
       pch    = 16,
       col    = "black",
       pt.cex = size_cex,
       bty    = "o",
       bg     = "white",
       title  = "count")


## Stacked proportions (6) 

library(readr)

f_Table_1
colnames(f_Table_1)     # Inspect
head(f_Table_1)         # Inspect

sub_df <- f_Table_1[f_Table_1$stage %in% c("s00h", "s72h"), c("cell_type", "stage", "proportion")]


sub_df$proportion <- as.numeric(sub_df$proportion)
mat <- xtabs(proportion ~ cell_type + stage, data = sub_df)
mat <- mat[, c("s00h", "s72h")]


cols <- c("Plasma" = "#3E5F8A", "B" = "pink")

barplot(mat,
        beside = FALSE,
        ylim = c(0, 0.3),
        border = "black",
        col = cols[rownames(mat)],
        las = 1)

legend("topright",
       legend = rownames(mat),
       fill = cols[rownames(mat)],
       bty = "o")

# Why stacked instead of side-by-side?
#A stacked bar makes it easy to compare:
#the total proportion (overall height) at s00h vs s72h, and
#the share contributed by each cell type within that total (B vs Plasma).
#Side-by-side is better when we care mainly about comparing absolute values across groups, not composition.


## Directed cell–cell interaction network (7)

library(igraph)

# 1) Make adjacency matrix properly
mat_df <- as.data.frame(g_Table_1)

# first column = row names (cell types)
rn <- mat_df[[1]]

# numeric adjacency values
mat <- as.matrix(mat_df[, -1])
mode(mat) <- "numeric"                 # force numeric without dropping dimnames
rownames(mat) <- rn
colnames(mat) <- colnames(mat_df)[-1]

# remove self-loops
diag(mat) <- 0

# Build graph
g <- graph_from_adjacency_matrix(mat,
                                 mode = "directed",
                                 weighted = TRUE,
                                 diag = FALSE)


g <- delete_edges(g, E(g)[weight == 0])

E(g)$color <- "grey70"

w <- E(g)$weight
E(g)$width <- 0.8 + 2.0 * (w / max(w))     # gentle scaling
E(g)$arrow.size <- 0.35                    # mostly constant looks like the example

V(g)$color <- "pink"
V(g)$frame.color <- "grey40"
V(g)$size <- 18

V(g)$label.color <- "blue4"
V(g)$label.cex <- 1.4                      # NOT 10
V(g)$label.font <- 2                       # bold-ish

set.seed(1)
L <- layout_with_fr(g)

plot(g,
     layout = L,
     edge.curved = 0,
     main = "Directed cell-cell interaction",
     vertex.label.family = "serif")

#Why directed?
#Because communication has a sender → receiver direction (who signals to whom).

# What does edge weight encode?
#Strength/amount of interaction (often inferred from ligand–receptor scores, interaction probability, or communication intensity depending on the method).
