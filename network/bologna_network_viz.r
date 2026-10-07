# NETWORK VISUALIZATION: incriminations in the Bologna inquisition register ----
# Idea: a network plot is an *argument*, not a picture. Every visual channel
# (position, colour, shape, size, edge style) should encode one variable,
# and the layout should reveal structure instead of a "hairball".
#
# Data:   Riccardo, Zbiral, Brys, Hampejs (2024), Zenodo, doi:10.5281/zenodo.13935366
#         "Incriminations in the inquisition register of Bologna (1291-1310)"
#         Each edge  from -> to  = person `from` incriminated person `to`
#         (a deponent accuses someone in front of the inquisitor) -> DIRECTED.
# Code:   adapted from the authors' Fig 1 (BolInrc_Main_Analysis.R), extended
#         here step by step for teaching.
# Package: igraph | Docs: https://r.igraph.org/
# Advanced code for the models: https://github.com/DISSINET/bologna.incr.social

# INSTALL ----
if (!requireNamespace("igraph", quietly = TRUE)) install.packages("igraph")
library(igraph)

# DATA ----
# df_nodes.tsv and df_edges.tsv (from the Zenodo record) sit next to this script.
# Set the working directory to that folder first (Session > Set Working Directory
# in RStudio, or setwd("path/to/folder")).
stopifnot(file.exists("df_nodes.tsv"), file.exists("df_edges.tsv"))
nodes <- read.delim("df_nodes.tsv", sep = "\t", header = TRUE, fileEncoding = "UTF-8")  # one row per person + attributes
edges <- read.delim("df_edges.tsv", sep = "\t", header = TRUE, fileEncoding = "UTF-8")  # from, to

head(nodes[, c("name", "label", "gender", "deponent", "churchperson")])
head(edges)
# gender: 1 = man, 0 = woman (assumption: 493 vs 170 persons, as expected
# in a medieval source; check the codebook of the record)
table(nodes$gender)

# BUILD THE GRAPH ----
# vertices = data frame whose FIRST column is the id used in `edges`
g <- graph_from_data_frame(d = edges, directed = TRUE, vertices = nodes)
g
summary(g)

# DESCRIBE BEFORE YOU DRAW ----
# these numbers tell us *what* we should expect to see
vcount(g); ecount(g)
edge_density(g)                                 # share of possible ties present
reciprocity(g)                                  # mutual accusations?
table(deponent = V(g)$deponent)                 # who speaks in front of the inquisitor
comp <- components(g, mode = "weak")
comp$no                                         # number of components
sort(comp$csize, decreasing = TRUE)[1:5]        # one giant + many tiny ones
sum(degree(g) == 0)                             # isolates

# in-degree = how often a person was incriminated, out-degree = how many they incriminated
V(g)$indeg  <- degree(g, mode = "in")
V(g)$outdeg <- degree(g, mode = "out")
summary(V(g)$indeg); summary(V(g)$outdeg)
head(sort(V(g)$indeg, decreasing = TRUE), 10)
hist(V(g)$indeg, breaks = 30, col = "grey70", border = "white",
     main = "In-degree distribution", xlab = "times incriminated")
# very skewed -> a few people are hubs; size by raw degree would be unreadable

# STEP 0: THE DEFAULT PLOT (what NOT to do) ----
# default layout, big labels, big nodes -> an unreadable hairball
par(mar = c(0, 0, 2, 0))
plot(g, main = "Step 0: default igraph plot")

# STEP 1: REMOVE NOISE ----
# no labels, small nodes, thin edges, small arrows
plot(g, vertex.label = NA, vertex.size = 3, edge.arrow.size = 0.2,
     main = "Step 1: no labels, small nodes")

# STEP 2: LAYOUT ----
# the layout is just a set of x/y coordinates: the *same* graph looks
# different under different algorithms. Compute it once, re-use it everywhere.
# Force-directed layouts are random-start -> set.seed for reproducibility.
set.seed(42)
lay_fr <- layout_with_fr(g)        # Fruchterman-Reingold: edges = springs, nodes repel
lay_kk <- layout_with_kk(g)        # Kamada-Kawai: distance on plot ~ geodesic distance
lay_dr <- layout_with_drl(g)       # DrL: built for large graphs, separates clusters
lay_gh <- layout_with_graphopt(g)

par(mfrow = c(2, 2), mar = c(0, 0, 2, 0))
for (l in c("fr", "kk", "dr", "gh")) {
  plot(g, layout = get(paste0("lay_", l)), vertex.label = NA, vertex.size = 2.5,
       edge.arrow.size = 0.1, edge.width = 0.4, main = paste("Layout:", l))
}
par(mfrow = c(1, 1))
# Q: which layout separates the small components best? which keeps the core compact?

# STEP 3: ENCODE ATTRIBUTES (the authors' Fig 1) ----
# colour = gender, shape = deponent, size = log(in-degree + 3)
# log() compresses the skewed in-degree; +3 keeps isolates visible (log(0) = -Inf)
node_colors <- ifelse(V(g)$gender == 1, "blue", "orange")
node_shapes <- ifelse(V(g)$deponent == 1, "square", "circle")
node_sizes  <- log(V(g)$indeg + 3)

plot(g, layout = lay_fr, vertex.label = NA,
     vertex.color = node_colors, vertex.shape = node_shapes,
     vertex.size = node_sizes, edge.arrow.size = 0.3,
     main = "Step 3: gender, deponent status, in-degree")
# Q: what is missing? -> a legend! A plot without a legend is not interpretable.

# STEP 4: LEGEND, TRANSPARENCY, COLOUR-BLIND SAFE PALETTE ----
# Okabe-Ito colours stay distinguishable for colour-blind readers;
# semi-transparent edges reveal density where lines overlap.
col_man   <- "#0072B2"
col_woman <- "#E69F00"
node_colors <- ifelse(V(g)$gender == 1, col_man, col_woman)
edge_col    <- adjustcolor("grey30", alpha.f = 0.35)

plot_bologna <- function(g, layout, main = "") {
  plot(g, layout = layout, vertex.label = NA,
       vertex.color = ifelse(V(g)$gender == 1, col_man, col_woman),
       vertex.frame.color = "white", vertex.frame.width = 0.4,
       vertex.shape = ifelse(V(g)$deponent == 1, "square", "circle"),
       vertex.size = log(V(g)$indeg + 3) * 1.3,
       edge.color = edge_col, edge.width = 0.5, edge.arrow.size = 0.25,
       main = main)
  legend("bottomleft", bty = "n", cex = 0.8,
         legend = c("man", "woman", "deponent (square)", "not deponent (circle)"),
         pch = c(16, 16, 15, 16),
         col = c(col_man, col_woman, "grey40", "grey40"),
         title = "Node size = log(in-degree + 3)")
}
par(mar = c(0, 0, 2, 0))
plot_bologna(g, lay_fr, "Step 4: Bologna incriminations (1291-1310)")

# STEP 5: FOCUS ON THE STRUCTURE ----
# 5a) isolates carry no relational information: drop them (and say so in the caption!)
g_noiso <- delete_vertices(g, which(degree(g) == 0))
# 5b) or keep only the giant weak component
big <- which.max(comp$csize)
g_giant <- induced_subgraph(g, which(comp$membership == big))
vcount(g_noiso); vcount(g_giant)

set.seed(2025)
lay_giant <- layout_with_fr(g_giant)
par(mar = c(0, 0, 2, 0))
plot_bologna(g_giant, lay_giant, "Step 5: giant component only")

# STEP 6: LABEL ONLY WHAT MATTERS ----
# labelling all nodes = noise; label the top hubs (most incriminated people)
top <- order(V(g_giant)$indeg, decreasing = TRUE)[1:8]
lab <- rep(NA, vcount(g_giant))
lab[top] <- V(g_giant)$label[top]
plot_bologna(g_giant, lay_giant, "Step 6: the 8 most incriminated people")
text(lay_giant[top, 1] / max(abs(lay_giant)), lay_giant[top, 2] / max(abs(lay_giant)) - 0.06,
     labels = lab[top], cex = 0.6, font = 2)
# (igraph rescales coordinates to [-1, 1]; we do the same for text())

# STEP 7: EDGES THAT MEAN SOMETHING ----
# colour each tie by the (sender gender -> receiver gender) combination.
# This is the visual version of the question the ERGM answers with nodematch("gender").
ends_g <- ends(g_giant, E(g_giant), names = FALSE)
sg <- V(g_giant)$gender[ends_g[, 1]]
rg <- V(g_giant)$gender[ends_g[, 2]]
tie_type <- paste(ifelse(sg == 1, "M", "F"), "->", ifelse(rg == 1, "M", "F"))
pal <- c("M -> M" = "#0072B2", "F -> F" = "#E69F00",
         "M -> F" = "#CC79A7", "F -> M" = "#009E73")
E(g_giant)$color <- adjustcolor(pal[tie_type], alpha.f = 0.55)

plot(g_giant, layout = lay_giant, vertex.label = NA, vertex.size = 2.5,
     vertex.color = "grey85", vertex.frame.color = "grey60",
     edge.arrow.size = 0.2, edge.width = 0.7,
     main = "Step 7: ties coloured by sender -> receiver gender")
legend("bottomleft", legend = names(pal), col = pal, lwd = 2, bty = "n", cex = 0.8)

# NUMBERS BEHIND THE PICTURE (bridge to ERGM) ----
# does the picture suggest homophily? compare observed vs expected-by-chance shares
mix <- table(sender = ifelse(V(g)$gender[ends(g, E(g), names = FALSE)[, 1]] == 1, "man", "woman"),
             receiver = ifelse(V(g)$gender[ends(g, E(g), names = FALSE)[, 2]] == 1, "man", "woman"))
mix
round(prop.table(mix, 1), 2)             # row %: whom do men / women incriminate?
round(prop.table(table(V(g)$gender)), 2) # chance baseline: share of men / women among nodes
assortativity_nominal(g, types = V(g)$gender + 1, directed = TRUE)
# > 0 = same-gender ties more common than random. A plot cannot test this:
# an ERGM with nodematch("gender") + controls (see ergm.r) can.

# STEP 8: COMMUNITIES ----
# detect groups on the undirected giant component, colour nodes by group.
# Infomap respects direction; Louvain is fast and a common default.
set.seed(2025)
g_und <- as_undirected(g_giant, mode = "collapse")
cl <- cluster_louvain(g_und)
length(cl); round(modularity(cl), 3)
sizes(cl)

# first 7 communities get their own colour, the rest are grey
grp   <- membership(cl)
big_c <- as.integer(names(sort(table(grp), decreasing = TRUE))[1:7])
pal_c <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
V(g_giant)$comm_col <- ifelse(grp %in% big_c, pal_c[match(grp, big_c)], "grey80")

plot(g_giant, layout = lay_giant, vertex.label = NA,
     vertex.color = V(g_giant)$comm_col, vertex.frame.color = "white",
     vertex.size = log(V(g_giant)$indeg + 3) * 1.3,
     edge.color = adjustcolor("grey30", 0.25), edge.arrow.size = 0.15,
     main = "Step 8: Louvain communities (7 largest coloured)")
# Q: do communities line up with gender, kinship_id, or the inquisitor (inq_*)?
table(community = grp, gender = V(g_giant)$gender)[big_c, ]

# STEP 9: ANOTHER ATTRIBUTE, ANOTHER STORY ----
# heresy affiliation instead of gender: same layout, different colouring.
aff <- with(as_data_frame(g, "vertices"),
            ifelse(cathar_aff == 1, "Cathar",
            ifelse(apostle_aff == 1, "Apostle",
            ifelse(other_heterodoxy_aff == 1, "other heterodoxy", "none / unknown"))))
aff_pal <- c("Cathar" = "#D55E00", "Apostle" = "#009E73",
             "other heterodoxy" = "#CC79A7", "none / unknown" = "grey85")
table(aff)

par(mar = c(0, 0, 2, 0))
plot(g, layout = lay_fr, vertex.label = NA, vertex.color = aff_pal[aff],
     vertex.frame.color = "white", vertex.size = log(V(g)$indeg + 3) * 1.3,
     edge.color = adjustcolor("grey30", 0.25), edge.arrow.size = 0.15,
     main = "Step 9: heresy affiliation")
legend("bottomleft", legend = names(aff_pal), pch = 16, col = aff_pal, bty = "n", cex = 0.8)

# SAVE (publication quality) ----
# vector (pdf/svg) scales without loss; tiff/png need a resolution.
# The authors used tiff, 33 x 33 cm, 600 dpi, LZW compression.
png("bologna_fig_giant.png", width = 20, height = 20, units = "cm", res = 300, bg = "white")
par(mar = c(0, 0, 2, 0))
plot_bologna(g_giant, lay_giant, "Bologna incriminations: giant component")
dev.off()

pdf("bologna_fig_all.pdf", width = 10, height = 10)
par(mar = c(0, 0, 2, 0))
plot_bologna(g, lay_fr, "Bologna incriminations (1291-1310)")
dev.off()

# EXERCISES ----
# 1. Re-plot with node size = out-degree. Who are the "big accusers"? Compare with in-degree.
# 2. Colour by `churchperson` or `middle_class`. Does the picture change your story?
# 3. Show only ties of ONE inquisitor (inq_FV, inq_GV, inq_GP, inq_BdF): induced_subgraph().
# 4. Change set.seed() and recompute layout_with_fr(). What changes, what stays?
#    -> positions are arbitrary; only proximity / structure is meaningful.
# 5. Use layout_with_fr(g, weights = ...) or layout_nicely() and compare the result.
# 6. Which visual channel is most accurate for a number: position, size, or colour?
