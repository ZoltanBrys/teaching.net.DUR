# HYPERGRAPH VISUALIZATION: Bologna testimonies as hyperedges ----
# Idea: in a graph a tie joins exactly TWO nodes. In a hypergraph a
# hyperedge joins ANY number of nodes.
# In the Bologna register, one deponent (SENDER) gives ONE testimony in which
# he/she incriminates several persons (RECEIVERS) at once:
#
#       testimony = hyperedge = { sender } + { receiver_1, ..., receiver_k }
#
# A hyperedge is an unordered set, so "who spoke" must be marked separately:
#   sender   = square filled with the colour of ITS testimony
#   receiver = white circle
# Two views, same colours: (1) hulls, (2) bipartite (incidence) graph.
#
# Data:    Zenodo 13935366 (df_nodes.tsv, df_edges.tsv), see bologna_network_viz.r
# Package: HyperG (CRAN, built on igraph) | https://cran.r-project.org/package=HyperG

# INSTALL ----
if (!requireNamespace("HyperG", quietly = TRUE)) install.packages("HyperG")
library(HyperG)          # also attaches igraph

# DATA ----
# df_nodes.tsv and df_edges.tsv sit next to this script (set the working directory first)
stopifnot(file.exists("df_nodes.tsv"), file.exists("df_edges.tsv"))
nodes <- read.delim("df_nodes.tsv", sep = "\t", header = TRUE, fileEncoding = "UTF-8")
edges <- read.delim("df_edges.tsv", sep = "\t", header = TRUE, fileEncoding = "UTF-8")

# TESTIMONIES -> HYPERGRAPH ----
# one testimony per sender: receivers = everybody this sender incriminated
receivers_by_sender <- split(edges$to, edges$from)
size <- sapply(receivers_by_sender, function(r) length(unique(r)) + 1)  # +1 = the sender
summary(size)       # many small, a few huge testimonies -> we draw a small sample

# pick 4 mid-sized testimonies that overlap (share persons), so the picture is readable
cand <- receivers_by_sender[size >= 5 & size <= 12]
ov   <- sapply(names(cand), function(a)
  sum(sapply(names(cand), function(b)
    length(intersect(c(a, cand[[a]]), c(b, cand[[b]]))) > 0)))
pick <- names(cand)[order(-ov, sapply(cand, length))][1:4]

lab <- setNames(sub("^P0*", "", nodes$name), nodes$name)       # P0377 -> 377

sender_lab <- unname(lab[pick])                                # sender of each testimony
tlist <- lapply(pick, function(s) unname(lab[unique(c(s, receivers_by_sender[[s]]))]))
names(tlist) <- paste0("T", seq_along(pick))                   # T1 ... T4

H <- hypergraph_from_edgelist(tlist, v = unique(unlist(tlist)))
H
data.frame(testimony = names(tlist), sender = sender_lab,
           n_receivers = edge_orders(H) - 1)
hdegree(H)          # persons with degree > 1 appear in several testimonies

# COLOURS AND ROLES ----
# ONE colour per testimony, used for: its hull, its sender, its node and sender link
t_col <- c(T1 = "#E69F00", T2 = "#56B4E9", T3 = "#009E73", T4 = "#CC79A7")

v         <- hnames(H)                                  # vertex order used by the plot
is_sender <- v %in% sender_lab
# colour of the testimony a sender belongs to (receivers: white)
v_fill    <- ifelse(is_sender, t_col[match(v, sender_lab)], "white")
# a sender can ALSO be a receiver in another testimony: keep the square, it sits
# inside two hulls (see 1788 in T1 / T3)

# PLOT 1: HULLS ----
plot_hulls_marked <- function(seed = 1) {
  par(mar = c(1, 1, 3, 1))
  set.seed(seed)
  plot(H, vertex.shape = ifelse(is_sender, "square", "circle"),
       vertex.color = v_fill, vertex.size = ifelse(is_sender, 20, 14),
       vertex.frame.color = "grey20", vertex.frame.width = ifelse(is_sender, 3, 1),
       vertex.label.cex = 0.7, vertex.label.font = ifelse(is_sender, 2, 1),
       mark.col = adjustcolor(t_col, 0.30), mark.border = adjustcolor(t_col, 0.95),
       main = "Bologna testimonies as hyperedges (hulls)")
  legend("bottomright", bty = "n", cex = 0.85, title = "testimony  (sender)",
         legend = paste0(names(tlist), "  (", sender_lab, ")"),
         fill = adjustcolor(t_col, 0.30), border = t_col)
  legend("bottomleft", bty = "n", cex = 0.85, title = "role",
         legend = c("sender", "receiver"), pch = c(22, 21),
         col = "grey20", pt.bg = c("grey60", "white"), pt.cex = 1.6)
}
plot_hulls_marked()
# Q: who sits in the overlap of two hulls? what does that mean historically?
#    (named by two different deponents = a tie BETWEEN testimonies)

# PLOT 2: BIPARTITE (INCIDENCE) GRAPH ----
# every testimony becomes a node (square); person -> testimony links are edges.
b <- as.bipartite(H)                  # type TRUE = persons, FALSE = testimonies (e1, e2, ...)
V(b)$label <- ifelse(V(b)$type, V(b)$name,
                     names(tlist)[match(V(b)$name, paste0("e", seq_along(tlist)))])

ee   <- ends(b, E(b), names = FALSE)  # every edge links one person and one testimony
pers <- ifelse(V(b)$type[ee[, 1]], ee[, 1], ee[, 2])
test <- ifelse(V(b)$type[ee[, 1]], ee[, 2], ee[, 1])
t_of_edge      <- V(b)$label[test]                                 # "T1" ...
is_sender_edge <- V(b)$label[pers] == sender_lab[match(t_of_edge, names(tlist))]

# sender link: thick, in the colour of its testimony; receiver link: thin grey
E(b)$color <- ifelse(is_sender_edge, t_col[t_of_edge], "grey70")
E(b)$width <- ifelse(is_sender_edge, 4, 1)

b_is_sender <- V(b)$type & V(b)$label %in% sender_lab
b_fill <- ifelse(!V(b)$type, t_col[V(b)$label],             # testimony node: its colour
          ifelse(b_is_sender, t_col[match(V(b)$label, sender_lab)], "white"))

plot_incidence_marked <- function(seed = 1) {
  par(mar = c(1, 1, 3, 1))
  set.seed(seed)
  plot(b, layout = layout_with_fr(b),
       vertex.shape = ifelse(!V(b)$type | b_is_sender, "square", "circle"),
       vertex.color = b_fill,
       vertex.size = ifelse(!V(b)$type, 22, ifelse(b_is_sender, 16, 12)),
       vertex.frame.color = "grey20", vertex.frame.width = ifelse(b_is_sender, 3, 1),
       vertex.label.cex = 0.7, vertex.label.font = ifelse(V(b)$type & !b_is_sender, 1, 2),
       main = "Bologna testimonies as hyperedges (bipartite)")
  legend("bottomleft", bty = "n", cex = 0.85, title = "legend",
         legend = c("testimony (hyperedge)", "sender (thick link)", "receiver (thin link)"),
         pch = c(22, 22, 21), col = "grey20", pt.bg = c("grey60", "grey60", "white"),
         pt.cex = 1.6)
}
plot_incidence_marked()

# SAVE ----
png("hypergraph_bologna_hulls.png", width = 20, height = 20, units = "cm", res = 250, bg = "white")
plot_hulls_marked()
dev.off()
png("hypergraph_bologna_bipartite.png", width = 20, height = 20, units = "cm", res = 250, bg = "white")
plot_incidence_marked()
dev.off()

# EXERCISES ----
# 1. Pick 4 other testimonies (change `size >= 5 & size <= 12`, or choose senders by id).
# 2. Compare the two plots: which one shows the overlaps better? which one scales to 10 testimonies?
# 3. hypergraph2graph(H) gives the ordinary graph: count its edges and compare with
#    sum(edges$from %in% pick). Why are there more? (co-receivers become linked)
# 4. Colour persons by gender (nodes$gender) instead of sender/receiver. Do senders differ?
# 5. Directed hypergraphs: the sender is the "tail", receivers the "head" of the hyperedge.
#    The thick coloured links in the bipartite plot are exactly this tail/head split.
