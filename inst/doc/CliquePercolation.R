## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(Matrix)
library(qgraph)
library(CliquePercolation)
library(colorspace)

## ---- echo = FALSE, dpi = 300, fig.cap = "**Unweighted network with eight nodes.**", fig.align = "center", out.extra = 'style = "border:none"', out.width = "60%"----
W <- matrix(c(0,1,1,1,0,0,0,0,
              0,0,1,1,0,0,0,0,
              0,0,0,0,0,0,0,0,
              0,0,0,0,1,1,1,0,
              0,0,0,0,0,1,1,0,
              0,0,0,0,0,0,1,0,
              0,0,0,0,0,0,0,1,
              0,0,0,0,0,0,0,0), nrow = 8, ncol = 8, byrow = TRUE)
W <- forceSymmetric(W)
rownames(W) <- letters[seq(from = 1, to = nrow(W))]
colnames(W) <- letters[seq(from = 1, to = nrow(W))]
qgraph(W, edge.width = 4)

## ---- echo = FALSE, dpi = 300, fig.cap = "**Six 3-cliques in unweighted network.**", fig.align = "center", out.extra = 'style = "border:none"', out.width = "60%"----
color1 <- c("#ff9600","#ff9600","#ff9600","#7f7f7f","#7f7f7f","#7f7f7f","#7f7f7f",
            "#7f7f7f","#7f7f7f","#7f7f7f","#7f7f7f","#7f7f7f")
color2 <- c("#ff9600","#7f7f7f","#7f7f7f","#ff9600","#ff9600","#7f7f7f","#7f7f7f",
            "#7f7f7f","#7f7f7f","#7f7f7f","#7f7f7f","#7f7f7f")
color3 <- c("#7f7f7f","#7f7f7f","#7f7f7f","#7f7f7f","#7f7f7f","#ff9600","#ff9600",
            "#ff9600","#7f7f7f","#7f7f7f","#7f7f7f","#7f7f7f")
color4 <- c("#7f7f7f","#7f7f7f","#7f7f7f","#7f7f7f","#7f7f7f","#ff9600","#7f7f7f",
            "#7f7f7f","#ff9600","#ff9600","#7f7f7f","#7f7f7f")
color5 <- c("#7f7f7f","#7f7f7f","#7f7f7f","#7f7f7f","#7f7f7f","#7f7f7f","#ff9600",
            "#7f7f7f","#ff9600","#7f7f7f","#ff9600","#7f7f7f")
color6 <- c("#7f7f7f","#7f7f7f","#7f7f7f","#7f7f7f","#7f7f7f","#7f7f7f","#7f7f7f",
            "#ff9600","#7f7f7f","#ff9600","#ff9600","#7f7f7f")
layout(matrix(c(1,2,3,
                4,5,6), ncol = 3, nrow = 2, byrow = TRUE))
qgraph(W, edge.color = color1, edge.width = 4)
qgraph(W, edge.color = color2, edge.width = 4)
qgraph(W, edge.color = color3, edge.width = 4)
qgraph(W, edge.color = color4, edge.width = 4)
qgraph(W, edge.color = color5, edge.width = 4)
qgraph(W, edge.color = color6, edge.width = 4)

## ---- echo = FALSE, dpi = 300, fig.cap = "**Two communities in unweighted network.**", fig.align = "center", out.extra = 'style = "border:none"', out.width = "60%"----
color7 <- c("#00AD9A","#00AD9A","#00AD9A","#00AD9A","#00AD9A","#E16A86","#E16A86",
            "#E16A86","#E16A86","#E16A86","#E16A86","#7f7f7f")
qgraph(W, edge.color = color7, edge.width = 4)

## ---- echo = FALSE, results = FALSE, dpi = 300, fig.cap = "**Community partition by node coloring in unweighted network.**", fig.align = "center", out.extra = 'style = "border:none"', out.width = "60%"----
cp <- cpAlgorithm(qgraph(W, DoNotPlot = TRUE), k = 3, method = "unweighted")
cpColoredGraph(qgraph(W, DoNotPlot = TRUE), cp$list.of.communities.labels, edge.width = 4)

## ---- echo = FALSE, dpi = 300, fig.cap = "**Weighted network with eight nodes.**", fig.align = "center", out.extra = 'style = "border:none"', out.width = "60%"----
layout <- qgraph(W, DoNotPlot = TRUE)$layout
W <- matrix(c(0,.1,.3,.3,0,0,0,0,
              0,0,.2,.2,0,0,0,0,
              0,0,0,0,0,0,0,0,
              0,0,0,0,.1,.1,.1,0,
              0,0,0,0,0,.1,.1,0,
              0,0,0,0,0,0,.1,0,
              0,0,0,0,0,0,0,-.2,
              0,0,0,0,0,0,0,0), nrow = 8, ncol = 8, byrow = TRUE)
W <- forceSymmetric(W)
rownames(W) <- letters[seq(from = 1, to = nrow(W))]
colnames(W) <- letters[seq(from = 1, to = nrow(W))]
qgraph(W, theme = "colorblind", cut = 0.02, edge.labels = TRUE, layout = layout)

## ---- echo = FALSE, dpi = 300, fig.cap = "**Surviving cliques in weighted network depending on I.**", fig.align = "center", out.extra = 'style = "border:none"', out.width = "60%"----
color8 <- c("#ff9600","#ff9600","#ff9600","#ff9600","#ff9600","#7f7f7f","#7f7f7f",
            "#7f7f7f","#7f7f7f","#7f7f7f","#7f7f7f","#7f7f7f")
color9 <- c("#ff9600","#ff9600","#ff9600","#ff9600","#ff9600","#ff9600","#ff9600",
            "#ff9600","#ff9600","#ff9600","#ff9600","#7f7f7f")
layout(matrix(c(1,2), ncol = 2, nrow = 1, byrow = TRUE))
qgraph(W, edge.color = color8, theme = "colorblind", cut = 0.02, edge.labels = TRUE,
       layout = layout, fade = FALSE, title = "k = 3, I = 0.17", title.cex = 0.5)
qgraph(W, edge.color = color9, theme = "colorblind", cut = 0.02, edge.labels = TRUE,
       layout = layout, fade = FALSE, title = "k = 3, I = 0.09", title.cex = 0.5)

## ----setup---------------------------------------------------------------
library(CliquePercolation)
library(qgraph)
library(Matrix)

## ---- echo = TRUE, dpi = 400, fig.cap = "**Weighted network with 150 nodes.**", fig.align = "center", out.extra = 'style = "border:none"', out.width = "100%"----
# create qgraph object; 150 nodes with letters as names; 1/7 of edges different from zero
W <- matrix(c(0), nrow = 150, ncol = 150, byrow = TRUE)
name.vector <- paste(letters[rep(seq(from = 1, to = 26), each = 26)],
                     letters[seq(from = 1, to = 26)], sep = "")[1:nrow(W)]
rownames(W) <- name.vector
colnames(W) <- name.vector

set.seed(4186)
W[upper.tri(W)] <- sample(c(rep(0,6),1), length(W[upper.tri(W)]), replace = TRUE)
rand_w <- stats::rnorm(length(which(W == 1)), mean = 0.3, sd = 0.1)
W[which(W == 1)] <- rand_w

W <- Matrix::forceSymmetric(W)
W <- qgraph::qgraph(W, theme = "colorblind", layout = "spring", cut = 0.4)

## ---- echo = TRUE, eval = FALSE------------------------------------------
#  thresholds <- cpThreshold(W, method = "weighted", k.range = c(3,4),
#                            I.range = c(seq(0.40, 0.01, by = -0.005)),
#                            threshold = c("largest.components.ratio","chi"))

## ---- echo = TRUE, eval = FALSE------------------------------------------
#  thresholds

## ---- echo = TRUE--------------------------------------------------------
cpk3I.35 <- cpAlgorithm(W, k = 3, method = "weighted", I = 0.35)
cpk4I.27 <- cpAlgorithm(W, k = 4, method = "weighted", I = 0.27)

## ---- echo = TRUE, results = FALSE---------------------------------------
cpk3I.35$list.of.communities.numbers

## ---- echo = TRUE, results = FALSE---------------------------------------
cpk3I.35$list.of.communities.labels

## ---- echo = TRUE, results = FALSE, dpi = 300, fig.cap = "**Community size distribution with k = 3 and I = 0.35.**", fig.align = "center", out.extra = 'style = "border:none"', out.width = "60%"----
cpCommunitySizeDistribution(cpk3I.35$list.of.communities.numbers)

## ---- echo = TRUE, results = FALSE, dpi = 300, fig.cap = "**Community size distribution with k = 4 and I = 0.27.**", fig.align = "center", out.extra = 'style = "border:none"', out.width = "60%"----
cpCommunitySizeDistribution(cpk4I.27$list.of.communities.numbers)

## ---- echo = TRUE, dpi = 400, fig.cap = "**Community network with k = 3 and I = 0.35.**", fig.align = "center", out.extra = 'style = "border:none"', out.width = "100%"----
commnetwork <- cpCommunityGraph(cpk3I.35$list.of.communities.numbers,
                                node.size.method = "proportional",
                                max.node.size = 20,
                                theme = "colorblind", layout = "spring", repulsion = 0.4)

## ---- echo = TRUE, results = FALSE---------------------------------------
commnetwork$community.weights.matrix

## ---- echo = FALSE, dpi = 300, fig.cap = "**Unweighted network with ten nodes.**", fig.align = "center", out.extra = 'style = "border:none"', out.width = "60%"----
W <- matrix(c(0,1,1,1,0,0,0,0,0,0,
              0,0,1,1,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0,0,
              0,0,0,0,1,1,1,0,0,0,
              0,0,0,0,0,1,1,0,0,0,
              0,0,0,0,0,0,1,0,0,0,
              0,0,0,0,0,0,0,1,1,0,
              0,0,0,0,0,0,0,0,1,0,
              0,0,0,0,0,0,0,0,0,1,
              0,0,0,0,0,0,0,0,0,0), nrow = 10, ncol = 10, byrow = TRUE)
W <- forceSymmetric(W)
rownames(W) <- letters[seq(from = 1, to = nrow(W))]
colnames(W) <- letters[seq(from = 1, to = nrow(W))]
W <- qgraph(W, edge.width = 4)

## ---- echo = TRUE, results = FALSE---------------------------------------
thresholds.small <- cpThreshold(W, method = "unweighted", k.range = c(3,4),
                                threshold = "entropy")

## ---- echo = TRUE--------------------------------------------------------
thresholds.small

## ---- echo = TRUE, results = FALSE---------------------------------------
set.seed(4186)
permute <- cpPermuteEntropy(W, cpThreshold.object = thresholds.small,
                            n = 100, interval = 0.95)

## ---- echo = TRUE--------------------------------------------------------
permute$Confidence.Interval

## ---- echo = TRUE--------------------------------------------------------
permute$Extracted.Rows

## ---- echo = TRUE--------------------------------------------------------
cpk3 <- cpAlgorithm(W, k = 3, method = "unweighted")

## ---- echo = TRUE--------------------------------------------------------
cpk3$list.of.communities.labels

## ---- echo = TRUE--------------------------------------------------------
cpk3$shared.nodes.labels

## ---- echo = TRUE--------------------------------------------------------
cpk3$isolated.nodes.labels

## ---- echo = TRUE, dpi = 300, fig.cap = "**Community coloring I.**", fig.align = "center", out.extra = 'style = "border:none"', out.width = "60%"----
colored.net1 <- cpColoredGraph(W, list.of.communities = cpk3$list.of.communities.labels,
                               edge.width = 4)

## ---- echo = TRUE, results = FALSE---------------------------------------
colored.net1$colors.communities
colored.net1$colors.nodes

## ---- echo = TRUE, dpi = 300, fig.cap = "**Community coloring II.**", fig.align = "center", out.extra = 'style = "border:none"', out.width = "60%"----
colored.net2 <- cpColoredGraph(W, list.of.communities = cpk3$list.of.communities.labels,
                               h.cp = c(50, 210), c.cp = 70, l.cp = 70,
                               edge.width = 4)

## ---- echo = TRUE, dpi = 300, fig.cap = "**Community coloring III.**", fig.align = "center", out.extra = 'style = "border:none"', out.width = "60%"----
#define list.of.sets
list.of.sets <- list(c("a","b","c","d","g","h","i"), c("e","f","j"))
colored.net3 <- cpColoredGraph(W, list.of.communities = cpk3$list.of.communities.labels,
                               list.of.sets = list.of.sets,
                               edge.width = 4)

## ---- echo = TRUE, results = FALSE---------------------------------------
colored.net3$basic.colors.sets

## ---- echo = FALSE, dpi = 300, fig.cap = "**Color patches.**", fig.align = "center", out.extra = 'style = "border:none"', out.width = "60%"----
swatchplot(colored.net3$basic.colors.sets)

## ---- echo = TRUE, dpi = 300, fig.cap = "**Community coloring IV.**", fig.align = "center", out.extra = 'style = "border:none"', out.width = "60%"----
colored.net4 <- cpColoredGraph(W, list.of.communities = cpk3$list.of.communities.labels,
                               list.of.sets = list.of.sets, set.palettes.size = 5,
                               edge.width = 4)

## ---- echo = TRUE, dpi = 300, fig.cap = "**Large network with 11 communities colored with qualitative_hcl.**", fig.align = "center", out.extra = 'style = "border:none"', out.width = "60%"----
#generate network with 11 communities
W <- matrix(c(0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
              0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
              0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,
              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,
              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,
              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,
              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,
              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,
              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,
              0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0), nrow = 25, ncol = 25, byrow = TRUE)
W <- forceSymmetric(W)
rownames(W) <- letters[seq(from = 1, to = nrow(W))]
colnames(W) <- letters[seq(from = 1, to = nrow(W))]
W <- qgraph(W, DoNotPlot = TRUE)

#run the clique percolation method
cpk3.large <- cpAlgorithm(W, k = 3, method = "unweighted")

#plot the network colored according to community partition (with qualitative_hcl)
colored.net.large1 <- cpColoredGraph(W, list.of.communities = cpk3.large$list.of.communities.labels,                                      edge.width = 4, layout = "spring", repulsion = 0.9)

## ---- echo = TRUE, dpi = 300, fig.cap = "**Large network with 11 communities colored with createPalette.**", fig.align = "center", out.extra = 'style = "border:none"', out.width = "60%"----
colored.net.large2 <- cpColoredGraph(W, list.of.communities = cpk3.large$list.of.communities.labels,
                                     larger.six = TRUE,
                                     edge.width = 4, layout = "spring", repulsion = 0.9)

