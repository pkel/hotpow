library(igraph)

# For now, we generate a random graph. In practice, we would load a topology or
# a simulation result from a graphml file.
#
# g <- read_graph("topo-or-result.xml", format="graphml")

n.nodes <- 32
n.edges <- n.nodes * 6
g <- erdos.renyi.game(n.nodes, n.edges, type="gnm", directed=T, loops=F)
# exponentially distributed computational power
alpha <- rexp(n.nodes, 1)
g <- set_vertex_attr(g, "alpha", value=alpha)
lnk <- vertex_attr(g,"alpha", index=head_of(g, 1:n.edges)) * vertex_attr(g,"alpha", index=tail_of(g, 1:n.edges))
lnk <- lnk / mean(lnk)
lnk <- abs(rnorm(n.edges, lnk, 1))

g <- set_edge_attr(g, "delta_vote", value = lnk * 0.001)
g <- set_edge_attr(g, "delta_block", value = lnk * 0.1)

# Fix the layout for the graph
l <- layout_nicely(g)

# This function generates tikz code for the nodes of graph
tikz.nodes <- function(g, size,..., layout) {
  if (missing(layout)) {
    layout <- layout_nicely(g)
  }
  if (missing(size)) {
    size <- rep(1,dim(layout)[1])
  }
  # map coordinates onto [0,1]
  rescale <- function(v) {
    v <- v - min(v)
    v <- v / max(v)
    round(v, digits=3)
  }
  x <- rescale(layout[,1])
  y <- rescale(layout[,2])
  size <- round(size, digits=2)
  i <- 1:dim(layout)[1]
  paste0("\\node[node,minimum width=",size,"em] (n",i,") at (",x,",",y,") {};")
}
# tikz.nodes(g)

# This function generates tikz code for the edges of a graph
tikz.edges <- function(g, width) {
  if (missing(width)){
    width <- 1
  }
  width <- width / max(2, width)
  width <- pmax(0.1, width)
  e <- get.edgelist(g)
  paste0("\\path[edge,->,line width=",width,"pt] (n",e[,1],") -- (n",e[,2],");")
}
# tikz.edges(g, lnk)


# Write some properties into tikz/tex files
writeLines(tikz.nodes(g, alpha, layout=l), con="nodes-alpha.tex")
writeLines(tikz.edges(g), con="edges-1.tex")
writeLines(tikz.edges(g, lnk), con="edges-lnk.tex")

# Plot using R for comparison
plot(g,
     vertex.label="",
     vertex.size=(5 + 5 * (vertex_attr(g, "alpha"))),
     edge.width=(1 + 0.3 * lnk),
     edge.arrow.size=0.5,
     layout=l
)
