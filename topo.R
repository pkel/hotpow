library(igraph)

n.nodes <- 32
n.edges <- n.nodes * 6
g <- erdos.renyi.game(n.nodes, n.edges, type="gnm", directed=T, loops=F)

# only honest nodes
g <- set_vertex_attr(g, "strategy", value="naive")

# exponentially distributed computational power
g <- set_vertex_attr(g, "alpha", value=rexp(n.nodes, 1))

# latency & bandwidth. We assume that this depends on computational strength
lnk <- vertex_attr(g,"alpha", index=head_of(g, 1:n.edges)) *
  vertex_attr(g,"alpha", index=tail_of(g, 1:n.edges))
lnk <- lnk / mean(lnk)
lnk <- abs(rnorm(n.edges, lnk, 1))

g <- set_edge_attr(g, "latency",
                   value=paste("Uniform", lnk * 0.0005, lnk * 0.0015))
g <- set_edge_attr(g, "bandwidth",
                   value=paste("Uniform", lnk * 0.005, lnk * 0.015))

p <- function () {
  plot(g,
       # vertex.label= 1:n.nodes - 1,
       vertex.size=(5 + 5 * (vertex_attr(g, "alpha"))),
       edge.width=(1 + 0.3 * lnk),
       edge.arrow.size=0.5
  )
}

# p()

write_graph(g, "topo.xml", format="graphml")
