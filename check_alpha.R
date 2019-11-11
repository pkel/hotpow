library(igraph)

g <- read_graph("result.xml", format="graphml")

# It seems that ATV assignment is working:
summary(lm(atv_count ~ alpha, data=vertex_attr(g)))
# plot(atv_count ~ alpha, data=vertex_attr(g))
