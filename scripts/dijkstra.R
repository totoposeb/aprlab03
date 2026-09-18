dijkstra <- function(graph, init_node) {
  # 1.1.2 Returns the shortest path from init_node to every other node in graph
  # using the Dijkstra algorithm.

  # Checks graph is a data.frame with required columns
  stopifnot(
    "graph must be a data.frame!" = is.data.frame(graph),
    "graph must contain columns v1, v2 and w!" = all(
      c("v1", "v2", "w") %in% names(graph)
    ),
    "w in graph must be numeric and non-negative!" = is.numeric(graph$w) &&
      all(graph$w >= 0)
  )

  # Gets all nodes in graph
  nodes <- unique(c(graph$v1, graph$v2))

  is_valid_node <- function(x) {
    # Returns TRUE or FALSE if a node is a valid numeric integer
    tol <- .Machine$double.eps ^ 0.5
    is.numeric(x) && all(abs(x - round(x)) <= tol) && all(x >= 1)
  }

  # Checks all nodes in graph are valid, init_node is valid, and init_node
  # is in graph
  stopifnot(
    "Not all nodes are valid!" = is_valid_node(nodes),
    "init_node must be a scalar integer!" = is_valid_node(init_node) &&
      length(init_node) == 1,
    "init_node is not in graph!" = init_node %in% nodes
  )

  # Creates a vector to store the distances from init_node to each vertex
  dist <- rep(Inf, max(nodes))
  dist[init_node] <- 0

  Q <- nodes

  while (length(Q) > 0) {
    # Find which node has the smallest distance
    u <- Q[which.min(dist[Q])]
    Q <- Q[Q != u]

    edges <- graph[graph$v1 == u, ]
    for (i in seq_len(nrow(edges))) {
      alt <- dist[u] + edges$w[i]
      if (alt < dist[edges$v2[i]]) {
        dist[edges$v2[i]] <- alt
      }
    }
  }

  return(dist)
}