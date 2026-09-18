# aprlab03

<!-- badges: start -->
[![R-CMD-check](https://github.com/totoposeb/aprlab03/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/totoposeb/aprlab03/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

aprlab03 implements two algorithms written for computer lab 3 of
732A94 Advanced R Programming at Linköping University:

- `euclidean()`: the greatest common divisor of two integers, using the
  [Euclidean algorithm](https://en.wikipedia.org/wiki/Euclidean_algorithm).
- `dijkstra()`: the shortest distance from a node to every other node in a
  weighted graph, using
  [Dijkstra's algorithm](https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm).

It also includes `wiki_graph`, the example graph from Wikipedia.

## Installation

``` r
# install.packages("devtools")
devtools::install_github("totoposeb/aprlab03")
```

## Example

``` r
library(aprlab03)

euclidean(100, 1000)
#> [1] 100

dijkstra(wiki_graph, 1)
#> [1]  0  7  9 20 20 11
```


