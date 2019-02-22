library(ergmito)
library(network)
library(sna)
library(magrittr)

set.seed(12312)

nets <- replicate(5, {
  network(
    rbernoulli(4, .5),
    vertex.attr = list(rpois(4, 20)),
    vertex.attrnames = "age")
  }, simplify = FALSE)

examplers <- lapply(nets, function(net) {

  new_rergmito(net ~ edges + nodeicov("age"), theta = c(-4, .2))

})

ans <- ergmito(nets ~ edges + nodeicov("age"))
summary(ans)

nets2 <- lapply(examplers, function(i) i$sample(1L, nvertex(i$network0))[[1L]])
ergmito(nets2 ~ edges + nodeicov("age")) %>%
  summary

library(gnet)
set.seed(1001)
s <- function(g, y) {
  mean(sapply(g, function(i) summary(i ~ mutual)) - y)
}

w <- struct_test(examplers, y = rnorm(5), stat = s, R = 100)

usethis::use_data(examplers)

