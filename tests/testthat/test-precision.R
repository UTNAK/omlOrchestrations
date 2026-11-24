# Two-stage chain a -> b (f1) -> c (f2). f2 records what it receives.
binding <- function(q, v, u = "u") {
  list(quantity = list(value = q), value = list(value = v), unit = list(value = u))
}
chain <- function(f1) {
  env <- new.env()
  env$f1 <- f1
  env$f2 <- function(y) {
    env$received <- y$value
    list(value = y$value, unit = y$unit)
  }
  ograph <- igraph::graph_from_edgelist(matrix(c("a", "b", "b", "c"), ncol = 2, byrow = TRUE))
  cdf <- data.frame(quantity = c("b", "c"), call = c("f1", "f2"),
                    arg_name = c("x", "y"), arg_quantity = c("a", "b"))
  vdf <- asserted_quantity_values(list(binding("a", "1")))
  list(env = env, run = function() {
    # do.call() looks up the call name from calculate_quantity's caller chain
    list2env(list(f1 = env$f1, f2 = env$f2), globalenv())
    on.exit(rm(list = c("f1", "f2"), envir = globalenv()))
    calculation_wrapper(ograph, vdf, cdf)
  })
}

test_that("calculated doubles reach the next call unchanged", {
  for (v in list(0.1 + 0.2, 1 / 3, 1e-17, 123456789.123456789, pi * 1e300)) {
    ch <- chain(local({ v <- v; function(x) list(value = v, unit = "u") }))
    res <- ch$run()
    expect_identical(ch$env$received, v)
    expect_identical(res$value[[3]], v)
  }
})

test_that("vectors and NULL pass through", {
  for (v in list(c(1 / 3, 2 / 3, 1), c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6), NULL)) {
    ch <- chain(local({ v <- v; function(x) list(value = v, unit = "u") }))
    res <- ch$run()
    expect_identical(ch$env$received, v)
    expect_identical(res$value[[3]], v)
  }
})

test_that("SPARQL literals are parsed without losing digits", {
  x <- c(0.1 + 0.2, 1 / 3, 1e-17, 123456789.123456789, 0.123457)
  vdf <- asserted_quantity_values(Map(binding, letters[seq_along(x)], sprintf("%.17g", x)))
  expect_identical(unlist(vdf$value), x)
  expect_identical(asserted_quantity_values(list(binding("a", "12.5")))$value[[1]], 12.5)
})

test_that("a vdf with a character value column still works", {
  ograph <- igraph::graph_from_edgelist(matrix(c("a", "s", "b", "s"), ncol = 2, byrow = TRUE))
  cdf <- data.frame(quantity = "s", call = "sum2", arg_name = c("p", "q"), arg_quantity = c("a", "b"))
  assign("sum2", function(p, q) list(value = p$value + q$value, unit = "kg"), globalenv())
  on.exit(rm("sum2", envir = globalenv()))
  vdf <- data.frame(quantity = c("a", "b"), value = c("1.5", "2.25"), unit = "kg")
  res <- calculation_wrapper(ograph, vdf, cdf)
  expect_identical(res$quantity, c("a", "b", "s"))
  expect_identical(unlist(res$value), c(1.5, 2.25, 3.75))
  expect_identical(res$unit, c("kg", "kg", "kg"))
})
