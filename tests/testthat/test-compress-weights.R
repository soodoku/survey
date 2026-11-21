test_that("compression preserves distinct weights that print identically", {
  adjacent <- 1 + .Machine$double.eps
  weights <- cbind(a = c(1, adjacent, 1, adjacent), b = c(2, 2, 3, 2))
  compressed <- compressWeights(weights)

  expect_identical(compressed$index, c(1L, 2L, 3L, 2L))
  expect_identical(compressed$weights, weights[1:3, , drop = FALSE])
  expect_identical(as.matrix(compressed), weights)
})

test_that("compression preserves dimensions, exceptional values and row order", {
  matrices <- list(matrix(2L, 8, 3), matrix(1:2, 1), matrix(c(3, 1, 3), 3),
                   matrix(numeric(), 0, 3), matrix(numeric(), 3, 0),
                   cbind(c(Inf, -Inf, NaN, NA, NaN, NA), 0))
  for (weights in matrices) {
    compressed <- compressWeights(weights)
    expect_identical(as.matrix(compressed), weights)
    rows <- lapply(seq_len(nrow(weights)), function(i) weights[i, ])
    expect_identical(compressed$weights, weights[!duplicated(rows), , drop = FALSE])
  }
})

test_that("compression leaves replicate estimates and their covariance intact", {
  data(api)
  design <- svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)
  dense <- as.svrepdesign(design, type = "JK1", compress = FALSE)
  compact <- compressWeights(dense)
  expect_equal(svymean(~api00 + api99, compact, return.replicates = TRUE),
               svymean(~api00 + api99, dense, return.replicates = TRUE),
               tolerance = 0)
  expect_equal(svytotal(~api00 + api99, compact, return.replicates = TRUE),
               svytotal(~api00 + api99, dense, return.replicates = TRUE),
               tolerance = 0)
})
