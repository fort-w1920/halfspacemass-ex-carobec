library(testthat)
library(depth)

source("halfspacemass-solution.R")

# result equals Tukey fuunction in package depth
test_that("turkey depth is right", {
  data_fig3 <- data.frame(z1 = c(-2, -.5, .5, 2), z2 = 0)
  depth_fig3 <- train_depth(data_fig3,
    n_halfspace = 1e4,
    scope = 1, seed = 4163
  )
  expect_equal(
    evaluate_depth(data = data_fig3, halfspaces = depth_fig3, metric = "depth"),
    sapply(
      1:nrow(data_fig3),
      function(x) {
        depth(data_fig3[x, ], data_fig3,
          method = "Tukey"
        )
      }
    )
  )
})

# write arguments
test_that("arguments checked", {
  data_fig3 <- data.frame(z1 = c(-2, -.5, .5, 2), z2 = 0)
  depth_fig3 <- train_depth(data_fig3,
    n_halfspace = 1e4,
    scope = 1, seed = 4163
  )
  expect_error(evaluate_depth(data = "data_fig3", halfspaces = "depth_fig3", metric = "depth"), regexp = "*type*")
  expect_error(evaluate_depth(data = data_fig3, halfspaces = depth_fig3, metric = "tukey"), regexp = "*'arg' should be one of*")
})
