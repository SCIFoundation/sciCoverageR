test_that("Function gives one answer for each row", {

  # Load necessary data
  data(example_equity, package = "sciCoverageR")
  data(drc_equity_tool_codes, package = "sciCoverageR")
  voq <- c(-0.379487647, -0.286513899, -0.060301474, 0.653964958)

  result <- equity_calculation(data = example_equity, vectorOfQuintiles = voq, equity_file = drc_equity_tool_codes)
  expect_equal(nrow(result), nrow(example_equity))
})

test_that("Order of columns does not matter", {

  # Load necessary data
  data(example_equity, package = "sciCoverageR")
  data(drc_equity_tool_codes, package = "sciCoverageR")
  voq <- c(-0.384090795, -0.3583393779, 0.111673076, 0.554555339)

  result <- equity_calculation(data = example_equity, vectorOfQuintiles = voq, equity_file = drc_equity_tool_codes)

  example_equity_2 <- example_equity[, sample(1:length(example_equity), length(example_equity))]
  result_2 <- equity_calculation(data = example_equity_2, vectorOfQuintiles = voq, equity_file = drc_equity_tool_codes)
  expect_equal(result, result_2)
})
