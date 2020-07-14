test_that("Function gives one answer for each row", {

  # Load necessary data
  data(hh, package = "sciCoverageR")
  data(eq_answers, package = "sciCoverageR")
  voq <- c(-0.384090795, -0.3583393779, 0.111673076, 0.554555339)
  eq <- hh[, 9:19]

  result <- equity_calculation(data = eq, vectorOfQuintiles = voq, equity_file = eq_answers)
  expect_equal(nrow(result), nrow(eq))
})

test_that("Order of columns does not matter", {

  # Load necessary data
  data(hh, package = "sciCoverageR")
  data(eq_answers, package = "sciCoverageR")
  voq <- c(-0.384090795, -0.3583393779, 0.111673076, 0.554555339)
  eq <- hh[, 9:19]

  result <- equity_calculation(data = eq, vectorOfQuintiles = voq, equity_file = eq_answers)

  eq_2 <- eq[, sample(1:length(eq), length(eq))]
  result_2 <- equity_calculation(data = eq, vectorOfQuintiles = voq, equity_file = eq_answers)
  expect_equal(result, result_2)
})
