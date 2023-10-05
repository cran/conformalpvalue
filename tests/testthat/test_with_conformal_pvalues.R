test_that("conformal_pvalues generates valid p-values", {
  # Create some example data
  train_data <- data.frame(x1 = as.numeric(rnorm(50, 1, 2)),x2 = as.numeric(rnorm(50, 2.5, 3)), target=as.factor(rbinom(50, 1, 0.5)))
  calib_data <- data.frame(x1 = as.numeric(rnorm(50, 1, 2)),x2 = as.numeric(rnorm(50, 2.5, 3)), target=as.factor(rbinom(50, 1, 0.5)))
  test_data <- data.frame(x1=as.numeric(rnorm(50, 1, 2)),x2 = as.numeric(rnorm(50, 2.5, 3)))

  # Run your function
  p_values <- conformal_pvalues(train_data, calib_data, test_data, target="target",method="glm")

  # Check that the p-values are within [0, 1]
  expect_true(all(p_values >= 0 & p_values <= 1))
})
