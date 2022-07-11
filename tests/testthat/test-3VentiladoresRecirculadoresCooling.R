test_that("check result type", {
  out <- class(getPred.rf.3FansRecirculatorsCooling(27.4, 25.2, 70.5, 22.2, 21.7, 100, 98, 414, 477, 1980))
  expected <- c("data.frame")

  expect_equal(out, expected)
})

test_that("check model prediction", {
  out <- getPred.rf.3FansRecirculatorsCooling(27.4, 25.2, 70.5, 22.2, 21.7, 100, 98, 414, 477, 1980)
  expected <- data.frame(Target = TRUE, Humidity = 73.3, Consumption = 1875.5)

  expect_equal(out, expected)
})

test_that("check model prediction1", {
  out <- getPred.rf.3FansRecirculators(27.0, 26.6, 64.5, 21.4, 21.6, 63, 63, 924, 925, 120)
  expected <- data.frame(Target = TRUE, Humidity = 64.5, Consumption = 98.7)

  expect_equal(out, expected)
})

test_that("check model prediction2", {
  out <- getPred.rf.2FansRecirculators(26.7, 26.3, 76.4, 21.6, 21.4, 70, 70, 80, 80, 50)
  expected <- data.frame(Target = TRUE, Humidity = 71, Consumption = 35.9)

  expect_equal(out, expected)
})

test_that("check humidity type", {
  out <- class(getPred.rf.finalHumidity(3, 27.4, 25.2, 70.5, 22.2, 21.7, 100, 98, 414, 477, 1980))
  expected <- "numeric"

  expect_equal(out, expected)
})

test_that("check humidity estimation", {
  out <- getPred.rf.finalHumidity(3, 27.4, 25.2, 70.5, 22.2, 21.7, 100, 98, 414, 477, 1980)
  expected <- 73.3

  expect_equal(out, expected)
})
