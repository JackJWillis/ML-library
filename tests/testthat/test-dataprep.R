context("data prep")

test_that("na indicators generated correctly continuous", {
  testdata <- data.frame(x=c(1., 2., 3.), y=c(1., NA, 3.), z=c(NA, NA, 3.))
  indicated <- na_indicator(testdata)

  expect_equal(indicated$x, testdata$x)
  expect_equal(indicated$y, c(1., 0., 3.))
  expect_equal(indicated$z, c(0., 0., 3.))

  expect_equal(indicated$x.NA, NULL)
  expect_equal(indicated$y.NA, c(FALSE, TRUE, FALSE))
  expect_equal(indicated$z.NA, c(TRUE, TRUE, FALSE))
})

test_that("na indicators generated correctly categorical", {
  testdata <- data.frame(
    x=as.factor(c(1, 2, 3)),
    y=as.factor(c(1, NA, 3)),
    z=as.factor(c(NA, NA, 3)),
    a=as.factor(c(TRUE, FALSE, TRUE)),
    b=as.factor(c(TRUE, NA, NA)),
    c=as.factor(c(TRUE, FALSE, NA)))
  indicated <- na_indicator(testdata)

  expect_equal(indicated$x, testdata$x)
  expect_equal(indicated$y, factor(c(1, MISSINGNESS_INDICATOR, 3)))
  expect_equal(indicated$z, factor(c(MISSINGNESS_INDICATOR, MISSINGNESS_INDICATOR, 3)))
  expect_equal(indicated$a, factor(c(TRUE, FALSE, TRUE)))
  # indicator_na appends to levels, levels must match for testthat
  expect_equal(indicated$b, factor(c(TRUE, MISSINGNESS_INDICATOR, MISSINGNESS_INDICATOR),
                                   levels=c(TRUE, MISSINGNESS_INDICATOR)))
  expect_equal(indicated$c, factor(c(TRUE, FALSE, MISSINGNESS_INDICATOR),
                                   levels=c(FALSE, TRUE, MISSINGNESS_INDICATOR)))
})
