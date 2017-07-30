## context("predict.utils")

test_that("splice_series works",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  A <- ts(c(100, 95, 125, 150,  NA,  NA,  NA,  NA), start=1991);
  B <- ts(c(NA,  NA,  NA, 100, 120, 150, 200, 225), start=1991);

  expect_type(splice_series(A, B), typeof(A))
  # expect_length()
}


test_that("splice_series fails well",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  # TO DO
}


test_that("i_predict works",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  # TO DO
}


test_that("i_predict fails well",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  # TO DO
}


test_that("inverse_fn works",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  # TO DO
}


test_that("inverse_fn fails well",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  # TO DO
}
