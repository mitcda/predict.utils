## context("predict.utils")

test_that("splice_series.default fails well",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  A <- c(100, 95, 125, 150,  NA,  NA,  NA,  NA);
  B <- c(NA,  NA,  NA, 100, 120, 150, 200, 225);

  expect_error(splice_series(letters[1:8], B, base.period=4))    ## x - non-numeric
  expect_error(splice_series(A, letters[1:8], base.period=4))    ## y - non-numeric
  expect_error(splice_series(A, B, base.period=4))               ## base.period of incorrect length
  expect_error(splice_series(A, B, base.period=rep(4,3)))        ## base.period of incorrect length
  expect_error(splice_series(A, B, base.period=c(10,3)))         ## base.period x index out of range
  expect_error(splice_series(A, B, base.period=c(3,10)))         ## base.period y index out of range
})


test_that("splice_series works on standard vectors",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  A <- c(100, 95, 125, 150,  NA,  NA,  NA,  NA);
  B <- c(NA,  NA,  NA, 100, 120, 150, 200, 225);

  expect_equal(typeof(splice_series(A, B, base.period=c(4,4))), typeof(A))
  expect_equal(class(splice_series(A, B, base.period=c(4,4))), class(A))
  expect_equal(typeof(splice_series(B, A, base.period=c(4,4), direction="before")), typeof(B))
  expect_equal(splice_series(A, B, base.period=c(4,4)), 8);
  expect_equal(splice_series(B, A, base.period=c(4,4), direction="before"), 8);
})



test_that("splice_series.ts fails well",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  A <- ts(c(100, 95, 125, 150,  NA,  NA,  NA,  NA), start=1991, frequency=4);
  B <- ts(c(NA,  NA,  NA, 100, 120, 150, 200, 225), start=1991, frequency=4);
  B.ann <- ts(c(NA,  NA,  NA, 100, 120, 150, 200, 225), start=1991, frequency=1);
  expect_error(splice_series(A, c(NA,  NA,  NA, 100, 120, 150, 200, 225),
                             base.period=c(1991,4)))             ## y - Not ts
  expect_error(splice_series(A, B, base.period=4))               ## base.period of incorrect length
  expect_error(splice_series(A, B,
                             base.period=as.Date("1991-12-01"))) ## base.period not numeric
  expect_error(splice_series(A, B.ann, base.period=c(1991,4)))   ## x & y of different frequencies
  expect_error(splice_series(A, B, base.period=c(1993,1)))       ## base.period y index out of range
})


test_that("splice_series works on time series (ts) objects",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  A <- ts(c(100, 95, 125, 150,  NA,  NA,  NA,  NA), start=1991, frequency=4);
  B <- ts(c(NA,  NA,  NA, 100, 120, 150, 200, 225), start=1991, frequency=4);

  expect_equal(typeof(splice_series(A, B, base.period=c(1991,4))), typeof(A)) 
  expect_equal(class(splice_series(A, B, base.period=c(1991,4))), class(B))
  expect_length(splice_series(A, B, base.period=c(1991,4)), 8);
}
)


test_that("splice_series.xts fails well",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  A <- xts(x=c(100, 95, 125, 150,  NA,  NA,  NA,  NA),
              order.by=seq(as.Date("1991-03-01"), by="quarter", length.out=8));
  B <- xts(x=c(NA,  NA,  NA, 100, 120, 150, 200, 225),
           order.by=seq(as.Date("1991-03-01"), by="quarter", length.out=8));
  B.POSIXct <- xts(x=c(NA,  NA,  NA, 100, 120, 150, 200, 225),
           order.by=seq(as.POSIXct("1991-03-01"), by="quarter", length.out=8));
  B.ann <- xts(x=c(NA,  NA,  NA, 100, 120, 150, 200, 225),
               order.by=seq(as.Date("1991-06-01"), by="year", length.out=8));
  A.ts <- ts(c(100, 95, 125, 150,  NA,  NA,  NA,  NA), start=1991, frequency=4);
  B.ts <- ts(c(NA,  NA,  NA, 100, 120, 150, 200, 225), start=1991, frequency=4);

  expect_error(splice_series(A, B.ts,
                             base.period=as.Date("1991-12-01")))    ## y - Not xts
  expect_error(splice_series(A, B.POSIXct,
                             base.period=as.Date("1991-12-01")))    ## Object time indexes of different class
  expect_error(splice_series(A, B,
                             base.period=as.POSIXct("1991-12-01"))) ## base.period not of same time class
  expect_error(splice_series(A, B, base.period=as.Date("1993-03-01"))) ## base.period y index out of range
  expect_error(splice_series(A, B, base.period=c(1993,1)))          ## x & y of different frequencies
})


test_that("splice_series works on extensible time series (xts) objects",
{
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()

  A <- xts(x=c(100, 95, 125, 150,  NA,  NA,  NA,  NA),
              order.by=seq(as.Date("1991-03-01"), by="quarter", length.out=8));
  B <- xts(x=c(NA,  NA,  NA, 100, 120, 150, 200, 225),
            order.by=seq(as.Date("1991-03-01"), by="quarter", length.out=8));
 
  expect_equal(typeof(splice_series(A, B, base.period=as.Date("1991-12-01"))), typeof(A))
  expect_equal(class(splice_series(A, B, base.period=as.Date("1991-12-01"))), class(A))
  expect_equal(nrow(splice_series(A, B, base.period=as.Date("1991-12-01"))), 8);
}
)
