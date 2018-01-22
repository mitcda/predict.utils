### Splice multiple series
#' @name splice_series
#' @title Splice together two time series objects.
#' @description Function to splice two vectors, covering different, but overlapping periods, into a single consistent time series.
#' @param x Original (base) time series.
#' @param y Splicing time series, to be applied to the base time series.
#' @param base.period Integer object comprising the base period observation. See \link{Details} for more detail explanation about the appropriate format of \code{base.period}. 
#' @param direction One of either 'after' (default) or 'before'.
#' @return An object of the same class as object \code{x}. 
#' @export
#' @author David Mitchell <david.p.mitchell@@homemail.com.au>
#' @details The function splices together two time series objects, by applying indexed movements in series \code{y} to base series \code{x}.  Both objects must be of the same frequency (i.e. annual, quarterly, monthly, etc.).  The function also allows for splicing 'forward' (Default)---i.e. extending an existing series beyond the base period---and 'backward'---i.e. splicing values onto the start of a series using the  
#'
#' For plain (non-time series) vectors, \code{base.period} must be a two-element integer vector identifying the base period observation index for \code{x} and \code{y}, respectively.  For time series (\code{ts}) objects, \code{base.period} , must be a two element vector specifying the year and year-part (e.g. \code{c(1994, 2)} for the second quarter 1994.  For extensible time series objects (\code{xts}), \code{base.period} must be a time-based class---one of ‘Date’, ‘POSIXct’, ‘timeDate’, ‘yearmon’ or ‘yearqtr’, and match the time-based index class of series \code{x}, and be within the range of \code{x} and \code{y}.
#' 
#' @seealso Other R implementations to splice time series include: \code{\link[tframe]{splice}}
#' @examples
#'
#'   ## Non-time series (default) method example
#'   A <- c(100, 95, 125, 150,  NA,  NA,  NA,  NA);
#'   B <- c(NA,  NA,  NA, 100, 120, 150, 200, 225);
#'   splice_series(A, B, base.period=c(4,4));
#'   splice_series(B, A, base.period=c(4,4), direction="before");
#'
#'   ## Time series (ts) object method example
#'   A <- ts(c(100, 95, 125, 150,  NA,  NA,  NA,  NA), start=1991, frequency=1);
#'   B <- ts(c(NA,  NA,  NA, 100, 120, 150, 200, 225), start=1991, frequency=1);
#'   splice_series(A, B, base.period=c(1994,1));
#'   splice_series(B, A, base.period=c(1994,1), direction="before");
#'
#'   ## Extensible time series (xts) object method example
#'   A <- xts(x=c(100, 95, 125, 150,  NA,  NA,  NA,  NA),
#'            order.by=seq(as.Date("1991-03-01"), by="quarter", length.out=8));
#'   B <- xts(x=c(NA,  NA,  NA, 100, 120, 150, 200, 225),
#'            order.by=seq(as.Date("1991-03-01"), by="quarter", length.out=8));
#'   splice_series(A, B, base.period=as.Date("1994-03-01"));
#'   splice_series(B, A, base.period=as.Date("1994-03-01"), direction="before");
#' 
splice_series <- function(x, ...) {
  UseMethod("splice_series")
}


splice_series.default <- function(x, y, base.period, direction="after")
{
  ## if ( !class(x) %in% c("numeric","integer") )
  ##   stop(sprintf("%s should be of type integer or numeric", substitute(x)));
  if ( !class(y) %in% c("numeric","integer") )
    stop(sprintf("%s should be of type integer or numeric", deparse(substitute(y))));
  if ( length(base.period) != 2 )
    stop("base.period should be of length 2");
  if ( !class(base.period) %in% c("numeric", "integer") )
    stop("base.period should be of type integer");
    if ( !base.period[1] %in% seq_along(x) ) 
    stop(sprintf("First element of base.period outside %s index range.", deparse(substitute(x))));
  if ( !base.period[2] %in% seq_along(y) ) 
    stop(sprintf("First element of base.period outside %s index range.", deparse(substitute(y))));
  ## Return time series base.period indexes 
  idx.x <- base.period[1]
  idx.y <- base.period[2]
  ## Splice xts time series
  if (direction == "after")
    z <- c(x[1:idx.x], x[idx.x] * y[(idx.y+1):length(y)] / y[idx.y])
  if (direction == "before")
    z <- c(x[idx.x] * y[1:(idx.y-1)] / y[idx.y], x[idx.x:length(x)])
  return(z)
}


splice_series.ts <- function(x, y, base.period, direction="after")
{
  if ( class(y) != "ts" )
    stop(sprintf("%s should be of class ts", deparse(substitute(y))));
  if ( length(base.period) != 2 )
    stop("base.period should be of length 2");
  if ( !class(base.period) %in% c("numeric", "integer") )
    stop("base.period should be of type integer");
  if ( frequency(x) != frequency(y))
    stop(sprintf("Objects %s and %s of different frequencies. base.period should be of type integer",
                 deparse(substitute(x)), deparse(substitute(y))));
  if ( !round(base.period[1] + (base.period[2]-1)/frequency(x),3) %in% time(x) ) 
    stop(sprintf("%s time range does not span base.period", deparse(substitute(x))));
  if ( !round(base.period[1] + (base.period[2]-1)/frequency(y),3) %in% time(y) ) 
    stop(sprintf("%s time range does not span base.period", deparse(substitute(x))));
  ## Return time series base.period indexes 
  idx.x <- which(round(base.period[1] + (base.period[2]-1)/frequency(x),3) == time(x))
  idx.y <- which(round(base.period[1] + (base.period[2]-1)/frequency(y),3) == time(y))
  ## Splice xts time series
  if (direction == "after")
    z <- ts(splice_series.default(c(x), c(y), base.period=c(idx.x, idx.y), direction=direction),
            start=start(x), frequency=frequency(x))
  if (direction == "before")
    z <- ts(splice_series.default(c(x), c(y), base.period=c(idx.x, idx.y), direction=direction),
            end=end(x), frequency=frequency(x));
  return(z)
}


splice_series.xts <- function(x, y, base.period, direction="after")
{
  if ( class(y)[1] != "xts" )
    stop(sprintf("%s should be of class xts", deparse(substitute(y))));
  if ( class(index(x)) != class(index(y)) )
    stop(sprintf("Object time indexes of different classes (%s, %s)---should be of same class",
                 class(index(x)), class(index(y))));
  if ( !class(base.period) %in% c("Date","POSIXct","timeDate","yearmon","yearqtr") )
    stop("base.period should be a time-based class (see documentation)");
  if ( class(base.period) != class(index(x)) )
    stop(sprintf("base.period not same time-based class as time index of x (%s)---should be of same class",
                 class(index(x))));
  if ( !base.period %in% index(x) ) 
    stop(sprintf("%s time range does not span base.period", deparse(substitute(x))));
  if ( !base.period %in% index(y) ) 
    stop(sprintf("%s time range does not span base.period", deparse(substitute(y))));
  ## Return time series base.period indexes 
  idx.x <- which(base.period == index(x))
  idx.y <- which(base.period == index(y))
  ## Splice xts time series
  if (direction == "after")
    z <- xts(splice_series.default(c(coredata(x)), c(coredata(y)),
                                   base.period=c(idx.x, idx.y), direction=direction),
             order.by=c(index(x)[1:idx.x], index(y)[-(1:idx.y)]))
  if (direction == "before")
    z <- xts(splice_series.default(c(coredata(x)), c(coredata(y)),
                                   base.period=c(idx.x, idx.y), direction=direction),
             order.by=c(index(y)[1:(idx.y-1)], index(x)[-(1:(idx.x-1))]))
  return(z)
}


## #' @param method One of either 'vector' (default) or 'iterative' (NOT IMPLEMENTED).
## splice_series.original <- function(x, y, base.period=NULL, method="vector", direction="forward")
## {
##   if (method == "iterative") {
##     for (i in seq_along(x)[-1]) 
##       x[i] <- ifelse(!is.na(x[i]), x[i], x[i-1] * y[i] / y[i-1]);
##   }
##   if (method == "vector") {
##     idx <- is.na(x) %>% which
##     x[idx] <- x[min(idx)-1] * y[idx] / y[min(idx)-1];
##   }
##   return(x)
## }



### Iterative predict function
#' @name i_predict
#' @title Iteratively predict model results
#' @description Function to iteratively predict/forecast from specified \code{model} object, primarily for predicting models that include lagged dependent variables
#' @importFrom formula.tools lhs
#' @export
#' @param object a model object for prediction
#' @param ... additional arguments applying to the predictions.
#' @return The form of the value returned by predict depends on the class of its argument. See the documentation of the particular methods for details of what is produced by that method.
#' @details This function provides a general approach to the problem of producing forecasts, or out of sample predictions, from (linear) models containing lagged dependent variables.  Other approaches to producing rolling or iterative forecasts include: \href{https://www.r-bloggers.com/variations-on-rolling-forecasts/}{Variations on rolling forecasts} and \href{https://stackoverflow.com/questions/4856555/iteratively-forecasting-dyn-models}{Iteratively forecasting dyn models}.
#' @author David Mitchell <david.p.mitchell@@homemail.com.au>
#' @seealso \code{predict}, \code{predict.lm}, \code{predict.glm}.
#' @note Function not yet tested against all possible \code{predict} methods.
#' @examples
#'  data(AvData);
#'  lm.AirFrgt <- lm(log(Freight.TKM) ~ log(A2304402X) + log(Aircraft.Departures)
#'                   + dQ2 + dQ3 + dQ4 + dPilot.Sep89 + dPilot.Dec89 + dPilot.Mar90
#'                   + lag(log(Freight.TKM)), data = AvData);
#'  data(FcstData);
#'  FcstData <- transform(FcstData,
#'                        A2304113C = splice_series(A2304113C, Fcst_Real_GDP_level),
#'                        A2304402X = splice_series(A2304402X, Fcst_Real_GDP_level),
#'                        Aircraft.Departures = splice_series(Aircraft.Departures, Fcst_Aircraft.Departures));
#'  AvFcsts <- exp(i_predict(lm.AirFrgt, newdata=Fcst.AvData, interval="prediction"));
#'  tail(AvFcsts);
#'  
i_predict <- function(object, newdata, ...)
{
  if (missing(newdata) || is.null(newdata))
  {
    predicted <- predict(object, ...)
  } else {
    ## Return the dependent variable name and inverse function
    rawVars <- names(newdata);
    LHS <- formula.tools::lhs(formula(object)) %>% deparse
    lhsVar <- rawVars[sapply(rawVars, function(x) grepl(x, LHS))];
    lhsFn <- sub(lhsVar, "", LHS);
    invFn <- inverse_fn(lhsFn);
    ## Iteratively predict over the forecast horizon
    while(is.na(tail(newdata[,lhsVar],1))) {
      ## TO DO: Insert break if any rhsVar at .idx is NA
      ##  with message: 'Forecast dependent variable (..) is NA, provide non-NA forecast inputs'
      .fit <- predict(object, newdata)
      ## TO DO: Suggest handling function substitution entirely within 'inverse_fn'
      .fit <- eval(parse(text=sub("_", ".fit", invFn)));
      .idx <- which(is.na(newdata[,lhsVar]) & !is.na(.fit));
      ## Update newdata 
      newdata[.idx, lhsVar] <- .fit[.idx];
    }
    predicted <- predict(object, newdata, ...);
    ## Temporary check to ensure iterative predictions are updating, and not just producing NAs
    ##  TO DO: Check rhsVar
    if ( !all(is.na(predicted) == is.na(predicted0)) )
      stop(sprintf("Iterative predictions failing -- check dependent variable inputs in %s",
                   deparse(substitute(newdata))))
    predicted0 <- predicted
  }
  return(predicted);
}


### inverse_fn
#' @name inverse_fn
#' @title Return specified function inverse
#' @description Function to return the inverse of the supplied function
#' @param x valid built-in function character string or mathematical operation.
#' @return Returns the inverse of the function specified in \code{x} as a character string
#' @author David Mitchell <david.p.mitchell@@homemail.com.au>
#' @note Function explicitly defined for use in \code{i_predict}. Function tested against R built-in functions (e.g. log(), sin(), etc.), but not yet thoroughly tested for mathematical expressions (e.g. x'^2' or '2*'x).
#' @seealso{i_predict}
#' @examples
#'  fn <- "log()"
#'  inverse_fn(fn)
inverse_fn <- function(x)
{
  x <- switch(x,
              "log()"   = "exp(_)",
              "exp()"   = "log(_)",
              "log1p()" = "exp1m(_)",
              "exp1m()" = "log1p(_)",
              "sin()"   = "asin(_)",
              "asin()"  = "sin(_)",
              "cos()"   = "acos(_)",
              "acos()"  = "cos(_)",
              "tan()"   = "atan(_)",
              "atan()"  = "tan(_)",
              "sqrt()"  = "_^2")
  regex.pt <- "\\^\\(*((1/)*\\d+)\\)*$"
  if (grepl(regex.pt, x))
    x <- paste0("_(1/", sub(regex.pt, "\\1", x), ")")
  return(x);
}
