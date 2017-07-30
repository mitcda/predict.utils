
### Splice multiple series
#' @name splice_series
#' @title Splice two of more vectors into a single series
#' @description Function to iteratively splice two vectors, covering different bases, into series  extend 
#' @export
#' @param x Original (base) vector 
#' @param y New or future vector series to be applied to the base vector (\code{x})
#' @param base.period Base period (vector index) on which to splice the two series.  If NULL, then function replaces all NA values in \code{x} using values in \code{y}. (Currently unimplemented.)
#' @param method One of either 'vector' (default) or 'iterative'.
#' @param direction One of either 'forward' (default) or 'backward'. (Currently not implemented.)
#' @return An object of the same class as object \code{x}. 
#' @author Dave Mitchell <david.p.mitchell@@homemail.com.au>
#' @note Arguments \code{base.period} and \code{direction} are currently unimplemented.
#' @examples
#'   A <- ts(c(100, 95, 125, 150,  NA,  NA,  NA,  NA), start=1991);
#'   B <- ts(c(NA,  NA,  NA, 100, 120, 150, 200, 225), start=1991);
#'   splice_series(A, B);
splice_series <- function(x, y, base.period=NULL, method="vector", direction="forward")
{
  if (method == "iterative") {
    for (i in seq_along(x)[-1]) 
      x[i] <- ifelse(!is.na(x[i]), x[i], x[i-1] * y[i] / y[i-1]);
  }
  if (method == "vector") {
    idx <- is.na(x) %>% which
    x[idx] <- x[min(idx)-1] * y[idx] / y[min(idx)-1];
  }
  return(x)
}


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
#' @author Dave Mitchell <david.p.mitchell@@homemail.com.au>
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
    ## Iteratively predict over forecast horizon
    while(is.na(tail(newdata[,lhsVar],1))) {
      .fit <- predict(object, newdata)
      ## TO DO: Suggest handling function substitution entirely within 'inverse_fn'
      .fit <- eval(parse(text=sub("_", ".fit", invFn)));
      .idx <- which(is.na(newdata[,lhsVar]) & !is.na(.fit));
      ## Update newdata
      newdata[.idx, lhsVar] <- .fit[.idx];
    }
    predicted <- predict(object, newdata, ...);
  }
  return(predicted);
}


### inverse_fn
#' @name inverse_fn
#' @title Return specified function inverse
#' @description Function to return the inverse of the supplied function
#' @param x valid built-in function character string or mathematical operation.
#' @return Returns the inverse of the function specified in \code{x} as a character string
#' @author Dave Mitchell <david.p.mitchell@@homemail.com.au>
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
