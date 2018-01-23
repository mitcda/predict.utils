### Iterative predict function
#' @name i_predict
#' @title Iteratively predicted values based on a model object
#' @description Function to iteratively predict/forecast from specified \code{model} object, primarily for predicting models that include lagged dependent variables
#' @param object a model object for predictions.
#' @param ... additional arguments affecting the predictions.
#' @return The form of the value returned by \code{i_predict} depends on the class of its argument. See details of the particular methods for details of what is produced by that method.
#' @details This function provides a general approach to the problem of producing forecasts, or out of sample predictions, from (linear) models containing lagged dependent variables.  Other approaches to producing rolling or iterative forecasts include: \href{https://www.r-bloggers.com/variations-on-rolling-forecasts/}{Variations on rolling forecasts} and \href{https://stackoverflow.com/questions/4856555/iteratively-forecasting-dyn-models}{Iteratively forecasting dyn models}.
#'
#' Most prediction methods that are similar to those for linear models have an argument \code{newdata} specifying the first place to look for explanatory variables to be used for prediction. Some considerable attempts are made to match up the columns in \code{newdata} to those used for fitting, for example that they are of comparable types and that any factors have the same level set in the same order (or can be transformed to be so). 
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
#' @export
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
    invFn <- .inverse_fn(lhsFn);
    ## Iteratively predict over the forecast horizon
    while(is.na(tail(newdata[,lhsVar],1))) {
      ## TO DO: Insert break if any rhsVar at .idx is NA
      ##  with message: 'Forecast dependent variable (..) is NA, provide non-NA forecast inputs'
      .fit <- predict(object, newdata)
      ## TO DO: Suggest handling function substitution entirely within '.inverse_fn'
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


### .inverse_fn
#' @name .inverse_fn
#' @title Return specified function inverse
#' @description Function to return the inverse of the supplied function
#' @param x valid built-in function character string or mathematical operation.
#' @return Returns the inverse of the function specified in \code{x} as a character string
#' @author David Mitchell <david.p.mitchell@@homemail.com.au>
#' @note Function explicitly defined for use in \code{i_predict}. Function tested against R built-in functions (e.g. log(), sin(), etc.), but not yet thoroughly tested for mathematical expressions (e.g. x'^2' or '2*'x).
#' @seealso{i_predict}
#' @examples
#'  fn <- "log()"
#'  .inverse_fn(fn)
.inverse_fn <- function(x)
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
