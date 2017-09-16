#' @name AvData
#' @docType data
#' @title Australian aviation freight regression data set
#' @format A data frame with 129 rows and 49 variables from first quarter 1985 to first quarter 2017.
#' @description A dataset containing Australian aviation freight model dependent and independent variables.  Key variables include:
#'   \item{date}{quarterly}
#'   \item{Freight.TKM}{Quarterly Australian air freight tonne kilometres}
#'   \item{Aircraft.Departures}{Australian air freight tonne kilometres}
#'   \item{A2304113C}{Quarterly Australian Gross National Expenditure (GNE), chain volume measures: seasonally adjusted}
#'   \item{A2304402X}{Quarterly Australian Gross Domestic Product (GDP), chain volume measures: seasonally adjusted}
#'   \item{dQx}{Quarterly dummy variables}
#'   \item{dPilot.QtrYY}{Australian pilot dispute dummy variables}
#'   ...
#' 
#' @source Australian aviation statistics \url{https://bitre.gov.au/publications/ongoing/domestic_airline_activity-time_series.aspx} and Australian national accounts \url{http://www.abs.gov.au/ausstats/abs@.nsf/mf/5206.0}.
#' @keywords datasets
"AvData"




#' @name FcstData
#' @docType data
#' @title Australian aviation freight forecasting data set
#' @format A data frame with 282 rows and 49 variables, comprising actual observations from first quarter 1985 to first quarter 2017 and independent variable forecasts from first quarter 2017 to second quarter 2055.
#' @description A dataset containing Australian aviation freight model dependent and independent variable forecast data.  Key variables include:
#'   \item{date}{quarterly}
#'   \item{Freight.TKM}{Quarterly Australian air freight tonne kilometres}
#'   \item{Aircraft.Departures}{Australian air freight tonne kilometres}
#'   \item{A2304113C}{Quarterly Australian Gross National Expenditure (GNE), chain volume measures: seasonally adjusted}
#'   \item{A2304402X}{Quarterly Australian Gross Domestic Product (GDP), chain volume measures: seasonally adjusted}
#'   \item{dQx}{Quarterly dummy variables}
#'   \item{dPilot.QtrYY}{Australian pilot dispute dummy variables}
#'   ...
#' 
#' @source Australian aviation statistics \url{https://bitre.gov.au/publications/ongoing/domestic_airline_activity-time_series.aspx} and Australian national accounts \url{http://www.abs.gov.au/ausstats/abs@.nsf/mf/5206.0}.
#' @keywords datasets
"FcstData"
