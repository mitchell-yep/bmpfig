#' @rdname format_hyperfile
#' @name format_hyperfile
#' @title Format hyperfile
#' @description  A function to reduce the size of the Tableau hyperfile for quick recall
#' This function allows you to identify indicators or OUs of interest for figure development
#' @param hyperfile Choose the imported csv of the hyperfile of choice
#' @param indicator create a vector list of the indicators of interest
#' @param OU create a vector list of the operating units of interest
#' @param fiscal_year create a vector list of the fiscal years of interset (i.e. c("FY19", "FY20", ...))
#' @return sorts the hyperfile with the indicator, OU, and FY of interest
require(dplyr)
require(magrittr)
format_hyperfile <- function(hyperfile, indicator, OU, fiscal_year){
  hyperfile <- hyperfile %>%
    filter(indicator %in% indicator &
             operating_unit %in% OU &
             FY %in% fiscal_year)
  return(hyperfile)
}
