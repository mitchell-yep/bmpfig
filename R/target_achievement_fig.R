#' @rdname target_achievement_fig
#' @name target_achievement_fig
#' @title Target Achievement Figure
#' @description  A function to generate target achievement
#' @param hyperfile select the hyperfile dataframe
#' @param indicator select the indicator of interest
#' @param OU create a vector list of the operating units of interest
#' @param fiscal_year create a vector list of the fiscal years of interset (i.e. c("FY19", "FY20", ...))
#' @param source write a string of the data source being used
#' @return Produces a figure of target achievement for the given OU, Indicator, and FY
require(dplyr)
require(magrittr)
require(stringr)
require(glitr)
target_achievement_fig <- function(hyperfile, indicator, OU, agency, fiscal_year, source){
  if (is.null(agency)){
    ach_targets <- hyperfile %>%
      filter(indicator %in% indicator &
               standardized_disaggregate == "Total Numerator" &
               operating_unit %in% OU &
               FY %in% fiscal_year) %>%
      group_by(FY) %>%
      summarise(targets = sum(targets, na.rm = TRUE))


    ach_results <- hyperfile %>%
      filter(indicator %in% indicator &
               standardized_disaggregate == "Total Numerator" &
               operating_unit %in% OU &
               FY %in% fiscal_year) %>%
      group_by(FY) %>%
      summarise(cumulative = sum(cumulative, na.rm = TRUE))
    title <- str_c("Annual ", indicator, " ",
                    paste(agency, collapse = ' & '),
                    "Performance in ", OU, " ",
                    paste(fiscal_year, collapse = ' & '))
  } else{
    ## VMMC_CIRC
    ach_targets <- hyperfie %>%
      filter(indicator == indicator &
               standardized_disaggregate == "Total Numerator" &
               funding_agency == agency &
               operating_unit %in% OU &
               FY %in% fiscal_year) %>%
      group_by(FY) %>%
      summarise(targets = sum(targets, na.rm = TRUE))


    ach_results <- hyperfile %>%
      filter(indicator == indicator &
               standardized_disaggregate == "Total Numerator" &
               funding_agency == agency &
               operating_unit %in% OU &
               FY %in% fiscal_year) %>%
    group_by(FY) %>%
      summarise(cumulative = sum(cumulative, na.rm = TRUE))
    title <- str_c("Annual ", indicator, " ",
                   "Performance in ", OU, " ",
                    paste(fiscal_year, collapse = ' & '))
  }
  achievement <- ach_targets %>%
    left_join(ach_results, by = "FY") %>%
    mutate(per_ach = str_c(round(100*(cumulative/targets), 0), "%"),
           ach = case_when((cumulative/targets) < 0.75 ~ "< 75%",
                           (cumulative/targets) >= 0.75 & (cumulative/targets) <= 0.9 ~ "75% - 90%",
                           (cumulative/targets) >= 0.9 & (cumulative/targets) < 1.1 ~ "90% - 110%",
                           (cumulative/targets) >= 1.1 ~ "> 110%"))

  graph <- achievement %>%
    ggplot(aes(x = FY)) +
    geom_bar(stat = "identity", aes(y = targets), fill = "grey") +
    geom_bar(stat = "identity", aes(y = cumulative, fill = ach), width = 0.7) +
    scale_fill_manual(breaks = c("< 75%", "75% - 90%", "90% - 110%", "> 110%"), values = c("#FF939A", "#FFCAA2", "#5BB5D5", "#E6E6E6"), name = "Percent Achievement:") +
    geom_text(aes(x = FY, y = cumulative-(cumulative/8)), label = achievement$per_ach, size = 5) +
    scale_y_continuous() +
    ylab(print(indicator)) +
    xlab("Fiscal Year") +
    ggtitle(title) +
    labs(captions = str_c("Source: ", source)) +
    si_style() +
    theme(legend.position = "bottom",
          panel.grid.major.x = element_blank())
  return(graph)
}
