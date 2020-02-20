getwd()
setwd("C:/Users/Gareth Lomax/Conflict/dummy_R/")
df <- read.csv('dummy_csv.csv')


groups <- dplyr::group_by(df, gid)
summary_deaths <- dplyr::summarize(groups, .var = sum(deaths))


# rebeltrack_casualty_count <- function(x, side, var, fill = 0, lag = 0,
#                                       weight = NULL) {
#   group <- x@dataset@events %>%
#     dplyr::mutate_(casualty_count = get_var_by_side("deaths_%s", side)) %>%
#     dplyr::group_by(actor, period_start)
#   
#   group_summary <- dplyr::summarize(group, .var = sum(casualty_count))
#   
#   data <- x %>%
#     weighted_lag(rlang::quo_name(rlang::enquo(var)),
#                  group,
#                  group_summary,
#                  fill,
#                  lag,
#                  weight)
#   
#   .rebeltrack_dataframe(dataset = x@dataset, data = data)
# }