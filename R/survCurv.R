#' Plot the survival curve using only tidyverse
#'
#' This function takes numerical vectors status and a numerical vector time, and calculates
#' and plots a survival curve S(t).
#'
#' @param status A numerical vector indicating the status at time
#' @param time A numerical vector indicating the time at corresponding status
#' @import tibble
#' @import dplyr
#' @import ggplot2
#' @import tidyr
#' @return A plot of the survival curve S(t).
#' @export
#' @examples
#' data = read.csv("https://jlucasmckay.bmi.emory.edu/global/bmi510/Labs-Materials/survival.csv")
#' survCurv(data$status, data$time)
survCurv = function(status, time) {
  data = tibble::tibble(time = time, status = status)
  data = data %>% dplyr::arrange(time) %>% dplyr::mutate(event = dplyr::if_else(status == 1, 1, 0))
  #Group by time and summarize events
  data = data %>% dplyr::group_by(time) %>% dplyr::summarise(n_events = sum(event), n_at_risk = n(), .groups = 'drop')
  #Calculate number at risk
  data = data %>% dplyr::mutate(n_at_risk = rev(cumsum(rev(n_at_risk))))
  #Calculate the survival probability at each time point
  data = data %>% dplyr::mutate(survival_probability = 1 - (n_events / n_at_risk)) %>% dplyr::mutate(cumulative_survival = cumprod(survival_probability)) %>% dplyr::arrange(time)
  ggplot2::ggplot(data, ggplot2::aes(x = time, y = cumulative_survival)) + ggplot2::geom_step() + ggplot2::labs(title = "Kaplan-Meier Survival Curve", x = "Time", y = "Survival Probability") + ggplot2::theme_minimal()
}
