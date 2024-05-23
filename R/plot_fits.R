###########
# This file WAS CREATED for the new easysurv release.
###########

#' @import ggplot2
#' @importFrom tidyr pivot_longer
#' @export
plot_fits <- function(data) {
  # Pivot_longer so that ggplot2 is happy (requires data frame)
  long_data <- tidyr::pivot_longer(data,
                                   cols = -".eval_time",
                                   names_to = "Model",
                                   values_to = "Survival"
  )

  p <- ggplot(data = long_data, aes(x = .eval_time, y = Survival))
  p <- p + geom_line(aes(color = Model, group = Model))
  p <- p + labs(
    x = "Time",
    y = "Survival",
    color = ifelse(length(unique(long_data$Model)) == 1, "Model", "Models")
  )
  p <- p + theme_bw()

  # more can be done here to improve this.

  # may want an optional KM argument.

  return(p)
}
