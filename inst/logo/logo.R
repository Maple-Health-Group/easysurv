surv_data <- easysurv::easy_lung[1:100, ] |>
  dplyr::mutate(
    time = time,
    event = status - 1,
    strata = sex
  ) |>
  dplyr::mutate_at("strata", as.factor)

dists <- c("exp",
           "gompertz",
           "lnorm")

times <- seq(
  from = 0,
  to = ceiling(max(surv_data$time) * 5),
  length.out = 200
)

models <- easysurv::fit_models(
  data = surv_data,
  time = "time",
  event = "event",
  predict_by = "strata",
  dists = dists
)

fit_plots <- easysurv::predict_and_plot(
  fit_models = models,
  data = surv_data,
  eval_time = times
)

p <- fit_plots[["plots"]][["1"]][["surv_plots"]]
p <- p +
  ggplot2::labs(title = NULL, subtitle = NULL, color = NULL) +
  ggplot2::guides(color = "none")
p <- p + ggplot2::xlim(10, 50) + ggplot2::ylim(0, 0.6)
p <- p + ggplot2::theme_classic() +
  ggplot2::theme(axis.line = ggplot2::element_line(color = "#595959"),
                 axis.ticks = ggplot2::element_blank())
p <- p + ggplot2::theme(axis.text = ggplot2::element_blank(),
                        axis.title = ggplot2::element_blank())
p <- p + ggpubr::theme_transparent()
p

s <- hexSticker::sticker(p,
                         package = "easysurv",
                         p_size = 25,
                         s_x = 1,
                         s_y = 0.9,
                         s_width = 2.2,
                         s_height = 1.1,
                         p_color = "#93100E",
                         h_fill = "#F2F2F2",
                         h_color = "#4E8098",
                         h_size = 1.2,
                         filename = "man/figures/logo2.png",
                         dpi = 300)

plot(s)

usethis::use_logo("man/figures/logo.png", retina = TRUE)
