#' @importFrom survminer ggsurvplot
#' @importFrom survival Surv
#' @importFrom survival survfit
#' @importFrom sysfonts font_add
#' @importFrom sysfonts font_add_google
#' @importFrom showtext showtext_auto
#' @importFrom grDevices pdf
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Loading easysurv ...",
    "\n",
    "Installing Roboto Condensed font ..."
  )

  # This only work with an internet connection
  sysfonts::font_add_google(
    name = "Roboto Condensed",
    family = "Roboto Condensed",
    db_cache = FALSE
  )

  # This doesn't work for sessions without admin rights.
  # font_path <- system.file("fonts", package = "easysurv")
  #
  # sysfonts::font_add(
  #   family = "Roboto Condensed",
  #   regular = paste(font_path, "RobotoCondensed-Regular.ttf", sep = "/"),
  #   bold = paste(font_path, "RobotoCondensed-Bold.ttf", sep = "/"),
  #   italic = paste(font_path, "RobotoCondensed-Italic.ttf", sep = "/"),
  #   bolditalic = paste(font_path, "RobotoCondensed-BoldItalic.ttf", sep = "/")
  # )

  showtext::showtext_auto()

  # Prevent creation of Rplots.pdf
  if (!interactive()) grDevices::pdf(NULL)

  # The first plot often presents font-related error messages, so try to print
  # a simple plot with suppressed warnings first.
  suppressWarnings(plot(survminer::ggsurvplot(
    survival::survfit(survival::Surv(time, status) ~ 1,
      data = data.frame(
        time = c(1, 2),
        status = c(0, 0)
      )
    ),
    ggtheme = theme_easysurv(),
    legend = "top",
    title = "Font initialization plot",
    subtitle = "If the font fails to load, try library(easysurv) again"
  )$plot))

  packageStartupMessage(
    paste0("easysurv version ",
           packageVersion("easysurv"),
           " loaded.")
    )
}

# Previous attempts that were associated with issues.
# suppressMessages(hrbrthemes::import_roboto_condensed())

# The load fonts method
# if (.Platform$OS.type == "windows") {
#  extrafont::loadfonts("win", quiet = TRUE)
# }
