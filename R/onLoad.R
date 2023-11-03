#' @importFrom sysfonts font_add_google
#' @importFrom showtext showtext_auto
.onLoad <- function(libname, pkgname) {
  sysfonts::font_add_google(name = "Roboto Condensed",
                            family = "Roboto Condensed",
                            db_cache = TRUE)
  showtext::showtext_auto()
}
