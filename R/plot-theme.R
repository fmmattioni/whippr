#' Whippr ggplot2 theme
#'
#' This theme was inspired by the plots from the Acta Physiologica Journal
#'
#' @return a ggplot2 object
#' @export
#' @importFrom ggplot2 theme_light theme element_rect element_line element_text
theme_whippr <- function() {
  theme_light(base_size = 14, base_family = "Arial") +
    theme(
      panel.background = element_rect(fill = "#fefeda"),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black", size = 1),
      axis.text = element_text(color = "black", face = "bold"),
      axis.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold"),
      panel.border = element_rect(colour = NA)
    )
}
