#' Bare reactable theme
#'
#' A minimal [reactable::reactableTheme()] with transparent borders and
#' background. Used as the default theme in [flexible_table_server()].
#'
#' @importFrom reactable reactableTheme
#'
#' @noRd
theme_bare <- reactable::reactableTheme(
  borderColor = "transparent",
  headerStyle = list(borderBottom = "none", fontWeight = "normal"),
  backgroundColor = "transparent"
)
