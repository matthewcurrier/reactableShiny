#' Bare reactable theme
#'
#' A minimal [reactable::reactableTheme()] with transparent borders and
#' background. Used as the default theme in [flexible_table_server()].
#'
#' Header weight is `bold`. "Bare" means the theme adds no chrome of its own —
#' it does not mean the header should be quieter than reactable's own default
#' (`.rt-th { font-weight: 600 }`), which is what the previous `normal` made
#' it. A table header is a heading; every consuming app wanted it bold and had
#' to override this to get there.
#'
#' Worth knowing when overriding: reactable does not emit a theme as a
#' stylesheet. It compiles the theme to a generated class injected into
#' `<head>` at runtime, which ties with `.rt-th` on specificity and wins on
#' source order — so a property declared here beats a host app's plain CSS
#' rule, and the host needs `!important` (or its own `reactable_theme`) to take
#' it back. Declare as little as possible here for that reason.
#'
#' @importFrom reactable reactableTheme
#'
#' @noRd
theme_bare <- reactable::reactableTheme(
  borderColor = "transparent",
  headerStyle = list(borderBottom = "none", fontWeight = "bold"),
  backgroundColor = "transparent"
)
