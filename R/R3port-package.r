#' Report functions for HTML and PDF files
#'
#' The R3port package is written to easily create pdf and html documents
#' including tables, listings and plots.
#'
#' The package makes use of the option system within R. The way it was implemented is to always use
#' an option in case it was set in options(). If an option is NULL, the function will evaluate the
#' argument.
#' The following options can be set within the package:
#' \itemize{
#'     \item show (see [ltx_doc()], [html_doc()])
#'     \item template (see [ltx_doc()], [html_doc()])
#'     \item css (see [html_doc()])
#'     \item compile (see [ltx_doc()])
#'     \item orientation (see [ltx_doc()])
#'     \item pwidth (see [ltx_plot()], [html_plot()])
#'     \item pheight (see [ltx_plot()], [html_plot()])
#'   }
#'
#' @name R3port
#' @import plyr reshape2 whisker tools
"_PACKAGE"
NULL
