#' Convert color name to hexidecimal code
#'
#' @param cname Character. Name of an HTML color.
#'
#' @details Pulled from the `gplots` package. This version allows for compatibility with older versions of R.
#'
#' @return A hexidecimal color code for the provided color name.
#' @export
#'
#' @examples
#' col2hex("white")
#'
#' @importFrom grDevices col2rgb
#'
col2hex <- function (cname) {
  colMat <- col2rgb(cname)
  rgb(red = colMat[1, ]/255, green = colMat[2, ]/255, blue = colMat[3,
  ]/255)
}
