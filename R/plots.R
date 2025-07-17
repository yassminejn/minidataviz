#' Plot a Histogram
#'
#' This function creates a histogram of a numeric vector using ggplot2.
#'
#' @param x A numeric vector.
#'
#' @return A ggplot2 histogram object.
#' @export
#'
#' @examples
#' plot_histogram(mtcars$mpg)
plot_histogram <- function(x) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.")
  }
  df <- data.frame(value = x)
  ggplot2::ggplot(df, ggplot2::aes(x = value)) +
    ggplot2::geom_histogram(bins = 30, fill = "skyblue", color = "black") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Histogram", x = "Value", y = "Frequency")
}


#' Plot a Scatterplot
#'
#' This function creates a scatterplot of two numeric vectors using ggplot2.
#'
#' @param x A numeric vector for the x-axis.
#' @param y A numeric vector for the y-axis.
#'
#' @return A ggplot2 scatterplot object.
#' @export
#'
#' @examples
#' plot_scatter(mtcars$mpg, mtcars$hp)
plot_scatter <- function(x, y) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.")
  }
  df <- data.frame(x = x, y = y)
  ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(color = "tomato") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Scatterplot", x = "X", y = "Y")
}

