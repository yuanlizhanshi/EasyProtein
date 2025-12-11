# ----------------------------------------------
# Utils:  helper functions for EasyProtein
# ----------------------------------------------

#' Scale matrix by row
#'
#' @description
#' Scale each row of a numeric matrix to mean 0 and sd 1.
#'
#' @param mtx A numeric matrix.
#'
#' @return A scaled matrix with the same dimensions.
#' @export
scale_mtx <- function(mtx) {
  new_mtx <- as.matrix(t(apply(mtx, 1, scale)))
  colnames(new_mtx) <- colnames(mtx)
  return(new_mtx)
}

#' Geometric mean
#'
#' @description
#' Compute geometric mean, optionally removing NA values.
#'
#' @param x Numeric vector.
#' @param na.rm Whether to remove NA values.
#'
#' @return A numeric value representing the geometric mean.
#' @export
geom_mean <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  exp(mean(log(x)))
}

#' Category table summarizer
#'
#' @description
#' Produce a table summarizing how many values fall within specified cut
#' categories, optionally cumulative.
#'
#' @param data Numeric vector.
#' @param categories Numeric vector of category boundaries.
#' @param cumulative Logical, whether cumulative counts should be returned.
#' @param na.rm Logical, remove NAs before computation.
#' @param digits Number of digits to round results.
#'
#' @return A 2-row matrix summarizing counts and proportions.
#' @export
catable <- function(data,
                    categories = c(
                      quantile(data, c(0.01, 0.1, 0.5, 0.9, 0.99),
                               na.rm = TRUE)
                    ),
                    cumulative = FALSE,
                    na.rm = TRUE,
                    digits = 3) {

  if (!is(data, "numeric"))
    stop("data should be numeric vector")
  if (!is(categories, "numeric"))
    stop("categories should be numeric vector")

  categories <- sort(categories)
  tot <- sum(!is.na(data), na.rm = na.rm)

  outmat <- matrix(0, nrow = 2, ncol = length(categories) + 1)

  # First category: X <= categories[1]
  outmat[1, 1] <- sum(data <= categories[1], na.rm = na.rm)
  outmat[2, 1] <- outmat[1, 1] / tot

  # Intermediate categories
  for (i in 1:(length(categories) - 1)) {
    outmat[1, i + 1] <- sum(data > categories[i] & data <= categories[i + 1], na.rm = na.rm)
    outmat[2, i + 1] <- outmat[1, i + 1] / tot
  }

  # Last category: X > max(categories)
  outmat[1, length(categories) + 1] <-
    sum(data > categories[length(categories)], na.rm = na.rm)
  outmat[2, length(categories) + 1] <-
    outmat[1, length(categories) + 1] / tot

  # Cumulative mode
  if (cumulative) {
    outmat[1, ] <- cumsum(outmat[1, ])
    outmat[2, ] <- cumsum(outmat[2, ])
  }

  # Column names
  cnams <- character(length(categories) + 1)
  cnams[1] <- paste0("X<=", categories[1])

  for (i in 1:(length(categories) - 1)) {
    if (cumulative)
      cnams[i + 1] <- paste0("X<=", categories[i + 1])
    else
      cnams[i + 1] <- paste0(categories[i], "<X<=", categories[i + 1])
  }

  if (cumulative)
    cnams[length(categories) + 1] <- "all X"
  else
    cnams[length(categories) + 1] <- paste0("X>", categories[length(categories)])

  colnames(outmat) <- cnams
  rownames(outmat) <- c("No", "Prop")

  round(outmat, digits = digits)
}
