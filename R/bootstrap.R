#' @title light bootstrap constructor
#' @description creates resampling indices but does not make entire copies of the dataset
#' @param d a data.frame, tibble or matrix 
#' @export
bootstrap <- function(data, times = 50L) {
  # Validate input
  !inherits(data, c("data.frame", "matrix")) && stop("d must be a data frame or numeric matrix")
  # Create samples for the most basic bootstrap scheme
  idx <- replicate(times, sample(nrow(data), replace = TRUE), simplify = FALSE)
  names(idx) <- paste0("sample", 1:times)
  # Output boot_light object
  # new("bootstrap_light", data = data, indices = idx, times = as.integer(times))
  x <- list(data = data, idx = idx, times = as.integer(times))
  class(x) <- c("bootstrap")
  x
}

x #' @title boot light indexing
#' @description bootstrap_light objects can be accessed as 3d arrays
#' @details `x[r, i, k]`
#' @rdname bootstrap_indexing
#' @export
# setClass("bootstrap_light",
#   slots = list(data = "data.frame", indices = "list", times = "integer"))
# setMethod(`[`, "bootstrap_light", function(x, i) {x@data[x@indices[[i]], , drop = FALSE]})
`[.bootstrap_light` <- function(x, r, i, j) x$data[x$idx[[r]], ][i, j]
# head.bootstrap_light <- function(x, n = 6L) head(x$data, n)
# tail.bootstrap_light <- function(x, n = 6L) tail(x$data, n)


#' @title Apply a function to each entrie of bootstrap
#' @description creates resampling indices but does not make entire copies of the dataset
#' @param x a bootstrap_light object
#' @param .f function to apply to x, can be formula, see the map in the purrr package for details
#' @export
bootstrap_map <- function(x, .f, times = 50L) {
  # Validate input
  !inherits(x, c("bootstrap")) && stop("x must be of class bootstrap")
  !inherits(FUN, c("function")) && stop("FUN must be a function")
  # Create samples for the most basic bootstrap scheme
  .f <- purrr:::as_mapper(.f)
  purrr::map(1:x$times, function(i) .f(x[i]))
}

