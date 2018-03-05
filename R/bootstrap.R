#' @title light bootstrap constructor
#' @description creates resampling indices but does not make entire copies of the dataset
#' @param d a data.frame, tibble or matrix 
#' @export
bootstrap <- function(data, times = 50L) {
  # Validate input
  !inherits(data, c("data.frame", "matrix")) && stop("d must be a data frame or numeric matrix")
  
  # Create samples for the most basic bootstrap scheme
  idx <- replicate(times, sample(nrow(data), replace = TRUE), simplify = FALSE)

  # Output boot_light object
  # new("bootstrap_light", data = data, indices = idx, times = as.integer(times))
  x <- list(data = data, idx = idx, times = as.integer(times))
  class(x) <- c("bootstrap")
  x
}

#' @title boot light indexing
#' @description simplifies accessing a bootstrap resample
#' @details use `x[i]` to access the i-th resample
#' @rdname bootstrap
#' @export
`[.bootstrap` <- function(x, i) x$data[x$idx[[i]], ]


#' @title Apply a function to each entrie of bootstrap
#' @description creates resampling indices but does not make entire copies of the dataset
#' @param x a bootstrap_light object
#' @param .f function to apply to x, can be formula, see the map in the purrr package for details
#' @export
bootstrap_map <- function(x, .f, times = 50L) {
  # Validate input
  !inherits(x, c("bootstrap")) && stop("x must be of class bootstrap")
  !inherits(.f, c("function", "formula")) && stop(".f must be a function or formula")
  
  # Create samples for the most basic bootstrap scheme
  .f <- purrr:::as_mapper(.f)
  purrr::map(1:x$times, function(i) .f(x[i]))
}

#' @title transpose list
#' @description Often in bootstrap we want to transpose lists. We want a list such that each entry i
#' is the i-th entry of every element in the original list.
#' @param list must have the same number of elements in every entry
#' @details  Each entry of list must be coerced to list is coerced to a vector. An error will happen 
#'  if this is not possible. The names of the new list will be taken from the first element of `list`.
#' @export
transpose <- function(l) {
  new_names <- names(l[[1]])
  new_l <- l %>% 
    purrr::reduce(cbind) %>% 
    split(1:nrow(.))
  names(new_l) <- new_names
  new_l
}
