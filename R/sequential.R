


map_to_grid <- function(x) {
  grid <- ricu::expand(patients)
  merge(grid, x, all.x = TRUE)
}


pad_rollind <- function(x, n, ...) {
  assert_that(all(is.na(x)) || !any(x < 0, na.rm = TRUE))
  pad_rollsum(x, n, ...) > 0
}

pad_rollsum <- function(x, n, ...) {
  dots <- rlang::dots_list(...)
  if (dots$align == "left") {
    xpad <- c(x, rep(NA, n))
    idx <- 1:length(x)
  } else if (dots$align == "right") {
    xpad <- c(rep(NA, n), x)
    idx <- (n+1):length(xpad)
  } else if (dots$align == "center") {
    xpad <- c(rep(NA, floor(n / 2)), x, rep(NA, ceiling(n / 2)))
    idx <- (floor(n / 2)+1):(length(xpad)-ceiling(n / 2))
  }
  
  res <- frollsum(xpad, n, ...)
  res[idx]
}


outcome_window <- function(x, window = 0L) {
  assert_that(all(window >= 0), length(window) <= 2, !has_gaps(x))
  
  if (length(window) == 1)
    window <- rep(window, 2L)
  
  id <- id_vars(x)
  val <- data_var(x)
  
  if (sum(window) > 0){
    # TODO: think about moving this into a separate function
    x[, c(val) := pad_rollind(.SD[[val]], window[1] + 1L, align = "left", na.rm = TRUE), by = c(id)]
    x[, c(val) := pad_rollind(.SD[[val]], window[2] + 1L, align = "right", na.rm = TRUE), by = c(id)]
  }
}


set_window <- function(x, value = NA, window = 0L) {
  assert_that(all(window >= 0), length(window) <= 2, !has_gaps(x))
  
  if (length(window) == 1)
    window <- rep(window, 2L)
  
  id <- id_vars(x)
  ind <- index_var(x)
  val <- data_var(x)
  
  if (sum(window) > 0){
    # TODO: think about moving this into a separate function
    x[window[1] <= get(ind) & get(ind) < window[2], c(val) := value]
  }
}


