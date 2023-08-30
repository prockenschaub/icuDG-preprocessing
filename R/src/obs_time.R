


stop_obs_at <- function(x, offset, by_ref = TRUE) {
  assert_that(is_atomic(offset), 
              ricu:::is_interval(offset), 
              units(offset) == units(index_col(x)), 
              units(offset) == units(index_col(patients)))
  
  if (!by_ref) {
    patients <- copy(patients)
    x <- copy(x)
  }
  
  ids <- id_vars(x)
  idx <- index_var(x)
  
  x <- x[, .(...time = min(.SD[[idx]]) + offset), by = c(ids)]
  patients[x, end := pmin(end, ...time), on = c(ids)] # Check if on=ids is correct
  invisible(patients)
}
