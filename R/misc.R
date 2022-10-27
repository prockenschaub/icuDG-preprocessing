
set_default <- function(x, value) {
  if (!exists(x)) {
    assign(x, value, pos = parent.frame())
  }
}


exclude <- function(df, exclusions){
  excl <- exclusions[[1]]
  incl <- df[!excl]
  incl_n <- nrow(incl)
  excl_n_total <- nrow(excl)
  excl_n <- nrow(df) - nrow(incl)
  
  if (length(exclusions) == 1) {
    return(list(
      incl = incl, 
      incl_n = incl_n,
      excl_n_total = excl_n_total, 
      excl_n = excl_n))
  }
  
  res <- exclude(incl, exclusions[-1])
  res[['incl_n']] <- c(incl_n, res[['incl_n']])
  res[['excl_n_total']] <- c(excl_n_total, res[['excl_n_total']])
  res[['excl_n']] <- c(excl_n_total, res[['excl_n']])
  res
}
