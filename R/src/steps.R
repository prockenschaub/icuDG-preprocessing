
library(glue)

load_step <- function(x, ...){
  UseMethod("load_step", x)
}

load_step.character <- function(x, ...){
  dict <- load_dictionary(src, x)
  
  if (length(x) == 1) {
    load_step(dict[[x]], ...)
  } else {
    load_step(dict[x], ...)
  }
}

load_step.character <- function(x, ...){
  dict <- load_dictionary(src, x)
  load_step(dict[x], ...)
}

load_step.concept <- function(x, merge_data = TRUE, ...){
  res <- lapply(x, load_step, ...)
  
  if (!merge_data) {
    res
  } else {
    merge_lst(res)
  }
}

load_step.cncpt <- function(x, cache = FALSE, ...){
  assert_that(ricu::is_cncpt(x))
  key <- field(x, 'name')
  # TODO: should this be limited to patients left in cohort
  # TODO: make cncpt_env src dependent
  # TODO: check interval that was previously loaded
  res <- get0(key, envir = cncpt_env, mode = "list", inherits = FALSE, 
              ifnotfound = NULL)
  if (is.null(res)) {
    res <- load_concepts(as_concept(x), src = src, ...)
    
    if (cache)
      assign(key, res, cncpt_env)
  }
  # TODO: decide whether asserts for the results are needed
  # TODO: decide whether copy is needed
  res
}


filter_step <- function(x, condition, ...){
  UseMethod("filter_step", x)
}

filter_step.data.table <- function(x, condition, col = data_col, ...) {
  assert_that(is_id_tbl(x) | is_ts_tbl(x))
  if (is.expression(condition)) {
    x[condition]
  } else if (is_win_tbl(condition)) {
    # TODO: refactor and make pretty
    id <- id_vars(x)
    ind <- index_var(x)
    sta <- index_var(condition)
    dur <- dur_var(condition)
    
    xjoin <- x[, .SD, .SDcols = meta_vars(x)]
    cjoin <- condition[, .SD, .SDcols = meta_vars(condition)]
    cjoin[, c("end") := .SD[[sta]] + .SD[[dur]]]
    xsel <- cjoin[xjoin, on = c(id, paste0(sta, "<=", ind), paste0("end>=", ind)), nomatch = 0]
    xsel <- xsel[, .SD, .SDcols = c(id, sta)]
    rename_cols(xsel, c(id, ind))
    
    x[unique(xsel)]
  } else {
    f <- rlang::as_function(condition)
    
    if (is.character(col)) {
      col <- function(x) get(col, -2L)
    }
    x[f(col(x))]
  }
}


mutate_step <- function(x, f, by = character(0), by_ref = FALSE, ...) {
  f <- rlang::as_function(f)
  if (!by_ref) {
    x <- copy(x)
  }
  x[, c(data_vars(x)) := lapply(.SD, f, ...), .SDcols = data_vars(x), by = c(by)]
}



summary_step <- function(x, f, ...) {
  UseMethod("summary_step", x)
}

summary_step.ts_tbl <- function(x, f, drop_index = FALSE, ...) {
  # TODO: think hard about what functions are needed here
  
  assert_that(is_ts_tbl(x))
  if (is_function(f)) {
    x[, lapply(.SD, f, ...), by = c(id_var(x)), .SDcols = !c(index_var(x))]
  } else if (is_character(f)) {
    f <- switch(
      f, 
      exists = summary_exists,
      count = summary_count,
      any = summary_any, 
      first = summary_first,
      last = summary_last
    )
    f(x, drop_index, ...)
  }
}

summary_exists <- function(x, ...){
  assert_that(is_ts_tbl(x))
  agg <- x[, .(var = TRUE), by=c(id_var(x))]
  setnames(agg, "var", data_var(x))
  agg
}

summary_count <- function(x, ...) {
  assert_that(is_ts_tbl(x))
  x[, .(n = .N), by = c(id_var(x))]
}

summary_any <- function(x, ...){
  assert_that(is_ts_tbl(x))
  
  id <- id_vars(x)
  ind <- index_var(x)
  dat <- data_vars(x)
  
  assert_that(all(x[, sapply(.SD, is.logical), .SDcols = dat]))
  
  agg <- x[, lapply(.SD, function(x) any(x)), .SDcols = dat, by=c(id)]
  agg
}

summary_first <- function(x, drop_index = FALSE, ...){
  assert_that(is_ts_tbl(x))
  x[, .SD[1], by=c(id_var(x))]
}

summary_last <- function(x, drop_index = FALSE, ...){
  assert_that(is_ts_tbl(x))
  x[, .SD[.N], by=c(id_var(x))]
}



function_step <- function(x, f, ...) {
  UseMethod("function_step", x)
}

function_step.list <- function(x, f, ...) {
  function_step.default(x, f, ...)
}

function_step.default <- function(x, f, ...) {
  do.call(f, c(list(x), list(...))) # TODO: allow for lambda functions
}