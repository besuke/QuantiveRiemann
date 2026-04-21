# ============================================================
# QuantiveRiemann - ir_curve_builders_basis.R
# Interest-rate basis curve builders
# Internal contract:
#   input spread must be decimal spread
#   e.g. -25bp -> -0.0025
# ============================================================


.qlr_ir_validate_basis_data <- function(basis_data) {
  required_cols <- c("tenor", "spread")
  missing_cols <- setdiff(required_cols, names(basis_data))

  if (length(missing_cols) > 0) {
    stop("basis_data is missing columns: ", paste(missing_cols, collapse = ", "))
  }

  invisible(TRUE)
}

.qlr_ir_basis_time_table <- function(curve_obj, basis_data) {
  ref_date <- curve_obj$referenceDate()
  dc <- curve_obj$dayCounter()

  basis_data |>
    dplyr::transmute(
      tenor = toupper(trimws(tenor)),
      spread = as.numeric(spread),
      target_date = purrr::map(tenor, ~ ref_date + qlr_period(.x)),
      date = purrr::map_chr(target_date, qlr_iso),
      time = purrr::map_dbl(target_date, ~ dc$yearFraction(ref_date, .x))
    ) |>
    dplyr::select(tenor, date, time, spread) |>
    dplyr::arrange(time)
}

.qlr_ir_basis_interp_fun <- function(time_vec, spread_vec) {
  stats::approxfun(
    x = time_vec,
    y = spread_vec,
    method = "linear",
    rule = 2
  )
}

qlr_ir_make_basis_curve <- function(
    basis_data,
    base_curve_env,
    spread_label = "basis",
    verbose = TRUE
) {
  .qlr_ir_validate_basis_data(basis_data)

  if (is.null(base_curve_env$curve)) {
    stop("base_curve_env must contain $curve")
  }

  curve_obj <- base_curve_env$curve

  .qlr_ir_msg(
    verbose,
    "[qlr_ir_make_basis_curve] start: ",
    if (!is.null(base_curve_env$currency)) base_curve_env$currency else "BASE",
    "::",
    if (!is.null(base_curve_env$instrument)) base_curve_env$instrument else "CURVE"
  )

  basis_tbl <- .qlr_ir_basis_time_table(curve_obj, basis_data)

  if (any(is.na(basis_tbl$spread))) {
    stop("basis_data contains non-numeric or missing spread values")
  }

  if (nrow(basis_tbl) < 2) {
    stop("basis_data must contain at least 2 tenor points")
  }

  spread_fun <- .qlr_ir_basis_interp_fun(
    time_vec = basis_tbl$time,
    spread_vec = basis_tbl$spread
  )

  out <- list(
    trade_date = base_curve_env$trade_date,
    settle_date = if (!is.null(base_curve_env$settle_date)) base_curve_env$settle_date else NA_character_,
    currency = if (!is.null(base_curve_env$currency)) base_curve_env$currency else NA_character_,
    instrument = if (!is.null(base_curve_env$instrument)) base_curve_env$instrument else NA_character_,
    spread_label = spread_label,
    base_curve = curve_obj,
    base_curve_env = base_curve_env,
    basis_tbl = basis_tbl,
    spread_fun = spread_fun
  )

  class(out) <- c("qlr_ir_basis_curve", class(out))

  .qlr_ir_msg(
    verbose,
    "[qlr_ir_make_basis_curve] done: points = ",
    nrow(basis_tbl)
  )

  out
}

qlr_ir_basis_spread <- function(basis_env, x) {
  tt <- qlr_curve_time(basis_env$base_curve, x)
  as.numeric(basis_env$spread_fun(tt))
}

qlr_ir_basis_zero_rate <- function(basis_env, x) {
  base_zero <- qlr_zero_rate_date(basis_env$base_curve, x)
  sprd <- qlr_ir_basis_spread(basis_env, x)

  if (is.na(base_zero) || is.na(sprd)) {
    return(NA_real_)
  }

  base_zero + sprd
}

qlr_ir_basis_discount <- function(basis_env, x) {
  tt <- qlr_curve_time(basis_env$base_curve, x)
  z <- qlr_ir_basis_zero_rate(basis_env, x)

  if (is.na(tt) || tt < 0 || is.na(z)) {
    return(NA_real_)
  }

  exp(-z * tt)
}

qlr_ir_basis_table <- function(
    basis_env,
    tenors = c("1M", "3M", "6M", "1Y", "2Y", "3Y", "5Y", "10Y", "20Y", "30Y")
) {
  ref_date <- basis_env$base_curve$referenceDate()

  tibble::tibble(
    tenor = tenors,
    target_date = purrr::map(tenors, ~ ref_date + qlr_period(.x)),
    date = purrr::map_chr(target_date, qlr_iso),
    spread = purrr::map_dbl(target_date, ~ qlr_ir_basis_spread(basis_env, .x)),
    base_zero = purrr::map_dbl(target_date, ~ qlr_zero_rate_date(basis_env$base_curve, .x)),
    basis_zero = purrr::map_dbl(target_date, ~ qlr_ir_basis_zero_rate(basis_env, .x)),
    basis_df = purrr::map_dbl(target_date, ~ qlr_ir_basis_discount(basis_env, .x))
  ) |>
    dplyr::select(tenor, date, spread, base_zero, basis_zero, basis_df)
}

