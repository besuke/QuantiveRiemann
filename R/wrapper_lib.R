# ============================================================
# QuantiveRiemann - wrapper_lib.R
# QuantLib SWIG API wrappers
# ============================================================

# ------------------------------------------------------------
# internal helpers
# ------------------------------------------------------------

.qlr_try_num <- function(expr) {
  tryCatch(expr, error = function(e) NA_real_)
}

.qlr_try_obj <- function(expr) {
  tryCatch(expr, error = function(e) NULL)
}

.qlr_try_bool <- function(expr) {
  tryCatch(expr, error = function(e) FALSE)
}

# ------------------------------------------------------------
# 1. Option wrappers
# ------------------------------------------------------------

qlr_option_npv <- function(opt) {
  qlr_safe_npv(opt)
}

qlr_option_price <- function(opt) {
  qlr_safe_npv(opt)
}

# ------------------------------------------------------------
# 2. Curve wrappers
# ------------------------------------------------------------

qlr_discount <- function(curve, t) {
  .qlr_try_num(curve$discount(t))
}

qlr_zero_rate <- function(curve, t) {
  df <- qlr_discount(curve, t)
  if (!is.numeric(t) || length(t) != 1 || is.na(t) || t <= 0 || is.na(df) || df <= 0) {
    return(NA_real_)
  }
  -log(df) / t
}

qlr_curve_dates <- function(curve, n = 200) {
  curve_tbl(curve, n)
}

# ------------------------------------------------------------
# 3. Quote wrappers
# ------------------------------------------------------------

qlr_quote_set <- function(quote, value) {
  .qlr_try_bool(quote$setValue(value))
}

qlr_quote_get <- function(quote) {
  .qlr_try_num(quote$value())
}

# ------------------------------------------------------------
# 4. Leg / Cashflow wrappers
# ------------------------------------------------------------

qlr_leg_size <- function(leg) {
  leg$size()
}

qlr_leg_get <- function(leg, i) {
  leg_cashflow_at(leg, i)
}

qlr_cf_date <- function(cf) {
  qlr_iso(CashFlow_date(cf))
}

qlr_cf_amount <- function(cf) {
  .qlr_try_num(CashFlow_amount(cf))
}

qlr_cf_rate <- function(cf) {
  cpn <- .qlr_try_obj(as_coupon(cf))
  if (is.null(cpn)) {
    return(NA_real_)
  }
  .qlr_try_num(cpn$rate())
}

qlr_leg_table <- function(leg, curve = NULL) {
  qlr_cashflow_leg_tbl(leg, curve)
}

# ------------------------------------------------------------
# 5. Swap wrappers
# ------------------------------------------------------------

qlr_swap_npv <- function(swap) {
  qlr_safe_npv(swap)
}

qlr_swap_fair_rate <- function(swap) {
  .qlr_try_num(swap$fairRate())
}

qlr_swap_fixed_leg <- function(swap) {
  swap$fixedLeg()
}

qlr_swap_float_leg <- function(swap) {
  swap$floatingLeg()
}

qlr_swap_fixed_leg_table <- function(swap, curve = NULL) {
 qlr_cashflow_leg_tbl(swap$fixedLeg(), curve)
}

qlr_swap_float_leg_table <- function(swap, curve = NULL) {
 qlr_cashflow_leg_tbl(swap$floatingLeg(), curve)
}

# ------------------------------------------------------------
# 6. Engine wrappers
# ------------------------------------------------------------

qlr_make_analytic_european <- function(process) {
  .qlr_try_obj(AnalyticEuropeanEngine(process))
}

qlr_make_fd_engine <- function(process, tsteps = 200, xsteps = 200) {
  .qlr_try_obj(FdBlackScholesVanillaEngine(process, tsteps, xsteps))
}

qlr_make_baw_engine <- function(process) {
  .qlr_try_obj(BaroneAdesiWhaleyApproximationEngine(process))
}

# ------------------------------------------------------------
# 7. Process wrappers
# ------------------------------------------------------------

qlr_make_bsm_process <- function(spot, div, rf, vol) {
  .qlr_try_obj(BlackScholesMertonProcess(spot, div, rf, vol))
}

qlr_make_fx_process <- function(spot, foreign, domestic, vol) {
  .qlr_try_obj(BlackScholesMertonProcess(spot, foreign, domestic, vol))
}

# ------------------------------------------------------------
# 8. Schedule wrappers
# ------------------------------------------------------------

qlr_schedule_dates <- function(schedule) {
  tibble::tibble(
    schedule_date = purrr::map_chr(
      seq_len(schedule$size()),
      ~ qlr_iso(schedule$date(as.integer(.x - 1)))
    )
  )
}

# ------------------------------------------------------------
# 9. Model wrappers (Hull-White / Vasicek / CIR)
# ------------------------------------------------------------

qlr_make_hw <- function(curve_handle, a = 0.03, sigma = 0.01) {
  .qlr_try_obj(HullWhite(curve_handle, a, sigma))
}

qlr_make_vasicek <- function(a = 0.03, b = 0.15, theta = 0.03, sigma = 0.01) {
  .qlr_try_obj(Vasicek(a, b, theta, sigma))
}

qlr_make_cir <- function(a = 0.03, b = 0.15, theta = 0.03, sigma = 0.02) {
  .qlr_try_obj(CIR(a, b, theta, sigma))
}

qlr_discount_bond <- function(model, t0, t1, r = 0.03) {
  if (is.null(model)) {
    return(NA_real_)
  }

  out <- .qlr_try_obj(model$discountBond(t0, t1, r))
  if (!is.null(out)) {
    return(out)
  }

  out <- .qlr_try_obj(model$discountBond(t1, r))
  if (!is.null(out)) {
    return(out)
  }

  NA_real_
}

# ------------------------------------------------------------
# 10. FX wrappers
# ------------------------------------------------------------

qlr_fx_forward <- function(spot, df_domestic, df_foreign) {
  spot * df_domestic / df_foreign
}

qlr_fx_forward_points <- function(spot, df_domestic, df_foreign) {
  qlr_fx_forward(spot, df_domestic, df_foreign) - spot
}

# ------------------------------------------------------------
# 11. Date-aware curve helpers
# additive version:
# - qlr_discount / qlr_zero_rate keep numeric-time behavior
# - qlr_discount_date / qlr_zero_rate_date accept QuantLib dates
# ------------------------------------------------------------

qlr_curve_time <- function(curve, x) {
  if (is.numeric(x) && length(x) == 1 && !is.na(x)) {
    return(as.numeric(x))
  }

  ref_date <- curve$referenceDate()
  dc <- curve$dayCounter()

  tt <- tryCatch(
    dc$yearFraction(ref_date, x),
    error = function(e) NA_real_
  )

  if (is.na(tt)) {
    stop("Unsupported time/date input")
  }

  tt
}

qlr_discount_date <- function(curve, x) {
  if (is.numeric(x) && length(x) == 1 && !is.na(x)) {
    return(.qlr_try_num(curve$discount(as.numeric(x))))
  }

  out <- .qlr_try_num(curve$discount(x))
  if (!is.na(out)) {
    return(out)
  }

  tt <- qlr_curve_time(curve, x)
  .qlr_try_num(curve$discount(tt))
}

qlr_zero_rate_date <- function(curve, x) {
  tt <- qlr_curve_time(curve, x)
  df <- qlr_discount_date(curve, x)

  if (is.na(tt) || tt <= 0 || is.na(df) || df <= 0) {
    return(NA_real_)
  }

  -log(df) / tt
}

qlr_forward_rate_date <- function(curve, d1, d2) {
  dc <- curve$dayCounter()

  out <- tryCatch(
    curve$forwardRate(d1, d2, dc, "Simple")$rate(),
    error = function(e) NA_real_
  )

  if (!is.na(out)) {
    return(out)
  }

  t1 <- qlr_curve_time(curve, d1)
  t2 <- qlr_curve_time(curve, d2)

  if (is.na(t1) || is.na(t2) || t2 <= t1) {
    return(NA_real_)
  }

  df1 <- qlr_discount_date(curve, d1)
  df2 <- qlr_discount_date(curve, d2)

  if (is.na(df1) || is.na(df2) || df1 <= 0 || df2 <= 0) {
    return(NA_real_)
  }

  yf <- tryCatch(
    dc$yearFraction(d1, d2),
    error = function(e) t2 - t1
  )

  if (is.na(yf) || yf <= 0) {
    return(NA_real_)
  }

  (df1 / df2 - 1) / yf
}
