## ============================================================
# QuantiveRiemann - curve_wrappers.R
# Curve-related QuantLib SWIG API wrappers
# ============================================================

# ------------------------------------------------------------
# internal helpers
# ------------------------------------------------------------

.qlr_try_num <- function(expr) {
  tryCatch(expr, error = function(e) NA_real_)
}

# ------------------------------------------------------------
# 1. Basic curve wrappers
# ------------------------------------------------------------

qlr_discount <- function(curve, t) {
  .qlr_try_num(curve$discount(t))
}

qlr_zero_rate <- function(curve, t) {
  df <- qlr_discount(curve, t)
  
  if (!is.numeric(t) || length(t) != 1L || is.na(t) || t <= 0 || is.na(df) || df <= 0) {
    return(NA_real_)
  }
  
  -log(df) / t
}

qlr_curve_dates <- function(curve, n = 200) {
  qlr_curve_tbl(curve, n = n)
}

# ------------------------------------------------------------
# 2. Date-aware curve helpers
# ------------------------------------------------------------

qlr_curve_time <- function(curve, x) {
  if (is.numeric(x) && length(x) == 1L && !is.na(x)) {
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
  if (is.numeric(x) && length(x) == 1L && !is.na(x)) {
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