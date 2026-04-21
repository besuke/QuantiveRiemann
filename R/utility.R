# ============================================================
# QuantiveRiemann - utility.R
# Common utilities (Date, Curve, Cashflow, Safe wrappers)
# ============================================================

.qlr_ir_msg <- function(verbose, ...) {
  if (isTRUE(verbose)) {
    message(...)
  }
}
# ------------------------------------------------------------
# 1. Date helpers
# ------------------------------------------------------------

qlr_date <- function(x) {
  if (inherits(x, "POSIXt")) {
    x <- format(as.Date(x), "%Y-%m-%d")
  }
  if (inherits(x, "Date")) {
    x <- format(x, "%Y-%m-%d")
  }
  if (is.character(x) && length(x) == 1L) {
    return(DateParser_parseISO(x))
  }
  x
}
qlr_chr <- function(x) {
  tryCatch(
    x$`__str__`(),
    error = function(e1) {
      tryCatch(
        as.character(x),
        error = function(e2) "<unprintable>"
      )
    }
  )
}
qlr_iso <- function(x) {
  tryCatch(Date_ISO(x), error = function(e) as.character(x))
}

qlr_set_eval_date <- function(eval_date) {
  Settings_instance()$setEvaluationDate(qlr_date(eval_date))
}

qlr_advance_days <- function(calendar_obj, date_obj, n_days) {
  Calendar_advance(calendar_obj, qlr_date(date_obj), as.integer(n_days), "Days")
}

qlr_period <- function(x) {
  if (length(x) != 1 || is.na(x)) {
    stop("tenor must be a single non-NA string")
  }

  x <- trimws(toupper(as.character(x)))

  m <- regexec("^([0-9]+)\\s*([DWMY])$", x)
  hit <- regmatches(x, m)[[1]]

  if (length(hit) == 0) {
    stop(
      "Unsupported tenor format: ", x,
      ". Use forms like 1D, 1W, 3M, 18M, 2Y."
    )
  }

  n <- as.integer(hit[2])
  u <- hit[3]

  switch(
    u,
    "D" = qlr_period_days(n),
    "W" = qlr_period_weeks(n),
    "M" = qlr_period_months(n),
    "Y" = qlr_period_years(n),
    stop("Unsupported tenor unit: ", u)
  )
}

qlr_period_days <- function(n) {
  Period(as.integer(n), "Days")
}

qlr_period_weeks <- function(n) {
  Period(as.integer(n), "Weeks")
}
qlr_period_months <- function(n) {
  Period(as.integer(n), "Months")
}

qlr_period_years <- function(n) {
  Period(as.integer(n), "Years")
}

# ------------------------------------------------------------
# 2. Safe wrappers for SWIG differences
# ------------------------------------------------------------

qlr_safe_npv <- function(obj) {
  tryCatch(obj$NPV(), error = function(e) NA_real_)
}

qlr_safe_engine_set <- function(obj, engine) {
  tryCatch({
    Instrument_setPricingEngine(obj, engine)
    TRUE
  }, error = function(e) FALSE)
}

qlr_safe_greek <- function(obj, greek_name) {
  fn <- tryCatch(obj[[greek_name]], error = function(e) NULL)
  if (is.null(fn)) {
    return(NA_real_)
  }
  tryCatch(fn(), error = function(e) NA_real_)
}

# ------------------------------------------------------------
# 3. Leg / Cashflow helpers
# ------------------------------------------------------------

qlr_leg_cashflow_at <- function(leg_obj, i_one_based) {
  idx0 <- as.integer(i_one_based - 1)

  tryCatch(
    leg_obj$get(idx0),
    error = function(e1) {
      tryCatch(
        Leg___getitem__(leg_obj, idx0),
        error = function(e2) {
          tryCatch(leg_obj[[i_one_based]][[1]], error = function(e3) NULL)
        }
      )
    }
  )
}

qlr_cashflow_leg_tbl <- function(leg_obj, curve_obj = NULL) {
  purrr::map_dfr(seq_len(leg_obj$size()), function(i) {
    cf_obj <- qlr_leg_cashflow_at(leg_obj, i)

    pay_date_obj <- CashFlow_date(cf_obj)
    pay_date_chr <- qlr_iso(pay_date_obj)

    amount_value <- tryCatch(CashFlow_amount(cf_obj), error = function(e) NA_real_)

    df_value <- if (!is.null(curve_obj)) {
      tryCatch(curve_obj$discount(pay_date_obj), error = function(e) NA_real_)
    } else {
      NA_real_
    }

    coupon_obj <- tryCatch(as_coupon(cf_obj), error = function(e) NULL)

    tibble::tibble(
      pay_date = pay_date_chr,
      accrual_start = if (!is.null(coupon_obj)) qlr_iso(Coupon_accrualStartDate(coupon_obj)) else NA_character_,
      accrual_end = if (!is.null(coupon_obj)) qlr_iso(Coupon_accrualEndDate(coupon_obj)) else NA_character_,
      amount = amount_value,
      rate = if (!is.null(coupon_obj)) tryCatch(coupon_obj$rate(), error = function(e) NA_real_) else NA_real_,
      df = df_value,
      pv = amount_value * df_value
    )
  })
}

# ------------------------------------------------------------
# 4. Curve helpers
# ------------------------------------------------------------

qlr_curve_tbl <- function(curve_obj, n = 200, extrapolate = TRUE) {
  if (isTRUE(extrapolate)) {
    TermStructure_enableExtrapolation(curve_obj)
  }

  ref_date_r <- as.Date(qlr_iso(curve_obj$referenceDate()))
  max_t <- curve_obj$maxTime()
  times <- seq(0, max_t, length.out = n)

  tibble::tibble(time = times) %>%
    dplyr::mutate(
      discount = purrr::map_dbl(time, ~ curve_obj$discount(.x)),
      zero = dplyr::if_else(time > 0, -log(discount) / time, 0),
      curve_date = ref_date_r + round(time * 365)
    )
}

# ------------------------------------------------------------
# 5. Display helpers
# ------------------------------------------------------------

qlr_show_tbl <- function(tbl, title = NULL, n = 10) {
  if (!is.null(title)) {
    cat("\n", strrep("=", 72), "\n", title, "\n", strrep("=", 72), "\n", sep = "")
  }
  print(dplyr::slice_head(tbl, n = n))
  invisible(tbl)
}

qlr_fmt_num <- function(x, digits = 6) {
  sprintf(paste0("%.", digits, "f"), x)
}
