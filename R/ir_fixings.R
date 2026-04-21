
# ============================================================
# QuantiveRiemann - ir_fixings.R
# Interest-rate fixing helpers
# ============================================================

.qlr_ir_apply_fixings_to_index <- function(index_obj, fixings_tbl, verbose = TRUE) {
  if (!is.data.frame(fixings_tbl)) {
    stop("fixings_tbl must be a data.frame or tibble")
  }

  required_cols <- c("date", "fixing")
  missing_cols <- setdiff(required_cols, names(fixings_tbl))

  if (length(missing_cols) > 0) {
    stop("fixings_tbl is missing columns: ", paste(missing_cols, collapse = ", "))
  }

  fixings_tbl2 <- fixings_tbl |>
    dplyr::transmute(
      date = as.Date(date),
      fixing = as.numeric(fixing)
    ) |>
    dplyr::arrange(date)

  if (any(is.na(fixings_tbl2$date))) {
    stop("fixings_tbl contains invalid or missing dates")
  }

  if (any(is.na(fixings_tbl2$fixing))) {
    stop("fixings_tbl contains invalid or missing fixing values")
  }

  purrr::pwalk(
    fixings_tbl2,
    function(date, fixing) {
      tryCatch(
        index_obj$addFixing(qlr_date(date), fixing, TRUE),
        error = function(e) NULL
      )
    }
  )

  .qlr_ir_msg(verbose, "[.qlr_ir_apply_fixings_to_index] applied = ", nrow(fixings_tbl2))

  invisible(index_obj)
}

qlr_ir_apply_fixings <- function(
    index_obj,
    fixings_tbl,
    currency,
    instrument,
    verbose = TRUE
) {
  if (!is.data.frame(fixings_tbl)) {
    stop("fixings_tbl must be a data.frame or tibble")
  }

  required_cols <- c("date", "currency", "instrument", "fixing")
  missing_cols <- setdiff(required_cols, names(fixings_tbl))

  if (length(missing_cols) > 0) {
    stop("fixings_tbl is missing columns: ", paste(missing_cols, collapse = ", "))
  }

  currency2 <- toupper(trimws(currency))
  instrument2 <- toupper(trimws(instrument))

  fx <- fixings_tbl |>
    dplyr::transmute(
      date = as.Date(date),
      currency = toupper(trimws(currency)),
      instrument = toupper(trimws(instrument)),
      fixing = as.numeric(fixing)
    ) |>
    dplyr::filter(
      currency == .env$currency2,
      instrument == .env$instrument2
    ) |>
    dplyr::arrange(date)

  .qlr_ir_msg(
    verbose,
    "[qlr_ir_apply_fixings] ",
    currency2, "::", instrument2,
    " fixings = ", nrow(fx)
  )

  if (nrow(fx) == 0) {
    warning("No fixings found for ", currency2, "::", instrument2)
    return(invisible(index_obj))
  }

  .qlr_ir_apply_fixings_to_index(
    index_obj = index_obj,
    fixings_tbl = fx |>
      dplyr::select(date, fixing),
    verbose = verbose
  )
}

# ============================================================
# QuantiveRiemann - ir_fixings.R
# Coupon fixing diagnostics
# ============================================================

qlr_ir_coupon_fixing_status_table <- function(
    swap_obj,
    curve_obj,
    index_obj,
    leg = 1,
    eval_date = NULL
) {
  if (is.null(eval_date)) {
    eval_date <- Settings_instance()$evaluationDate()
  } else {
    eval_date <- qlr_date(eval_date)
  }

  target_leg <- swap_obj$leg(as.integer(leg))

  purrr::map_dfr(seq_len(target_leg$size()), function(i) {
    cashflow_obj <- qlr_leg_cashflow_at(target_leg, i)
    coupon_obj <- as_floating_rate_coupon(cashflow_obj)

    fixing_date_obj <- FloatingRateCoupon_fixingDate(coupon_obj)
    fixing_date_chr <- qlr_iso(fixing_date_obj)
    fixing_date_r <- as.Date(fixing_date_chr)
    eval_date_r <- as.Date(qlr_iso(eval_date))

    accrual_start_obj <- Coupon_accrualStartDate(coupon_obj)
    accrual_end_obj <- Coupon_accrualEndDate(coupon_obj)
    pay_date_obj <- CashFlow_date(cashflow_obj)

    pay_date_chr <- qlr_iso(pay_date_obj)
    pay_date_r <- as.Date(pay_date_chr)

    ref_date_r <- as.Date(qlr_iso(curve_obj$referenceDate()))
    t <- as.numeric(pay_date_r - ref_date_r) / 365

    df <- if (!is.na(t) && t >= 0) {
      tryCatch(curve_obj$discount(pay_date_obj), error = function(e) NA_real_)
    } else {
      NA_real_
    }

    amount_value <- tryCatch(CashFlow_amount(cashflow_obj), error = function(e) NA_real_)
    coupon_rate <- tryCatch(coupon_obj$rate(), error = function(e) NA_real_)
    coupon_spread <- tryCatch(FloatingRateCoupon_spread(coupon_obj), error = function(e) NA_real_)

    fixing_value <- tryCatch(
      index_obj$fixing(fixing_date_obj),
      error = function(e) NA_real_
    )

    tibble::tibble(
      leg_no = as.integer(leg),
      cashflow_no = i,
      fixing_date = fixing_date_chr,
      fixing_before_eval = fixing_date_r < eval_date_r,
      fixing_on_eval = fixing_date_r == eval_date_r,
      accrual_start = qlr_iso(accrual_start_obj),
      accrual_end = qlr_iso(accrual_end_obj),
      pay_date = pay_date_chr,
      amount = amount_value,
      coupon_rate = coupon_rate,
      spread = coupon_spread,
      fixing_value = fixing_value,
      implied_margin = coupon_rate - fixing_value,
      df = df,
      pv = amount_value * df,
      fixing_source = dplyr::case_when(
        fixing_date_r < eval_date_r ~ "historical fixing expected",
        fixing_date_r == eval_date_r ~ "today fixing boundary",
        TRUE ~ "forward projection expected"
      )
    )
  })
}
