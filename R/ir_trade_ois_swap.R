# ============================================================
# QuantiveRiemann - ir_trade_ois_swap.R
# OIS trade builders
# ============================================================

.qlr_ir_get_ois_trade_convention <- function(currency, instrument) {
  key <- paste0(currency, "::", instrument)

  if (identical(key, "JPY::TONA")) {
    return(list(
      calendar = Japan(),
      fixed_day_counter = Actual365Fixed(),
      payment_lag = 2L,
      fixed_leg_tenor = qlr_period_years(1),
      overnight_leg_tenor = qlr_period_years(1),
      index_builder = function(curve_handle) {
        OvernightIndex(
          "TONA",
          0,
          JPYCurrency(),
          Japan(),
          Actual365Fixed(),
          curve_handle
        )
      }
    ))
  }

  if (identical(key, "USD::SOFR")) {
    sofr_cal <- Sofr()$fixingCalendar()
    return(list(
      calendar = sofr_cal,
      fixed_day_counter = Actual360(),
      payment_lag = 2L,
      fixed_leg_tenor = qlr_period_years(1),
      overnight_leg_tenor = qlr_period_years(1),
      index_builder = function(curve_handle) {
        Sofr(curve_handle)
      }
    ))
  }

  stop("Unsupported OIS trade convention for ", key)
}

qlr_ois_daily_forward_table <- function(
    swap,
    curve,
    index,
    eval_date = NULL,
    calendar = NULL,
    day_count = NULL,
    notional = 1e7
) {
  if (is.null(eval_date)) {
    eval_date <- Settings_instance()$evaluationDate()
  } else {
    eval_date <- qlr_date(eval_date)
  }

  if (is.null(calendar)) {
    calendar <- index$fixingCalendar()
  }

  if (is.null(day_count)) {
    day_count <- index$dayCounter()
  }

  leg_obj <- swap$leg(1)
  cf_obj <- leg_cashflow_at(leg_obj, 1)
  cpn_obj <- as_floating_rate_coupon(cf_obj)

  accrual_start <- Coupon_accrualStartDate(cpn_obj)
  accrual_end   <- Coupon_accrualEndDate(cpn_obj)

  accrual_start_chr <- qlr_iso(accrual_start)
  accrual_end_chr   <- qlr_iso(accrual_end)
  eval_date_r <- as.Date(qlr_iso(eval_date))

  business_dates_tbl <- make_business_dates_tbl(
    start_date = accrual_start_chr,
    end_date   = accrual_end_chr,
    calendar_obj = calendar
  )

  out <- business_dates_tbl |>
    dplyr::mutate(
      next_date = dplyr::lead(accrual_date),
      fixing_date = accrual_date
    ) |>
    dplyr::filter(!is.na(next_date)) |>
    dplyr::mutate(
      fixing_date_r = as.Date(fixing_date),
      next_date_r   = as.Date(next_date),
      days = as.integer(next_date_r - fixing_date_r),
      fixing_before_eval = fixing_date_r < eval_date_r,
      fixing_on_eval     = fixing_date_r == eval_date_r,
      fixing_value = purrr::map_dbl(
        fixing_date,
        ~ tryCatch(index$fixing(qlr_date(.x)), error = function(e) NA_real_)
      ),
      df_start = purrr::map_dbl(
        fixing_date,
        ~ curve$discount(qlr_date(.x))
      ),
      df_end = purrr::map_dbl(
        next_date,
        ~ curve$discount(qlr_date(.x))
      ),
      forward_rate_from_df = (df_start / df_end - 1) * 365 / days,
      applied_rate = dplyr::case_when(
        fixing_before_eval ~ fixing_value,
        fixing_on_eval ~ dplyr::coalesce(fixing_value, forward_rate_from_df),
        TRUE ~ forward_rate_from_df
      ),
      amount = notional * applied_rate * days / 365
    ) |>
    dplyr::select(
      fixing_date,
      next_date,
      days,
      fixing_before_eval,
      fixing_on_eval,
      fixing_value,
      forward_rate_from_df,
      applied_rate,
      amount
    )

  out
}
.qlr_ir_make_ois_schedule <- function(
    effective,
    maturity,
    calendar,
    bdc = "ModifiedFollowing",
    date_rule = "Backward",
    eom = FALSE
) {
  Schedule(
    qlr_date(effective),
    qlr_date(maturity),
    qlr_period_years(1),
    calendar,
    bdc,
    bdc,
    date_rule,
    eom
  )
}

.qlr_ir_make_overnight_schedule <- function(
    effective,
    maturity,
    calendar,
    bdc = "ModifiedFollowing",
    date_rule = "Backward",
    eom = FALSE
) {
  Schedule(
    qlr_date(effective),
    qlr_date(maturity),
    qlr_period_years(1),
    calendar,
    bdc,
    bdc,
    date_rule,
    eom
  )
}

.qlr_ir_try_make_ois_trade <- function(
    swap_type,
    nominal,
    fixed_schedule,
    fixed_rate,
    fixed_day_counter,
    overnight_schedule,
    index,
    spread,
    payment_lag
) {
  # SWIG build ごとの差分を吸収するために、可能性の高いシグネチャを順に試す
  out <- tryCatch(
    OvernightIndexedSwap(
      swap_type,
      nominal,
      fixed_schedule,
      fixed_rate,
      fixed_day_counter,
      overnight_schedule,
      index,
      spread,
      payment_lag
    ),
    error = function(e) NULL
  )
  if (!is.null(out)) {
    return(out)
  }

  out <- tryCatch(
    OvernightIndexedSwap(
      swap_type,
      nominal,
      fixed_schedule,
      fixed_rate,
      fixed_day_counter,
      overnight_schedule,
      index,
      spread
    ),
    error = function(e) NULL
  )
  if (!is.null(out)) {
    return(out)
  }

  out <- tryCatch(
    OvernightIndexedSwap(
      swap_type,
      nominal,
      fixed_schedule,
      fixed_rate,
      fixed_day_counter,
      index,
      spread
    ),
    error = function(e) NULL
  )
  if (!is.null(out)) {
    return(out)
  }

  NULL
}


qlr_trade_ois_swap <- function(
    curve_env,
    effective = NULL,
    maturity,
    nominal = 1e6,
    fixed_rate,
    float_spread = 0.0,
    swap_type = Swap_Payer_get(),
    fixed_bdc = "ModifiedFollowing",
    date_rule = "Backward",
    eom = FALSE,
    fixed_schedule_tenor = NULL,
    overnight_schedule_tenor = NULL,
    payment_lag = NULL,
    verbose = TRUE
) {
  if (is.null(curve_env$curve_handle)) {
    stop("curve_env must contain $curve_handle")
  }

  if (is.null(curve_env$currency) || is.null(curve_env$instrument)) {
    stop("curve_env must contain $currency and $instrument")
  }

  if (is.null(effective)) {
    if (is.null(curve_env$settle_date)) {
      stop("effective is NULL and curve_env does not contain $settle_date")
    }
    effective <- curve_env$settle_date
  }

  conv <- .qlr_ir_get_ois_trade_convention(
    currency = curve_env$currency,
    instrument = curve_env$instrument
  )

  fixed_schedule_tenor_obj <- if (is.null(fixed_schedule_tenor)) {
    conv$fixed_leg_tenor
  } else {
    if (inherits(fixed_schedule_tenor, "Period")) {
      fixed_schedule_tenor
    } else {
      qlr_period(fixed_schedule_tenor)
    }
  }

  overnight_schedule_tenor_obj <- if (is.null(overnight_schedule_tenor)) {
    conv$overnight_leg_tenor
  } else {
    if (inherits(overnight_schedule_tenor, "Period")) {
      overnight_schedule_tenor
    } else {
      qlr_period(overnight_schedule_tenor)
    }
  }

  payment_lag_use <- if (is.null(payment_lag)) {
    conv$payment_lag
  } else {
    as.integer(payment_lag)
  }

  .qlr_ir_msg(
    verbose,
    "[qlr_trade_ois_swap] start: ",
    curve_env$currency, "::", curve_env$instrument,
    " ", as.character(effective), " -> ", as.character(maturity),
    " | fixed_tenor=", qlr_chr(fixed_schedule_tenor_obj),
    " | overnight_tenor=", qlr_chr(overnight_schedule_tenor_obj),
    " | payment_lag=", payment_lag_use
  )

  fixed_schedule <- Schedule(
    qlr_date(effective),
    qlr_date(maturity),
    fixed_schedule_tenor_obj,
    conv$calendar,
    fixed_bdc,
    fixed_bdc,
    date_rule,
    eom
  )

  overnight_schedule <- Schedule(
    qlr_date(effective),
    qlr_date(maturity),
    overnight_schedule_tenor_obj,
    conv$calendar,
    fixed_bdc,
    fixed_bdc,
    date_rule,
    eom
  )

  index_obj <- conv$index_builder(curve_env$curve_handle)

  swap_obj <- .qlr_ir_try_make_ois_trade(
    swap_type = swap_type,
    nominal = nominal,
    fixed_schedule = fixed_schedule,
    fixed_rate = fixed_rate,
    fixed_day_counter = conv$fixed_day_counter,
    overnight_schedule = overnight_schedule,
    index = index_obj,
    spread = float_spread,
    payment_lag = payment_lag_use
  )

  if (is.null(swap_obj)) {
    stop(
      "Could not construct OvernightIndexedSwap with this SWIG QuantLib build. ",
      "Your build may expose a different constructor signature. ",
      "Please inspect available OIS constructors in your QuantLib binding."
    )
  }

  engine <- DiscountingSwapEngine(curve_env$curve_handle)
  qlr_safe_engine_set(swap_obj, engine)

  out <- list(
    trade_type = "OIS_SWAP",
    currency = curve_env$currency,
    instrument = curve_env$instrument,
    effective = as.character(as.Date(effective)),
    maturity = as.character(as.Date(maturity)),
    nominal = nominal,
    fixed_rate = fixed_rate,
    float_spread = float_spread,
    payment_lag = payment_lag_use,
    fixed_schedule_tenor = fixed_schedule_tenor_obj,
    overnight_schedule_tenor = overnight_schedule_tenor_obj,
    swap = swap_obj,
    index = index_obj,
    fixed_schedule = fixed_schedule,
    overnight_schedule = overnight_schedule,
    curve_env = curve_env
  )

  .qlr_ir_msg(verbose, "[qlr_trade_ois_swap] done")
  out
}

qlr_trade_ois_swap_py <- function(
    curve_env,
    effective = NULL,
    maturity,
    notional = 1e6,
    fixed_rate,
    float_spread = 0,
    pay_receive = c("pay", "receive"),
    verbose = TRUE,
    ...
) {
  pay_receive <- match.arg(pay_receive)

  swap_type <- if (identical(pay_receive, "pay")) {
    Swap_Payer_get()
  } else {
    Swap_Receiver_get()
  }

  qlr_trade_ois_swap(
    curve_env = curve_env,
    effective = effective,
    maturity = maturity,
    nominal = notional,
    fixed_rate = fixed_rate,
    float_spread = float_spread,
    swap_type = swap_type,
    verbose = verbose,
    ...
  )
}
qlr_trade_ois_swap_py <- function(
    curve_env,
    effective = NULL,
    maturity,
    notional = 1e6,
    fixed_rate,
    float_spread = 0.0,
    pay_receive = c("pay", "receive"),
    fixed_schedule_tenor = NULL,
    floating_schedule_tenor = NULL,
    pay_lag = NULL,
    fixed_bdc = "ModifiedFollowing",
    date_rule = "Backward",
    eom = FALSE,
    verbose = TRUE
) {
  pay_receive <- match.arg(pay_receive)

  swap_type <- if (identical(pay_receive, "pay")) {
    Swap_Payer_get()
  } else {
    Swap_Receiver_get()
  }

  qlr_trade_ois_swap(
    curve_env = curve_env,
    effective = effective,
    maturity = maturity,
    nominal = notional,
    fixed_rate = fixed_rate,
    float_spread = float_spread,
    swap_type = swap_type,
    fixed_bdc = fixed_bdc,
    date_rule = date_rule,
    eom = eom,
    fixed_schedule_tenor = fixed_schedule_tenor,
    overnight_schedule_tenor = floating_schedule_tenor,
    payment_lag = pay_lag,
    verbose = verbose
  )
}
