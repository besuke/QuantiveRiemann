# ============================================================
# QuantiveRiemann - bond.R
# Bond analytics
# - theoretical bond formulas
# - QuantLib object-based bond analytics
# Depends on:
#   utility.R
#   wrapper_lib.R
# ============================================================

# ------------------------------------------------------------
# internal helpers
# ------------------------------------------------------------

.qlr_bond_cashflows <- function(
    face_amount = 100,
    coupon_rate = 0.01,
    frequency = 2,
    maturity_years = 5
) {
  times <- seq(1 / frequency, maturity_years, by = 1 / frequency)

  cashflows <- rep(
    face_amount * coupon_rate / frequency,
    length(times)
  )

  cashflows[length(cashflows)] <- cashflows[length(cashflows)] + face_amount

  list(
    times = times,
    cashflows = cashflows
  )
}

.qlr_bond_metric_tbl <- function(metric, value) {
  tibble::tibble(
    metric = metric,
    value = value
  )
}

.qlr_bond_extract_scalar <- function(x) {
  if (length(x) == 1) {
    return(as.numeric(x))
  }

  as.numeric(x[[1]])
}

.qlr_bond_clean_price <- function(x) {
  out <- tryCatch(
    BondPrice(x, "Clean"),
    error = function(e) NULL
  )

  if (!is.null(out)) {
    return(out)
  }

  x
}

.qlr_bond_quote_handle <- function(x) {
  QuoteHandle(SimpleQuote(x))
}

.qlr_bond_prev_coupon_date <- function(schedule, settlement_date) {
  tryCatch(
    schedule$previousDate(settlement_date),
    error = function(e) NULL
  )
}

.qlr_bond_duration_call <- function(bond, interest_rate_obj, duration_type) {
  out <- tryCatch(
    BondFunctions_duration(bond, interest_rate_obj, duration_type),
    error = function(e) NULL
  )

  if (!is.null(out)) {
    return(out)
  }

  tryCatch(
    BondFunctions$duration(bond, interest_rate_obj, duration_type),
    error = function(e) NA_real_
  )
}

.qlr_bond_bpv_call <- function(bond, interest_rate_obj) {
  out <- tryCatch(
    BondFunctions_basisPointValue(bond, interest_rate_obj),
    error = function(e) NULL
  )

  if (!is.null(out)) {
    return(out)
  }

  tryCatch(
    BondFunctions$basisPointValue(bond, interest_rate_obj),
    error = function(e) NA_real_
  )
}

.qlr_bond_convexity_call <- function(bond, interest_rate_obj) {
  out <- tryCatch(
    BondFunctions_convexity(bond, interest_rate_obj),
    error = function(e) NULL
  )

  if (!is.null(out)) {
    return(out)
  }

  tryCatch(
    BondFunctions$convexity(bond, interest_rate_obj),
    error = function(e) NA_real_
  )
}

.qlr_bond_zspread_call <- function(
    bond,
    clean_price,
    curve,
    day_counter,
    compounding,
    frequency
) {
  out <- tryCatch(
    BondFunctions_zSpread(
      bond,
      .qlr_bond_clean_price(clean_price),
      curve,
      day_counter,
      compounding,
      frequency
    ),
    error = function(e) NULL
  )

  if (!is.null(out)) {
    return(out)
  }

  tryCatch(
    BondFunctions$zSpread(
      bond,
      .qlr_bond_clean_price(clean_price),
      curve,
      day_counter,
      compounding,
      frequency
    ),
    error = function(e) NA_real_
  )
}

.qlr_bond_duration_type_macaulay <- function() {
  out <- tryCatch(Duration_Macaulay, error = function(e) NULL)
  if (!is.null(out)) {
    return(out)
  }

  tryCatch(Duration$Macaulay, error = function(e) NULL)
}

.qlr_bond_duration_type_modified <- function() {
  out <- tryCatch(Duration_Modified, error = function(e) NULL)
  if (!is.null(out)) {
    return(out)
  }

  tryCatch(Duration$Modified, error = function(e) NULL)
}

.qlr_bond_cf_row <- function(cf) {
  coupon_obj <- tryCatch(as_coupon(cf), error = function(e) NULL)

  if (!is.null(coupon_obj)) {
    return(
      tibble::tibble(
        pay_date = qlr_iso(CashFlow_date(cf)),
        accrual_start = qlr_iso(Coupon_accrualStartDate(coupon_obj)),
        accrual_end = qlr_iso(Coupon_accrualEndDate(coupon_obj)),
        coupon_rate = tryCatch(coupon_obj$rate(), error = function(e) NA_real_),
        amount = tryCatch(CashFlow_amount(cf), error = function(e) NA_real_),
        cashflow_type = "coupon"
      )
    )
  }

  tibble::tibble(
    pay_date = qlr_iso(CashFlow_date(cf)),
    accrual_start = NA_character_,
    accrual_end = NA_character_,
    coupon_rate = NA_real_,
    amount = tryCatch(CashFlow_amount(cf), error = function(e) NA_real_),
    cashflow_type = "other"
  )
}

# ------------------------------------------------------------
# 1. theoretical zero-coupon bond
# ------------------------------------------------------------

#' Zero-coupon bond price
#' @export
qlr_bond_zero_price <- function(
    face_amount = 100,
    rate = 0.02,
    maturity_years = 5
) {
  face_amount / (1 + rate)^maturity_years
}

#' Zero-coupon bond yield
#' @export
qlr_bond_zero_yield <- function(
    price,
    face_amount = 100,
    maturity_years = 5
) {
  (face_amount / price)^(1 / maturity_years) - 1
}

# ------------------------------------------------------------
# 2. theoretical coupon bond pricing
# ------------------------------------------------------------

#' Coupon bond price from yield
#' @export
qlr_bond_coupon_price <- function(
    face_amount = 100,
    coupon_rate = 0.01,
    ytm = 0.02,
    frequency = 2,
    maturity_years = 5
) {
  cf_obj <- .qlr_bond_cashflows(
    face_amount = face_amount,
    coupon_rate = coupon_rate,
    frequency = frequency,
    maturity_years = maturity_years
  )

  times <- cf_obj$times
  cashflows <- cf_obj$cashflows

  sum(cashflows / (1 + ytm / frequency)^(frequency * times))
}

#' Macaulay duration for theoretical coupon bond
#' Convexity for theoretical coupon bond
#' @export
qlr_bond_convexity <- function(
    face_amount = 100,
    coupon_rate = 0.01,
    ytm = 0.02,
    frequency = 2,
    maturity_years = 5
) {
  cf_obj <- .qlr_bond_cashflows(
    face_amount = face_amount,
    coupon_rate = coupon_rate,
    frequency = frequency,
    maturity_years = maturity_years
  )

  times <- cf_obj$times
  cashflows <- cf_obj$cashflows

  pv <- cashflows / (1 + ytm / frequency)^(frequency * times)
  price <- sum(pv)

  sum(times^2 * pv / price)
}

#' DV01 for theoretical coupon bond
#' @export
qlr_bond_dv01 <- function(
    face_amount = 100,
    coupon_rate = 0.01,
    ytm = 0.02,
    frequency = 2,
    maturity_years = 5
) {
  price_0 <- qlr_bond_coupon_price(
    face_amount = face_amount,
    coupon_rate = coupon_rate,
    ytm = ytm,
    frequency = frequency,
    maturity_years = maturity_years
  )

  price_up <- qlr_bond_coupon_price(
    face_amount = face_amount,
    coupon_rate = coupon_rate,
    ytm = ytm + 0.0001,
    frequency = frequency,
    maturity_years = maturity_years
  )

  price_0 - price_up
}

#' Theoretical coupon bond cashflow table
#' @export
qlr_bond_cashflow_table <- function(
    face_amount = 100,
    coupon_rate = 0.01,
    frequency = 2,
    maturity_years = 5
) {
  cf_obj <- .qlr_bond_cashflows(
    face_amount = face_amount,
    coupon_rate = coupon_rate,
    frequency = frequency,
    maturity_years = maturity_years
  )

  tibble::tibble(
    period = seq_along(cf_obj$times),
    time = cf_obj$times,
    cashflow = cf_obj$cashflows
  )
}

#' Theoretical full bond analysis
#' @export
qlr_bond_analysis <- function(
    face_amount = 100,
    coupon_rate = 0.01,
    ytm = 0.02,
    frequency = 2,
    maturity_years = 5
) {
  price <- qlr_bond_coupon_price(
    face_amount = face_amount,
    coupon_rate = coupon_rate,
    ytm = ytm,
    frequency = frequency,
    maturity_years = maturity_years
  )

  duration <- qlr_bond_duration(
    face_amount = face_amount,
    coupon_rate = coupon_rate,
    ytm = ytm,
    frequency = frequency,
    maturity_years = maturity_years
  )

  modified_duration <- qlr_bond_modified_duration(
    face_amount = face_amount,
    coupon_rate = coupon_rate,
    ytm = ytm,
    frequency = frequency,
    maturity_years = maturity_years
  )

  convexity <- qlr_bond_convexity(
    face_amount = face_amount,
    coupon_rate = coupon_rate,
    ytm = ytm,
    frequency = frequency,
    maturity_years = maturity_years
  )

  dv01 <- qlr_bond_dv01(
    face_amount = face_amount,
    coupon_rate = coupon_rate,
    ytm = ytm,
    frequency = frequency,
    maturity_years = maturity_years
  )

  cashflows <- qlr_bond_cashflow_table(
    face_amount = face_amount,
    coupon_rate = coupon_rate,
    frequency = frequency,
    maturity_years = maturity_years
  )

  list(
    price = price,
    duration = duration,
    modified_duration = modified_duration,
    convexity = convexity,
    dv01 = dv01,
    cashflows = cashflows
  )
}

# ------------------------------------------------------------
# 3. curve table passthrough
# ------------------------------------------------------------

#' Yield curve table
#' @export
qlr_bond_yield_curve <- function(curve, n = 50) {
  qlr_curve_tbl(curve, n = n)
}

# ------------------------------------------------------------
# 4. QuantLib object-based fixed-rate bond builder
# ------------------------------------------------------------

#' Build a QuantLib fixed-rate bond and schedule
#' @export
qlr_fixed_rate_bond <- function(
    trade_date,
    settlement_days = 2,
    effective_date,
    maturity_date,
    face_amount = 100,
    coupon_rate = 0.01,
    calendar,
    schedule_frequency,
    accrual_day_counter,
    payment_convention,
    maturity_convention = payment_convention,
    date_generation,
    end_of_month = FALSE
) {
  trade_date <- qlr_date(trade_date)
  effective_date <- qlr_date(effective_date)
  maturity_date <- qlr_date(maturity_date)

  qlr_set_eval_date(trade_date)

  settlement_date <- Calendar_advance(
    calendar,
    trade_date,
    as.integer(settlement_days),
    "Days"
  )

  schedule <- Schedule(
    effective_date,
    maturity_date,
    schedule_frequency,
    calendar,
    payment_convention,
    maturity_convention,
    date_generation,
    end_of_month
  )

  bond <- FixedRateBond(
    as.integer(settlement_days),
    face_amount,
    schedule,
    c(coupon_rate),
    accrual_day_counter
  )

  list(
    trade_date = trade_date,
    settlement_date = settlement_date,
    schedule = schedule,
    bond = bond
  )
}

# ------------------------------------------------------------
# 5. price / yield
# ------------------------------------------------------------

#' Bond yield from clean price
#' @export
#' Bond yield from clean price
#' @export
qlr_bond_yield_from_clean_price <- function(
    bond,
    clean_price,
    day_counter,
    compounding,
    frequency
) {
  clean_price_arg <- .qlr_bond_clean_price(clean_price)

  out <- tryCatch(
    bond$bondYield(
      clean_price_arg,
      day_counter,
      compounding,
      frequency
    ),
    error = function(e) NULL
  )

  if (!is.null(out)) {
    return(out)
  }

  out <- tryCatch(
    BondFunctions_bondYield(
      bond,
      clean_price_arg,
      day_counter,
      compounding,
      frequency
    ),
    error = function(e) NULL
  )

  if (!is.null(out)) {
    return(out)
  }

  out <- tryCatch(
    bond$yield(
      clean_price_arg,
      day_counter,
      compounding,
      frequency
    ),
    error = function(e) NULL
  )

  if (!is.null(out)) {
    return(out)
  }

  stop("Could not compute bond yield in this SWIG build")
}
#' Bond price measures from yield
#' @export
qlr_bond_price_measures <- function(
    bond,
    yield,
    day_counter,
    compounding,
    frequency,
    settlement_date = NULL,
    schedule = NULL
) {
  accrued_amount <- tryCatch(
    QuantLibGauss::qlg_bond_accrued(bond),
    error = function(e) tryCatch(bond$accruedAmount(), error = function(e) NA_real_)
  )

  clean_price <- tryCatch(
    QuantLibGauss::qlg_bond_price_from_yield(
      bond = bond,
      ytm = yield,
      day_counter = day_counter,
      compounding = compounding,
      frequency = frequency
    ),
    error = function(e) tryCatch(
      bond$cleanPrice(yield, day_counter, compounding, frequency),
      error = function(e) NA_real_
    )
  )

  dirty_price <- tryCatch(
    bond$dirtyPrice(yield, day_counter, compounding, frequency),
    error = function(e) {
      if (is.na(clean_price) || is.na(accrued_amount)) {
        NA_real_
      } else {
        clean_price + accrued_amount
      }
    }
  )

  previous_coupon_date <- NULL
  accrued_days <- NA_real_

  if (!is.null(settlement_date) && !is.null(schedule)) {
    previous_coupon_date <- .qlr_bond_prev_coupon_date(schedule, settlement_date)

    if (!is.null(previous_coupon_date)) {
      accrued_days <- tryCatch(
        day_counter$dayCount(previous_coupon_date, settlement_date),
        error = function(e) NA_real_
      )
    }
  }

  tibble::tibble(
    metric = c(
      "settlement_date",
      "previous_coupon_date",
      "accrued_days",
      "accrued_amount",
      "clean_price",
      "dirty_price"
    ),
    value = list(
      if (!is.null(settlement_date)) as.Date(qlr_iso(settlement_date)) else NA,
      if (!is.null(previous_coupon_date)) as.Date(qlr_iso(previous_coupon_date)) else NA,
      accrued_days,
      accrued_amount,
      clean_price,
      dirty_price
    )
  )
}

# ------------------------------------------------------------
# 6. risk measures
# ------------------------------------------------------------

#' Bond risk measures from yield
#' @export
qlr_bond_risk_measures <- function(
    bond,
    yield,
    day_counter,
    compounding,
    frequency
) {
  interest_rate_obj <- InterestRate(
    yield,
    day_counter,
    compounding,
    frequency
  )

  macaulay_duration <- tryCatch(
    .qlr_bond_duration_call(
      bond = bond,
      interest_rate_obj = interest_rate_obj,
      duration_type = .qlr_bond_duration_type_macaulay()
    ),
    error = function(e) NA_real_
  )

  modified_duration <- tryCatch(
    QuantLibGauss::qlg_bond_duration(
      bond = bond,
      ytm = yield,
      day_counter = day_counter,
      compounding = compounding,
      frequency = frequency
    ),
    error = function(e) tryCatch(
      .qlr_bond_duration_call(
        bond = bond,
        interest_rate_obj = interest_rate_obj,
        duration_type = .qlr_bond_duration_type_modified()
      ),
      error = function(e) NA_real_
    )
  )

  bpv <- tryCatch(
    QuantLibGauss::qlg_bond_pv01(
      bond = bond,
      ytm = yield,
      day_counter = day_counter,
      compounding = compounding,
      frequency = frequency
    ),
    error = function(e) tryCatch(
      .qlr_bond_bpv_call(
        bond = bond,
        interest_rate_obj = interest_rate_obj
      ),
      error = function(e) NA_real_
    )
  )

  convexity <- tryCatch(
    QuantLibGauss::qlg_bond_convexity(
      bond = bond,
      ytm = yield,
      day_counter = day_counter,
      compounding = compounding,
      frequency = frequency
    ),
    error = function(e) tryCatch(
      .qlr_bond_convexity_call(
        bond = bond,
        interest_rate_obj = interest_rate_obj
      ),
      error = function(e) NA_real_
    )
  )

  tibble::tibble(
    metric = c(
      "macaulay_duration",
      "modified_duration",
      "bpv",
      "convexity"
    ),
    value = c(
      macaulay_duration,
      modified_duration,
      bpv,
      convexity
    )
  )
}

#' Bond hand-calculated risk checks
#' @export
qlr_bond_risk_handcalc <- function(
    bond,
    yield,
    dirty_price,
    modified_duration,
    convexity,
    day_counter,
    compounding,
    frequency
) {
  price_up_1bp <- tryCatch(
    bond$dirtyPrice(yield + 0.0001, day_counter, compounding, frequency),
    error = function(e) NA_real_
  )

  price_down_1bp <- tryCatch(
    bond$dirtyPrice(yield - 0.0001, day_counter, compounding, frequency),
    error = function(e) NA_real_
  )

  bpv_hand <- (price_up_1bp - price_down_1bp) / 2
  modified_duration_hand <- -bpv_hand * 100 / dirty_price

  convexity_hand <- (
    (price_up_1bp - dirty_price) -
      (dirty_price - price_down_1bp)
  ) * 10000 / dirty_price

  price_up_100bp <- tryCatch(
    bond$dirtyPrice(yield + 0.01, day_counter, compounding, frequency),
    error = function(e) NA_real_
  )

  delta_approx <- -modified_duration / 100 * dirty_price
  gamma_approx <- convexity / 10000 * dirty_price
  price_approx_delta_gamma <- dirty_price + delta_approx + 0.5 * gamma_approx

  tibble::tibble(
    metric = c(
      "bpv_hand",
      "modified_duration_hand",
      "convexity_hand",
      "price_up_100bp",
      "price_approx_delta_gamma"
    ),
    value = c(
      bpv_hand,
      modified_duration_hand,
      convexity_hand,
      price_up_100bp,
      price_approx_delta_gamma
    )
  )
}

# ------------------------------------------------------------
# 7. QuantLib cashflow tables
# ------------------------------------------------------------

#' Bond cashflow table from QuantLib bond
#' @export
#' Bond cashflow table from QuantLib bond
#' @export
#' Bond cashflow table from QuantLib bond
#' @export
qlr_bond_cashflow_table_ql <- function(
    bond,
    curve = NULL
) {
  cashflow_leg <- bond$cashflows()

  purrr::map_dfr(
    seq_len(cashflow_leg$size()),
    function(i) {
      cf <- qlr_leg_cashflow_at(cashflow_leg, i)
      coupon_obj <- tryCatch(as_coupon(cf), error = function(e) NULL)

      out <- tibble::tibble(
        pay_date = qlr_iso(CashFlow_date(cf)),
        coupon_rate = if (!is.null(coupon_obj)) {
          tryCatch(coupon_obj$rate(), error = function(e) NA_real_)
        } else {
          NA_real_
        },
        accrual_start = if (!is.null(coupon_obj)) {
          qlr_iso(Coupon_accrualStartDate(coupon_obj))
        } else {
          NA_character_
        },
        accrual_end = if (!is.null(coupon_obj)) {
          qlr_iso(Coupon_accrualEndDate(coupon_obj))
        } else {
          NA_character_
        },
        amount = tryCatch(CashFlow_amount(cf), error = function(e) NA_real_),
        cashflow_type = if (!is.null(coupon_obj)) "coupon" else "principal"
      )

      if (is.null(curve)) {
        return(out)
      }

      dplyr::mutate(
        out,
        discount_factor = qlr_discount_date(curve, qlr_date(pay_date)),
        pv = amount * discount_factor
      )
    }
  )
}

#' Bond cashflow table from QuantLib bond leg
#' @export
qlr_bond_leg_table <- function(
    bond,
    leg = 1,
    curve = NULL
) {
  leg_obj <- bond$leg(as.integer(leg))
  qlr_cashflow_leg_tbl(leg_obj, curve_obj = curve)
}

# ------------------------------------------------------------
# 8. flat forward bond pricing
# ------------------------------------------------------------

#' Flat forward term-structure handle
#' @export
qlr_bond_flat_forward_handle <- function(
    settlement_date,
    rate,
    day_counter,
    compounding,
    frequency
) {
  settlement_date <- qlr_date(settlement_date)

  curve_obj <- FlatForward(
    settlement_date,
    .qlr_bond_quote_handle(rate),
    day_counter,
    compounding,
    frequency
  )

  YieldTermStructureHandle(curve_obj)
}

#' Bond NPV with curve handle
#' @export
qlr_bond_npv_with_curve <- function(
    bond,
    curve_handle
) {
  engine <- DiscountingBondEngine(curve_handle)
  bond$setPricingEngine(engine)
  qlr_safe_npv(bond)
}

#' Bond NPV with flat yield
#' @export
qlr_bond_npv_with_flat_yield <- function(
    bond,
    settlement_date,
    rate,
    day_counter,
    compounding,
    frequency
) {
  curve_handle <- qlr_bond_flat_forward_handle(
    settlement_date = settlement_date,
    rate = rate,
    day_counter = day_counter,
    compounding = compounding,
    frequency = frequency
  )

  qlr_bond_npv_with_curve(
    bond = bond,
    curve_handle = curve_handle
  )
}

# ------------------------------------------------------------
# 9. z-spread
# ------------------------------------------------------------

#' Bond z-spread from clean price
#' @export
qlr_bond_zspread <- function(
    bond,
    clean_price,
    curve,
    day_counter,
    compounding,
    frequency
) {
  .qlr_bond_zspread_call(
    bond = bond,
    clean_price = clean_price,
    curve = curve,
    day_counter = day_counter,
    compounding = compounding,
    frequency = frequency
  )
}

#' Bond NPV with z-spreaded curve
#' @export
qlr_bond_npv_with_zspread <- function(
    bond,
    base_curve_handle,
    z_spread,
    compounding,
    frequency,
    day_counter
) {
  spread_curve <- ZeroSpreadedTermStructure(
    base_curve_handle,
    .qlr_bond_quote_handle(z_spread),
    compounding,
    frequency,
    day_counter
  )

  spread_curve_handle <- YieldTermStructureHandle(spread_curve)
  engine <- DiscountingBondEngine(spread_curve_handle)
  bond$setPricingEngine(engine)

  list(
    spread_curve = spread_curve,
    spread_curve_handle = spread_curve_handle,
    npv = qlr_safe_npv(bond)
  )
}

# ------------------------------------------------------------
# 10. asset swap
# ------------------------------------------------------------

#' Asset swap analysis for fixed-rate bond
#' @export
qlr_asset_swap_analysis <- function(
    bond,
    clean_price,
    ibor_index,
    spread,
    settlement_date,
    maturity_date,
    calendar,
    floating_schedule_frequency,
    payment_convention,
    date_generation,
    end_of_month = FALSE,
    floating_day_counter,
    pay_fixed_rate = TRUE,
    par_asset_swap = TRUE,
    discount_curve_handle
) {
  settlement_date <- qlr_date(settlement_date)
  maturity_date <- qlr_date(maturity_date)

  floating_schedule <- Schedule(
    settlement_date,
    maturity_date,
    floating_schedule_frequency,
    calendar,
    payment_convention,
    payment_convention,
    date_generation,
    end_of_month
  )

  asset_swap <- AssetSwap(
    pay_fixed_rate,
    bond,
    clean_price,
    ibor_index,
    spread,
    floating_schedule,
    floating_day_counter,
    par_asset_swap
  )

  engine <- DiscountingSwapEngine(discount_curve_handle)
  asset_swap$setPricingEngine(engine)

  tibble::tibble(
    metric = c("fair_spread", "fair_clean_price"),
    value = c(
      tryCatch(asset_swap$fairSpread(), error = function(e) NA_real_),
      tryCatch(asset_swap$fairCleanPrice(), error = function(e) NA_real_)
    )
  )
}

# ------------------------------------------------------------
# 11. full QuantLib chapter-style pipeline
# ------------------------------------------------------------

#' Full QuantLib bond analysis pipeline
#' @export
qlr_bond_ch4_analysis <- function(
    trade_date,
    settlement_days = 2,
    effective_date,
    maturity_date,
    face_amount = 100,
    coupon_rate = 0.01,
    clean_price_input = 100,
    calendar,
    schedule_frequency,
    accrual_day_counter,
    compounding,
    frequency,
    payment_convention,
    maturity_convention = payment_convention,
    date_generation,
    end_of_month = FALSE
) {
  bond_obj <- qlr_fixed_rate_bond(
    trade_date = trade_date,
    settlement_days = settlement_days,
    effective_date = effective_date,
    maturity_date = maturity_date,
    face_amount = face_amount,
    coupon_rate = coupon_rate,
    calendar = calendar,
    schedule_frequency = schedule_frequency,
    accrual_day_counter = accrual_day_counter,
    payment_convention = payment_convention,
    maturity_convention = maturity_convention,
    date_generation = date_generation,
    end_of_month = end_of_month
  )

  yield <- qlr_bond_yield_from_clean_price(
    bond = bond_obj$bond,
    clean_price = clean_price_input,
    day_counter = accrual_day_counter,
    compounding = compounding,
    frequency = frequency
  )

  price_table <- qlr_bond_price_measures(
    bond = bond_obj$bond,
    yield = yield,
    day_counter = accrual_day_counter,
    compounding = compounding,
    frequency = frequency,
    settlement_date = bond_obj$settlement_date,
    schedule = bond_obj$schedule
  )

  risk_table <- qlr_bond_risk_measures(
    bond = bond_obj$bond,
    yield = yield,
    day_counter = accrual_day_counter,
    compounding = compounding,
    frequency = frequency
  )

  dirty_price <- tryCatch(
    bond_obj$bond$dirtyPrice(
      yield,
      accrual_day_counter,
      compounding,
      frequency
    ),
    error = function(e) NA_real_
  )

  modified_duration <- risk_table |>
    dplyr::filter(metric == "modified_duration") |>
    dplyr::pull(value) |>
    .qlr_bond_extract_scalar()

  convexity <- risk_table |>
    dplyr::filter(metric == "convexity") |>
    dplyr::pull(value) |>
    .qlr_bond_extract_scalar()

  handcalc_table <- qlr_bond_risk_handcalc(
    bond = bond_obj$bond,
    yield = yield,
    dirty_price = dirty_price,
    modified_duration = modified_duration,
    convexity = convexity,
    day_counter = accrual_day_counter,
    compounding = compounding,
    frequency = frequency
  )

  list(
    trade_date = bond_obj$trade_date,
    settlement_date = bond_obj$settlement_date,
    schedule = bond_obj$schedule,
    bond = bond_obj$bond,
    yield = yield,
    price_table = price_table,
    risk_table = risk_table,
    handcalc_table = handcalc_table
  )
}

# ------------------------------------------------------------
# 12. convenience example-style tables
# ------------------------------------------------------------

#' Bond summary table
#' @export
qlr_bond_summary_table <- function(
    bond,
    yield,
    day_counter,
    compounding,
    frequency,
    settlement_date = NULL,
    schedule = NULL
) {
  price_table <- qlr_bond_price_measures(
    bond = bond,
    yield = yield,
    day_counter = day_counter,
    compounding = compounding,
    frequency = frequency,
    settlement_date = settlement_date,
    schedule = schedule
  )

  risk_table <- qlr_bond_risk_measures(
    bond = bond,
    yield = yield,
    day_counter = day_counter,
    compounding = compounding,
    frequency = frequency
  )

  dplyr::bind_rows(price_table, risk_table)
}


#' Asset swap analysis for fixed-rate bond
# ------------------------------------------------------------
# internal helper
# ------------------------------------------------------------

.qlr_try_make_asset_swap <- function(
    bond,
    clean_price,
    ibor_index,
    spread,
    floating_schedule,
    floating_day_counter,
    par_asset_swap,
    maturity_date,
    pay_fixed_rate = TRUE,
    gearing = 1.0,
    non_par_repayment = 100.0
) {
  tryCatch(
    AssetSwap__SWIG_0(
      pay_fixed_rate,
      bond,
      clean_price,
      ibor_index,
      spread,
      floating_schedule,
      floating_day_counter,
      par_asset_swap,
      gearing,
      non_par_repayment,
      maturity_date
    ),
    error = function(e) NULL
  )
}

# ------------------------------------------------------------
# exported
# ------------------------------------------------------------

#' Asset swap analysis for fixed-rate bond
#' @export
qlr_asset_swap_analysis <- function(
    bond,
    clean_price,
    ibor_index,
    spread,
    settlement_date,
    maturity_date,
    calendar,
    floating_schedule_frequency,
    payment_convention,
    date_generation,
    end_of_month = FALSE,
    floating_day_counter,
    pay_fixed_rate = TRUE,
    par_asset_swap = TRUE,
    discount_curve_handle,
    gearing = 1.0,
    non_par_repayment = 100.0
) {
  settlement_date <- qlr_date(settlement_date)
  maturity_date <- qlr_date(maturity_date)

  floating_schedule <- Schedule(
    settlement_date,
    maturity_date,
    floating_schedule_frequency,
    calendar,
    payment_convention,
    payment_convention,
    date_generation,
    end_of_month
  )

  asset_swap_obj <- .qlr_try_make_asset_swap(
    bond = bond,
    clean_price = clean_price,
    ibor_index = ibor_index,
    spread = spread,
    floating_schedule = floating_schedule,
    floating_day_counter = floating_day_counter,
    par_asset_swap = par_asset_swap,
    maturity_date = maturity_date,
    pay_fixed_rate = pay_fixed_rate,
    gearing = gearing,
    non_par_repayment = non_par_repayment
  )

  if (is.null(asset_swap_obj)) {
    return(
      tibble::tibble(
        metric = c("fair_spread", "fair_clean_price", "status"),
        value = list(
          NA_real_,
          NA_real_,
          "AssetSwap__SWIG_0 constructor failed in this SWIG build"
        )
      )
    )
  }

  asset_swap_engine <- DiscountingSwapEngine(discount_curve_handle)
  Instrument_setPricingEngine(asset_swap_obj, asset_swap_engine)

  tibble::tibble(
    metric = c("fair_spread", "fair_clean_price", "status"),
    value = list(
      AssetSwap_fairSpread(asset_swap_obj),
      AssetSwap_fairCleanPrice(asset_swap_obj),
      "ok"
    )
  )
}

fmt_pct <- function(x, digits = 4) {
  sprintf(paste0("%.", digits, "f%%"), 100 * x)
}

make_us_tsy_bond <- function(
    effective_date,
    maturity_date,
    coupon_rate_pct,
    face_amount = 100,
    settlement_days = 1L
) {
  effective_date_ql <- qlr_date(effective_date)
  maturity_date_ql <- qlr_date(maturity_date)

  bond_schedule <- Schedule(
    effective_date_ql,
    maturity_date_ql,
    qlr_period_months(6),
    cal_us_gov,
    "Unadjusted",
    "Unadjusted",
    "Backward",
    FALSE
  )

  bond_obj <- FixedRateBond(
    settlement_days,
    face_amount,
    bond_schedule,
    c(coupon_rate_pct / 100),
    dc_act_act_bond
  )

  list(
    bond = bond_obj,
    schedule = bond_schedule,
    effective_date = effective_date_ql,
    maturity_date = maturity_date_ql,
    coupon_rate_pct = coupon_rate_pct,
    face_amount = face_amount
  )
}


# ------------------------------------------------------------
# 13. US Treasury bond helpers
# ------------------------------------------------------------

#' Build a US Treasury fixed-rate bond
#' @export
qlr_us_treasury_bond <- function(
    effective_date,
    maturity_date,
    coupon_rate_pct,
    face_amount = 100,
    settlement_days = 1L,
    calendar = UnitedStates("GovernmentBond"),
    day_counter = ActualActual("Bond")
) {
  effective_date <- qlr_date(effective_date)
  maturity_date <- qlr_date(maturity_date)

  schedule <- Schedule(
    effective_date,
    maturity_date,
    qlr_period_months(6),
    calendar,
    "Unadjusted",
    "Unadjusted",
    "Backward",
    FALSE
  )

  bond <- FixedRateBond(
    as.integer(settlement_days),
    face_amount,
    schedule,
    c(coupon_rate_pct / 100),
    day_counter
  )

  list(
    bond = bond,
    schedule = schedule,
    effective_date = effective_date,
    maturity_date = maturity_date,
    coupon_rate_pct = coupon_rate_pct,
    face_amount = face_amount
  )
}

# ------------------------------------------------------------
# 14. Treasury futures basis helpers
# ------------------------------------------------------------

#' Calculate one-row gross basis measures for a deliverable bond
#' @export
qlr_bond_futures_gross_basis_row <- function(
    issue_date,
    maturity_date,
    coupon_rate_pct,
    conversion_factor,
    market_yield_pct,
    settlement_date,
    futures_price,
    settlement_days = 1L,
    calendar = UnitedStates("GovernmentBond"),
    day_counter = ActualActual("Bond"),
    compounding = Compounding_Compounded_get(),
    frequency = Frequency_Semiannual_get()
) {
  bond_bundle <- qlr_us_treasury_bond(
    effective_date = issue_date,
    maturity_date = maturity_date,
    coupon_rate_pct = coupon_rate_pct,
    settlement_days = settlement_days,
    calendar = calendar,
    day_counter = day_counter
  )

  bond_obj <- bond_bundle$bond

  interest_rate_obj <- InterestRate(
    market_yield_pct / 100,
    day_counter,
    compounding,
    frequency
  )

  bpv_value <- BondFunctions_basisPointValue(
    bond_obj,
    interest_rate_obj
  )

  clean_price <- bond_obj$cleanPrice(
    market_yield_pct / 100,
    day_counter,
    compounding,
    frequency,
    settlement_date
  )

  dirty_price <- bond_obj$dirtyPrice(
    market_yield_pct / 100,
    day_counter,
    compounding,
    frequency,
    settlement_date
  )

  gross_basis <- clean_price - futures_price * conversion_factor

  tibble::tibble(
    issue_date = qlr_iso(bond_bundle$effective_date),
    maturity = qlr_iso(bond_obj$maturityDate()),
    coupon = bond_obj$nextCouponRate(),
    yield_pct = market_yield_pct,
    bpv = bpv_value,
    clean_price = clean_price,
    dirty_price = dirty_price,
    conversion_factor = conversion_factor,
    gross_basis = gross_basis,
    bond_obj = list(bond_obj)
  )
}

#' Calculate one-row net basis / carry / implied repo measures
#' @export
qlr_bond_futures_net_basis_row <- function(
    bond_obj,
    conversion_factor,
    clean_price,
    dirty_price,
    gross_basis,
    settlement_date,
    repo_end_date,
    repo_rate,
    repo_day_counter = Actual360(),
    carry_day_counter = Actual360(),
    futures_price
) {
  accrued_start <- bond_obj$accruedAmount(settlement_date)
  accrued_end <- bond_obj$accruedAmount(repo_end_date)

  coupon_income <- accrued_end - accrued_start
  repo_year_fraction <- repo_day_counter$yearFraction(settlement_date, repo_end_date)
  repo_cost <- repo_rate * repo_year_fraction * dirty_price
  carry <- coupon_income - repo_cost
  net_basis <- gross_basis - carry
  forward_price <- clean_price - carry

  implied_repo <- (
    (futures_price * conversion_factor + accrued_end) / dirty_price - 1
  ) / repo_year_fraction

  tibble::tibble(
    accrued_start = accrued_start,
    accrued_end = accrued_end,
    coupon_income = coupon_income,
    dirty_price = dirty_price,
    repo_cost = repo_cost,
    carry = carry,
    net_basis = net_basis,
    forward_price = forward_price,
    implied_repo = implied_repo
  )
}
