# ============================================================
# QuantiveRiemann - ir_workflows.R
# Interest-rate workflows / demo object builders
#
# QuantiveRiemann keeps the qlr_* educational workflow,
# while trade construction and option pricing are delegated
# to QuantLibGauss.
# ============================================================

.qlr_ir_flat_curve_env <- function(
    rate = 0.03,
    trade_date = "2023-01-03"
) {
  .qlr_ir_require_quantlibgauss()

  QuantLibGauss::qlg_eval_date(trade_date)

  day_counter <- QuantLib::Actual365Fixed()

  curve <- QuantLib::FlatForward(
    QuantLibGauss::qlg_date(trade_date),
    as.numeric(rate),
    day_counter
  )

  handle <- QuantLib::YieldTermStructureHandle(curve)

  list(
    curve = curve,
    handle = handle,
    rate = rate,
    trade_date = trade_date
  )
}

qlr_ir_build_demo_objects <- function(
    rate = 0.03,
    trade_date = "2023-01-03",
    effective = "2023-01-05",
    maturity = "2028-01-05",
    nominal = 1e6,
    fixed_rate = 0.03,
    strike = 0.03,
    exercise_date = "2024-01-03",
    verbose = TRUE
) {
  .qlr_ir_require_quantlibgauss()

  curve_env <- .qlr_ir_flat_curve_env(
    rate = rate,
    trade_date = trade_date
  )

  swap_trade <- tibble::tibble(
    effective_date = effective,
    maturity_date = maturity,
    fixed_rate = fixed_rate,
    notional = nominal,
    swap_type = "payer",
    index = "Euribor6M",
    spread = 0,
    fixed_tenor_n = 1,
    fixed_tenor_unit = "Years",
    floating_tenor_n = 6,
    floating_tenor_unit = "Months"
  )

  swap_obj <- QuantLibGauss::qlg_make_vanilla_swap_from_trade(
    trade = swap_trade,
    forecast_handle = curve_env$handle,
    discount_handle = curve_env$handle
  )

  swap_env <- list(
    swap = swap_obj,
    trade = swap_trade
  )

  cap_floor_tbl <- qlr_ir_price_cap_floor(
    strike = strike,
    curve_handle = curve_env$handle,
    notional = nominal,
    start_date = effective,
    maturity_date = maturity,
    valuation_date = trade_date,
    discount_rate = rate,
    forecast_rate = rate,
    verbose = verbose
  )

  swaption_tbl <- qlr_ir_price_swaption(
    swap = swap_obj,
    exercise_date = exercise_date,
    curve_handle = curve_env$handle,
    valuation_date = trade_date,
    discount_rate = rate,
    verbose = verbose
  )

  list(
    curve_env = curve_env,
    swap_env = swap_env,
    cap_floor_tbl = cap_floor_tbl,
    swaption_tbl = swaption_tbl
  )
}

qlr_ir_analysis <- function(verbose = TRUE) {
  out <- qlr_ir_build_demo_objects(verbose = verbose)

  list(
    curve = out$curve_env,
    swap = out$swap_env,
    cap_floor = out$cap_floor_tbl,
    swaption = out$swaption_tbl
  )
}