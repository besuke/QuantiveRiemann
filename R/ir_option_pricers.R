# ============================================================
# QuantiveRiemann - ir_option_pricers.R
# Interest-rate option pricers
#
# QuantiveRiemann keeps the qlr_* educational interface,
# while pricing is delegated to QuantLibGauss.
# ============================================================

.qlr_ir_require_quantlibgauss <- function() {
  if (!requireNamespace("QuantLibGauss", quietly = TRUE)) {
    stop(
      "QuantLibGauss is required for interest-rate option pricing.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

.qlr_ir_hw_cap_floor_engine <- function(
    curve_handle = NULL,
    valuation_date = "2023-01-03",
    discount_rate = 0.03,
    hw_a = 0.03,
    hw_sigma = 0.01,
    method = c("analytic", "tree"),
    time_steps = 60L
) {
  method <- match.arg(method)

  if (!is.null(curve_handle)) {
    return(
      QuantLibGauss::qlg_hull_white_cap_floor_engine(
        term_structure = curve_handle,
        a = hw_a,
        sigma = hw_sigma,
        method = method,
        time_steps = time_steps
      )
    )
  }

  QuantLibGauss::qlg_hull_white_cap_floor_engine(
    valuation_date = valuation_date,
    rate = discount_rate,
    a = hw_a,
    sigma = hw_sigma,
    method = method,
    time_steps = time_steps
  )
}

.qlr_ir_hw_swaption_engine <- function(
    curve_handle = NULL,
    valuation_date = "2023-01-03",
    discount_rate = 0.03,
    hw_a = 0.03,
    hw_sigma = 0.01,
    method = c("jamshidian", "tree"),
    time_steps = 60L
) {
  method <- match.arg(method)

  if (!is.null(curve_handle)) {
    return(
      QuantLibGauss::qlg_hull_white_swaption_engine(
        term_structure = curve_handle,
        a = hw_a,
        sigma = hw_sigma,
        method = method,
        time_steps = time_steps
      )
    )
  }

  QuantLibGauss::qlg_hull_white_swaption_engine(
    valuation_date = valuation_date,
    rate = discount_rate,
    a = hw_a,
    sigma = hw_sigma,
    method = method,
    time_steps = time_steps
  )
}

qlr_ir_price_cap_floor <- function(
    float_leg = NULL,
    strike,
    curve_handle = NULL,
    vol = 0.20,
    notional = 1e6,
    start_date = "2023-01-05",
    maturity_date = "2028-01-05",
    valuation_date = "2023-01-03",
    discount_rate = 0.03,
    forecast_rate = discount_rate,
    hw_a = 0.03,
    hw_sigma = 0.01,
    hw_method = c("analytic", "tree"),
    time_steps = 60L,
    verbose = TRUE
) {
  .qlr_ir_require_quantlibgauss()

  hw_method <- match.arg(hw_method)

  .qlr_ir_msg(
    verbose,
    "[qlr_ir_price_cap_floor] strike = ",
    strike,
    ", vol = ",
    vol
  )

  cap_black <- QuantLibGauss::qlg_make_cap(
    notional = notional,
    start_date = start_date,
    maturity_date = maturity_date,
    cap_rate = strike,
    valuation_date = valuation_date,
    discount_rate = discount_rate,
    forecast_rate = forecast_rate,
    volatility = vol
  )

  floor_black <- QuantLibGauss::qlg_make_floor(
    notional = notional,
    start_date = start_date,
    maturity_date = maturity_date,
    floor_rate = strike,
    valuation_date = valuation_date,
    discount_rate = discount_rate,
    forecast_rate = forecast_rate,
    volatility = vol
  )

  cap_hw <- QuantLibGauss::qlg_make_cap(
    notional = notional,
    start_date = start_date,
    maturity_date = maturity_date,
    cap_rate = strike,
    valuation_date = valuation_date,
    discount_rate = discount_rate,
    forecast_rate = forecast_rate,
    volatility = vol,
    pricing_engine = .qlr_ir_hw_cap_floor_engine(
      curve_handle = curve_handle,
      valuation_date = valuation_date,
      discount_rate = discount_rate,
      hw_a = hw_a,
      hw_sigma = hw_sigma,
      method = hw_method,
      time_steps = time_steps
    )
  )

  floor_hw <- QuantLibGauss::qlg_make_floor(
    notional = notional,
    start_date = start_date,
    maturity_date = maturity_date,
    floor_rate = strike,
    valuation_date = valuation_date,
    discount_rate = discount_rate,
    forecast_rate = forecast_rate,
    volatility = vol,
    pricing_engine = .qlr_ir_hw_cap_floor_engine(
      curve_handle = curve_handle,
      valuation_date = valuation_date,
      discount_rate = discount_rate,
      hw_a = hw_a,
      hw_sigma = hw_sigma,
      method = hw_method,
      time_steps = time_steps
    )
  )

  tibble::tibble(
    instrument = c("Cap", "Floor", "Cap", "Floor"),
    model = c("Black", "Black", "Hull-White", "Hull-White"),
    strike = rep(strike, 4),
    vol = c(vol, vol, NA_real_, NA_real_),
    npv = c(
      QuantLibGauss::qlg_cap_floor_npv(cap_black),
      QuantLibGauss::qlg_cap_floor_npv(floor_black),
      QuantLibGauss::qlg_cap_floor_npv(cap_hw),
      QuantLibGauss::qlg_cap_floor_npv(floor_hw)
    ),
    vega = c(
      QuantLibGauss::qlg_cap_floor_vega(cap_black),
      QuantLibGauss::qlg_cap_floor_vega(floor_black),
      QuantLibGauss::qlg_cap_floor_vega(cap_hw),
      QuantLibGauss::qlg_cap_floor_vega(floor_hw)
    )
  )
}

qlr_ir_price_swaption <- function(
    swap,
    exercise_date,
    curve_handle = NULL,
    vol = 0.20,
    valuation_date = "2023-01-03",
    discount_rate = 0.03,
    hw_a = 0.03,
    hw_sigma = 0.01,
    hw_method = c("jamshidian", "tree"),
    time_steps = 60L,
    verbose = TRUE
) {
  .qlr_ir_require_quantlibgauss()

  hw_method <- match.arg(hw_method)

  .qlr_ir_msg(
    verbose,
    "[qlr_ir_price_swaption] exercise_date = ",
    exercise_date,
    ", vol = ",
    vol
  )

  engine_hw <- .qlr_ir_hw_swaption_engine(
    curve_handle = curve_handle,
    valuation_date = valuation_date,
    discount_rate = discount_rate,
    hw_a = hw_a,
    hw_sigma = hw_sigma,
    method = hw_method,
    time_steps = time_steps
  )

  swaption_hw <- QuantLibGauss::qlg_make_swaption(
    underlying_swap = swap,
    exercise_date = exercise_date,
    pricing_engine = engine_hw
  )

  tibble::tibble(
    model = "Hull-White",
    method = hw_method,
    vol = NA_real_,
    npv = QuantLibGauss::qlg_swaption_npv(swaption_hw),
    vega = QuantLibGauss::qlg_swaption_vega(swaption_hw),
    annuity = QuantLibGauss::qlg_swaption_annuity(swaption_hw)
  )
}
