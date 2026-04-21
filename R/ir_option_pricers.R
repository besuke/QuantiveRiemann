# ============================================================
# QuantiveRiemann - ir_option_pricers.R
# Interest-rate option pricers
# ============================================================


qlr_ir_price_cap_floor <- function(
    float_leg,
    strike,
    curve_handle,
    vol = 0.20,
    verbose = TRUE
) {
  .qlr_ir_msg(
    verbose,
    "[qlr_ir_price_cap_floor] strike = ",
    strike,
    ", vol = ",
    vol
  )

  cap <- Cap(float_leg, c(strike))
  floor <- Floor(float_leg, c(strike))

  vol_handle <- QuoteHandle(SimpleQuote(vol))

  engine_black <- tryCatch(
    BlackCapFloorEngine(curve_handle, vol_handle, Actual365Fixed()),
    error = function(e) NULL
  )

  if (!is.null(engine_black)) {
    qlr_safe_engine_set (cap, engine_black)
    qlr_safe_engine_set (floor, engine_black)
  }

  tibble::tibble(
    instrument = c("Cap", "Floor"),
    strike = strike,
    vol = vol,
    npv = c(qlr_option_npv(cap), qlr_option_npv(floor))
  )
}

qlr_ir_price_swaption <- function(
    swap,
    exercise_date,
    curve_handle,
    vol = 0.20,
    verbose = TRUE
) {
  .qlr_ir_msg(
    verbose,
    "[qlr_ir_price_swaption] exercise_date = ",
    exercise_date,
    ", vol = ",
    vol
  )

  exercise <- EuropeanExercise(qlr_date(exercise_date))
  swaption <- Swaption(swap, exercise)

  vol_handle <- QuoteHandle(SimpleQuote(vol))

  engine_black <- tryCatch(
    BlackSwaptionEngine(curve_handle, vol_handle),
    error = function(e) NULL
  )

  npv_black <- NA_real_
  if (!is.null(engine_black)) {
    qlr_safe_engine_set (swaption, engine_black)
    npv_black <- qlr_option_npv(swaption)
  }

  hw <- qlr_make_hw(curve_handle)
  engine_hw <- tryCatch(
    JamshidianSwaptionEngine(hw),
    error = function(e) NULL
  )

  npv_hw <- NA_real_
  if (!is.null(engine_hw)) {
    swaption_hw <- Swaption(swap, exercise)
    qlr_safe_engine_set (swaption_hw, engine_hw)
    npv_hw <- qlr_option_npv(swaption_hw)
  }

  tibble::tibble(
    model = c("Black", "Hull-White"),
    vol = c(vol, NA_real_),
    npv = c(npv_black, npv_hw)
  )
}

qlr_ir_analysis <- function(verbose = TRUE) {
  .qlr_ir_msg(verbose, "[qlr_ir_analysis] start")

  curve_env <- qlr_ir_make_flat_curve(verbose = verbose)

  swap_env <- qlr_ir_make_swap(
    curve_handle = curve_env$handle,
    effective = "2023-01-05",
    maturity = "2028-01-05",
    nominal = 1e6,
    fixed_rate = 0.03,
    verbose = verbose
  )

  cap_floor <- qlr_ir_price_cap_floor(
    float_leg = qlr_swap_float_leg(swap_env$swap),
    strike = 0.03,
    curve_handle = curve_env$handle,
    verbose = verbose
  )

  swaption <- qlr_ir_price_swaption(
    swap = swap_env$swap,
    exercise_date = "2024-01-03",
    curve_handle = curve_env$handle,
    verbose = verbose
  )

  out <- list(
    curve = curve_env,
    swap = swap_env,
    cap_floor = cap_floor,
    swaption = swaption
  )

  .qlr_ir_msg(verbose, "[qlr_ir_analysis] done")
  out
}
