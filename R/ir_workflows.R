# ============================================================
# QuantiveRiemann - ir_workflows.R
# Interest-rate workflows / demo object builders
# ============================================================

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
  curve_env <- qlr_ir_make_flat_curve(
    rate = rate,
    trade_date = trade_date,
    verbose = verbose
  )
  
  swap_env <- qlr_ir_make_swap(
    curve_handle = curve_env$handle,
    effective = effective,
    maturity = maturity,
    nominal = nominal,
    fixed_rate = fixed_rate,
    verbose = verbose
  )
  
  cap_floor_tbl <- qlr_ir_price_cap_floor(
    float_leg = qlr_swap_float_leg(swap_env$swap),
    strike = strike,
    curve_handle = curve_env$handle,
    verbose = verbose
  )
  
  swaption_tbl <- qlr_ir_price_swaption(
    swap = swap_env$swap,
    exercise_date = exercise_date,
    curve_handle = curve_env$handle,
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