devtools::load_all(".")

curve_env <- qlr_ir_make_flat_curve(
  rate = 0.03,
  trade_date = "2023-01-03"
)

swap_env <- qlr_ir_make_swap(
  curve_handle = curve_env$handle,
  effective = "2023-01-05",
  maturity = "2028-01-05",
  nominal = 1e6,
  fixed_rate = 0.03
)

cap_floor_tbl <- qlr_ir_price_cap_floor(
  float_leg = qlr_swap_float_leg(swap_env$swap),
  strike = 0.03,
  curve_handle = curve_env$handle
)

message("=== Cap / Floor valuation ===")
print(cap_floor_tbl)

swaption_tbl <- qlr_ir_price_swaption(
  swap = swap_env$swap,
  exercise_date = "2024-01-03",
  curve_handle = curve_env$handle
)

message("=== Swaption valuation ===")
print(swaption_tbl)

full <- qlr_ir_analysis()

message("=== Full IR pipeline results ===")
print(full$cap_floor)
print(full$swaption)