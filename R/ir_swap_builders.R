# ============================================================
# QuantiveRiemann - ir_swap_builders.R
# Interest-rate swaps
# ============================================================


.qlr_ir_make_schedule <- function(
    effective,
    maturity,
    tenor,
    calendar,
    bdc = "ModifiedFollowing",
    date_rule = "Backward",
    eom = FALSE
) {
  Schedule(
    qlr_date(effective),
    qlr_date(maturity),
    tenor,
    calendar,
    bdc,
    bdc,
    date_rule,
    eom
  )
}

qlr_ir_make_swap <- function(
    curve_handle,
    effective,
    maturity,
    nominal = 1e6,
    fixed_rate = 0.03,
    float_spread = 0.0,
    calendar = TARGET(),
    fixed_tenor = qlr_period_years(1),
    float_tenor = qlr_period_months(6),
    fixed_day_counter = Thirty360("BondBasis"),
    float_day_counter = Actual360(),
    fixed_bdc = "ModifiedFollowing",
    float_bdc = "ModifiedFollowing",
    date_rule = "Backward",
    index_builder = function(handle) Euribor6M(handle),
    swap_type = Swap_Payer_get(),
    verbose = TRUE
) {
  .qlr_ir_msg(verbose, "[qlr_ir_make_swap] start: ", effective, " -> ", maturity)

  fixed_schedule <- .qlr_ir_make_schedule(
    effective = effective,
    maturity = maturity,
    tenor = fixed_tenor,
    calendar = calendar,
    bdc = fixed_bdc,
    date_rule = date_rule
  )

  float_schedule <- .qlr_ir_make_schedule(
    effective = effective,
    maturity = maturity,
    tenor = float_tenor,
    calendar = calendar,
    bdc = float_bdc,
    date_rule = date_rule
  )

  index <- index_builder(curve_handle)

  swap <- VanillaSwap(
    swap_type,
    nominal,
    fixed_schedule,
    fixed_rate,
    fixed_day_counter,
    float_schedule,
    index,
    float_spread,
    float_day_counter
  )

  engine <- DiscountingSwapEngine(curve_handle)
  qlr_safe_engine_set(swap, engine)

  out <- list(
    swap = swap,
    fixed_schedule = fixed_schedule,
    float_schedule = float_schedule,
    index = index,
    curve_handle = curve_handle
  )

  .qlr_ir_msg(verbose, "[qlr_ir_make_swap] done")
  out
}

qlr_ir_fixing_table <- function(swap, curve, index) {
  float_leg <- qlr_swap_float_leg(swap)

  tibble::tibble(i = seq_len(float_leg$size())) |>
    dplyr::mutate(
      cf = purrr::map(i, ~ qlr_leg_cashflow_at(float_leg, .x)),
      fixing_date = purrr::map_chr(
        cf,
        ~ qlr_iso(FloatingRateCoupon_fixingDate(as_floating_rate_coupon(.x)))
      ),
      fixing_value = purrr::map_dbl(
        cf,
        ~ tryCatch(
          index$fixing(FloatingRateCoupon_fixingDate(as_floating_rate_coupon(.x))),
          error = function(e) NA_real_
        )
      ),
      pay_date = purrr::map_chr(cf, ~ qlr_iso(CashFlow_date(.x))),
      amount = purrr::map_dbl(cf, ~ qlr_cf_amount(.x)),
      df = purrr::map_dbl(
        cf,
        ~ tryCatch(curve$discount(CashFlow_date(.x)), error = function(e) NA_real_)
      ),
      pv = amount * df
    ) |>
    dplyr::select(-cf)
}
.qlr_ir_get_ois_swap_convention <- function(currency) {
  currency <- toupper(trimws(currency))

  switch(
    currency,
    "JPY" = list(
      calendar = Japan(),
      fixed_tenor = qlr_period_years(1),
      float_tenor = qlr_period_years(1),
      fixed_day_counter = Actual365Fixed(),
      float_day_counter = Actual365Fixed(),
      index_builder = function(handle) {
        OvernightIndex(
          "TONA",
          2,
          JPYCurrency(),
          Japan(),
          Actual365Fixed(),
          handle
        )
      }
    ),
    "USD" = list(
      calendar = UnitedStates("SOFR"),
      fixed_tenor = qlr_period_years(1),
      float_tenor = qlr_period_years(1),
      fixed_day_counter = Actual360(),
      float_day_counter = Actual360(),
      index_builder = function(handle) Sofr(handle)
    ),
    "EUR" = list(
      calendar = TARGET(),
      fixed_tenor = qlr_period_years(1),
      float_tenor = qlr_period_years(1),
      fixed_day_counter = Actual360(),
      float_day_counter = Actual360(),
      index_builder = function(handle) Estr(handle)
    ),
    "GBP" = list(
      calendar = UnitedKingdom("Settlement"),
      fixed_tenor = qlr_period_years(1),
      float_tenor = qlr_period_years(1),
      fixed_day_counter = Actual365Fixed(),
      float_day_counter = Actual365Fixed(),
      index_builder = function(handle) Sonia(handle)
    ),
    stop("Unsupported currency: ", currency)
  )
}

qlr_trade_irs_ois_swap <- function(
    curve_env,
    effective,
    maturity,
    nominal = 1e6,
    fixed_rate,
    float_spread = 0,
    verbose = TRUE
) {
  if (is.null(curve_env$currency)) {
    stop("curve_env must contain $currency")
  }

  if (is.null(curve_env$curve_handle)) {
    stop("curve_env must contain $curve_handle")
  }

  conv <- .qlr_ir_get_ois_swap_convention(curve_env$currency)

  qlr_ir_make_swap(
    curve_handle = curve_env$curve_handle,
    effective = effective,
    maturity = maturity,
    nominal = nominal,
    fixed_rate = fixed_rate,
    float_spread = float_spread,
    calendar = conv$calendar,
    fixed_tenor = conv$fixed_tenor,
    float_tenor = conv$float_tenor,
    fixed_day_counter = conv$fixed_day_counter,
    float_day_counter = conv$float_day_counter,
    index_builder = conv$index_builder,
    verbose = verbose
  )
}

make_vanilla_swap_safe <- function(
    index_obj,
    effective_date,
    maturity_date,
    fixed_rate,
    nominal = 1e7,
    swap_type = Swap_Payer_get(),
    calendar_obj = Japan(),
    fixed_tenor = qlr_period_years(1),
    float_tenor = qlr_period_years(1),
    fixed_day_count = Actual365Fixed(),
    float_day_count = Actual365Fixed(),
    bdc = "ModifiedFollowing",
    date_rule = "Backward",
    eom = FALSE,
    spread = 0
) {
  fixed_schedule <- Schedule(
    qlr_date(effective_date),
    qlr_date(maturity_date),
    fixed_tenor,
    calendar_obj,
    bdc,
    bdc,
    date_rule,
    eom
  )

  float_schedule <- Schedule(
    qlr_date(effective_date),
    qlr_date(maturity_date),
    float_tenor,
    calendar_obj,
    bdc,
    bdc,
    date_rule,
    eom
  )

  swap_obj <- VanillaSwap(
    swap_type,
    nominal,
    fixed_schedule,
    fixed_rate,
    fixed_day_count,
    float_schedule,
    index_obj,
    spread,
    float_day_count
  )

  list(
    swap = swap_obj,
    fixed_schedule = fixed_schedule,
    float_schedule = float_schedule
  )
}
