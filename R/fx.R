# ============================================================
# QuantiveRiemann - fx.R
# FX derivatives / Garman-Kohlhagen / FX forward / FX option
# ============================================================

# ------------------------------------------------------------
# 1. FX market setup
# ------------------------------------------------------------

qlr_fx_setup <- function(
    eval_date = "2023-01-03",
    spot_fx = 130,
    foreign_rate = 0.01,
    domestic_rate = 0.03,
    volatility = 0.12,
    maturity = "2024-01-03",
    strike = 130
) {
  qlr_set_eval_date(eval_date)

  calendar <- TARGET()
  dc <- Actual365Fixed()

  spot_quote <- SimpleQuote(spot_fx)
  spot_handle <- QuoteHandle(spot_quote)

  foreign_curve <- FlatForward(qlr_date(eval_date), foreign_rate, dc)
  domestic_curve <- FlatForward(qlr_date(eval_date), domestic_rate, dc)
  vol_curve <- BlackConstantVol(qlr_date(eval_date), calendar, volatility, dc)

  TermStructure_enableExtrapolation(foreign_curve)
  TermStructure_enableExtrapolation(domestic_curve)

  list(
    eval_date = eval_date,
    maturity = maturity,
    strike = strike,
    spot_handle = spot_handle,
    foreign = YieldTermStructureHandle(foreign_curve),
    domestic = YieldTermStructureHandle(domestic_curve),
    vol = BlackVolTermStructureHandle(vol_curve),
    calendar = calendar,
    dc = dc
  )
}

# ------------------------------------------------------------
# 2. FX forward table
# ------------------------------------------------------------

qlr_fx_forward_table <- function(spot, foreign_curve, domestic_curve) {
  tibble::tibble(time = c(1 / 12, 3 / 12, 6 / 12, 1, 2, 3)) %>%
    dplyr::mutate(
      df_foreign = purrr::map_dbl(time, ~ foreign_curve$discount(.x)),
      df_domestic = purrr::map_dbl(time, ~ domestic_curve$discount(.x)),
      forward = qlr_fx_forward(spot, df_domestic, df_foreign),
      points = forward - spot
    )
}

# ------------------------------------------------------------
# 3. Garman-Kohlhagen process
# ------------------------------------------------------------

qlr_fx_make_process <- function(fx_env) {
  qlr_make_fx_process(
    spot = fx_env$spot_handle,
    foreign = fx_env$foreign,
    domestic = fx_env$domestic,
    vol = fx_env$vol
  )
}

# ------------------------------------------------------------
# 4. European FX option valuation
# ------------------------------------------------------------

qlr_fx_option_price <- function(fx_env, process) {
  payoff_call <- PlainVanillaPayoff("Call", fx_env$strike)
  payoff_put <- PlainVanillaPayoff("Put", fx_env$strike)

  exercise <- EuropeanExercise(qlr_date(fx_env$maturity))

  call_opt <- VanillaOption(payoff_call, exercise)
  put_opt <- VanillaOption(payoff_put, exercise)

  engine <- qlr_make_analytic_european(process)

  qlr_option_set_engine(call_opt, engine)
  qlr_option_set_engine(put_opt, engine)

  tibble::tibble(
    type = c("Call", "Put"),
    npv = c(qlr_option_npv(call_opt), qlr_option_npv(put_opt)),
    delta = c(qlr_option_greek(call_opt, "delta"), qlr_option_greek(put_opt, "delta")),
    gamma = c(qlr_option_greek(call_opt, "gamma"), qlr_option_greek(put_opt, "gamma")),
    vega = c(qlr_option_greek(call_opt, "vega"), qlr_option_greek(put_opt, "vega")),
    theta = c(qlr_option_greek(call_opt, "theta"), qlr_option_greek(put_opt, "theta")),
    rho = c(qlr_option_greek(call_opt, "rho"), qlr_option_greek(put_opt, "rho")),
    dividend_rho = c(
      qlr_option_greek(call_opt, "dividendRho"),
      qlr_option_greek(put_opt, "dividendRho")
    )
  )
}

# ------------------------------------------------------------
# 5. Put-call parity check
# ------------------------------------------------------------

qlr_fx_put_call_parity <- function(fx_env, foreign_curve, domestic_curve, call_npv, put_npv) {
  t <- fx_env$dc$yearFraction(qlr_date(fx_env$eval_date), qlr_date(fx_env$maturity))

  df_f <- foreign_curve$discount(t)
  df_d <- domestic_curve$discount(t)

  lhs <- call_npv - put_npv
  rhs <- qlr_quote_get(fx_env$spot_handle) * df_f - fx_env$strike * df_d

  tibble::tibble(
    lhs = lhs,
    rhs = rhs,
    diff = lhs - rhs
  )
}

# ------------------------------------------------------------
# 6. Scenario table
# ------------------------------------------------------------

.qlr_fx_call_scenario <- function(fx_env, spot, vol, domestic_rate) {
  local_spot <- QuoteHandle(SimpleQuote(spot))

  local_domestic <- YieldTermStructureHandle(
    FlatForward(qlr_date(fx_env$eval_date), domestic_rate, fx_env$dc)
  )

  local_vol <- BlackVolTermStructureHandle(
    BlackConstantVol(qlr_date(fx_env$eval_date), fx_env$calendar, vol, fx_env$dc)
  )

  process <- qlr_make_fx_process(
    spot = local_spot,
    foreign = fx_env$foreign,
    domestic = local_domestic,
    vol = local_vol
  )

  engine <- qlr_make_analytic_european(process)
  payoff <- PlainVanillaPayoff("Call", fx_env$strike)
  exercise <- EuropeanExercise(qlr_date(fx_env$maturity))

  opt <- VanillaOption(payoff, exercise)
  qlr_option_set_engine(opt, engine)

  qlr_option_npv(opt)
}

qlr_fx_scenario_table <- function(fx_env) {
  tidyr::crossing(
    spot = c(120, 125, 130, 135, 140),
    vol = c(0.08, 0.12, 0.16),
    domestic = c(0.02, 0.03, 0.04)
  ) %>%
    dplyr::mutate(
      call_npv = purrr::pmap_dbl(
        list(spot, vol, domestic),
        ~ .qlr_fx_call_scenario(fx_env, ..1, ..2, ..3)
      )
    )
}

# ------------------------------------------------------------
# 7. FX analysis pipeline
# ------------------------------------------------------------

qlr_fx_analysis <- function() {
  fx_env <- qlr_fx_setup()

  forward_tbl <- qlr_fx_forward_table(
    qlr_quote_get(fx_env$spot_handle),
    fx_env$foreign$ptr(),
    fx_env$domestic$ptr()
  )

  process <- qlr_fx_make_process(fx_env)

  option_tbl <- qlr_fx_option_price(fx_env, process)

  parity_tbl <- qlr_fx_put_call_parity(
    fx_env,
    fx_env$foreign$ptr(),
    fx_env$domestic$ptr(),
    option_tbl$npv[1],
    option_tbl$npv[2]
  )

  scenario_tbl <- qlr_fx_scenario_table(fx_env)

  list(
    env = fx_env,
    forward = forward_tbl,
    option = option_tbl,
    parity = parity_tbl,
    scenario = scenario_tbl
  )
}
