# ============================================================
# QuantiveRiemann - equity.R
# Equity derivatives / BS model / American options
# ============================================================

suppressMessages({
  library(QuantLib)
  library(tidyverse)
})

# ------------------------------------------------------------
# 1. Black-Scholes environment
# ------------------------------------------------------------

equity_setup <- function(
    eval_date = "2023-01-03",
    spot = 100,
    div = 0.01,
    rf = 0.02,
    vol = 0.20,
    maturity = "2024-01-03",
    strike = 100
) {
  set_eval_date(eval_date)

  dc <- Actual365Fixed()
  calendar <- TARGET()

  spot_handle <- QuoteHandle(SimpleQuote(spot))
  div_curve <- YieldTermStructureHandle(FlatForward(ql_date(eval_date), div, dc))
  rf_curve  <- YieldTermStructureHandle(FlatForward(ql_date(eval_date), rf, dc))
  vol_curve <- BlackVolTermStructureHandle(BlackConstantVol(ql_date(eval_date), calendar, vol, dc))

  list(
    eval_date = eval_date,
    maturity = maturity,
    strike = strike,
    spot = spot_handle,
    div = div_curve,
    rf = rf_curve,
    vol = vol_curve
  )
}

# ------------------------------------------------------------
# 2. European option
# ------------------------------------------------------------

equity_european <- function(env) {
  process <- ql_make_bsm_process(env$spot, env$div, env$rf, env$vol)
  engine <- ql_make_analytic_european(process)

  payoff_call <- PlainVanillaPayoff("Call", env$strike)
  payoff_put  <- PlainVanillaPayoff("Put",  env$strike)
  exercise <- EuropeanExercise(ql_date(env$maturity))

  call <- VanillaOption(payoff_call, exercise)
  put  <- VanillaOption(payoff_put,  exercise)

  ql_set_engine(call, engine)
  ql_set_engine(put,  engine)

  tibble(
    type = c("Call", "Put"),
    npv = c(ql_option_npv(call), ql_option_npv(put)),
    delta = c(ql_option_greek(call, "delta"), ql_option_greek(put, "delta")),
    gamma = c(ql_option_greek(call, "gamma"), ql_option_greek(put, "gamma")),
    vega  = c(ql_option_greek(call, "vega"),  ql_option_greek(put, "vega"))
  )
}

# ------------------------------------------------------------
# 3. American option
# ------------------------------------------------------------

equity_american <- function(env) {
  process <- ql_make_bsm_process(env$spot, env$div, env$rf, env$vol)
  engine <- ql_make_fd_engine(process)

  payoff <- PlainVanillaPayoff("Call", env$strike)
  exercise <- AmericanExercise(ql_date(env$eval_date), ql_date(env$maturity))

  opt <- VanillaOption(payoff, exercise)
  ql_set_engine(opt, engine)

  tibble(
    type = "American Call",
    npv = ql_option_npv(opt)
  )
}
