# ============================================================
# chapter07_normal_model_rewrite.R
# ------------------------------------------------------------
# 第7章 Normal model / Bachelier model
# - futures option under normal model
# - implied normal volatility
# - SOFR curve via QuantiveRiemann
# - Term SOFR style curve for cap pricing
# - Bachelier cap pricing
# - forward swap rate / annuity
# - Bachelier swaption pricing
# - closure-based / class-like normal calculator
# - PhiTilde / inversePhiTilde / normalVol inversion
# ============================================================

# ------------------------------------------------------------
# 0. setup
# ------------------------------------------------------------

.libPaths()
.libPaths(c(
  "/Library/Frameworks/R.framework/Versions/4.5-arm64/Resources/library",
  .libPaths()
))
.libPaths()

suppressPackageStartupMessages({
  library(QuantLib)
  library(ggplot2)
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(tibble)
  devtools::load_all(".")
})

# ------------------------------------------------------------
# 0. local helpers
# ------------------------------------------------------------

fmt_num <- function(x, digits = 6) {
  sprintf(paste0("%.", digits, "f"), x)
}

fmt_pct <- function(x, digits = 4) {
  sprintf(paste0("%.", digits, "f%%"), 100 * x)
}

qlr_curve_table <- function(curve_obj, n = 12, extrapolate = TRUE) {
  if (extrapolate) {
    tryCatch(
      TermStructure_enableExtrapolation(curve_obj),
      error = function(e) NULL
    )
  }

  ref_date <- tryCatch(
    as.Date(qlr_iso(curve_obj$referenceDate())),
    error = function(e) as.Date(NA)
  )

  max_time <- tryCatch(curve_obj$maxTime(), error = function(e) 3)

  tibble(
    time = seq(0, max_time, length.out = n)
  ) |>
    mutate(
      discount = map_dbl(
        time,
        ~ tryCatch(curve_obj$discount(.x), error = function(e) NA_real_)
      ),
      zero = if_else(time > 0, -log(discount) / time, 0),
      curve_date = ref_date + round(time * 365)
    )
}

make_schedule_tbl <- function(schedule_obj) {
  tibble(
    schedule_date = map_chr(
      seq_len(schedule_obj$size()),
      ~ qlr_iso(qlr_schedule_date_at(schedule_obj, .x))
    )
  )
}

schedule_date_vec <- function(schedule_obj) {
  map(
    seq_len(schedule_obj$size()),
    ~ qlr_schedule_date_at(schedule_obj, .x)
  )
}

safe_discount <- function(curve_obj, x) {
  tryCatch(curve_obj$discount(x), error = function(e) NA_real_)
}

normal_option_price <- function(
    option_sign,
    forward,
    strike,
    vol,
    maturity,
    discount_factor
) {
  if (vol <= 0 || maturity <= 0) {
    return(discount_factor * max(option_sign * (forward - strike), 0))
  }

  sd <- vol * sqrt(maturity)

  if (abs(sd) < 1e-15) {
    return(discount_factor * max(option_sign * (forward - strike), 0))
  }

  d <- option_sign * (forward - strike) / sd

  discount_factor * sd * (d * pnorm(d) + dnorm(d))
}

normal_option_greeks <- function(
    option_sign,
    forward,
    strike,
    vol,
    maturity,
    discount_factor,
    risk_free_rate
) {
  sd <- vol * sqrt(maturity)
  d <- option_sign * (forward - strike) / sd

  npv <- discount_factor * sd * (d * pnorm(d) + dnorm(d))
  delta <- option_sign * discount_factor * pnorm(d)
  gamma <- discount_factor * dnorm(d) / sd
  vega <- discount_factor * sqrt(maturity) * dnorm(d)
  theta <- risk_free_rate * npv - 0.5 * discount_factor * dnorm(d) * vol / sqrt(maturity)

  tibble(
    metric = c("npv", "delta", "gamma", "vega", "theta"),
    value = c(npv, delta, gamma, vega, theta)
  )
}

calc_annuity_from_schedule <- function(schedule_obj, curve_obj, day_counter) {
  date_vec <- schedule_date_vec(schedule_obj)

  if (length(date_vec) < 2) {
    return(NA_real_)
  }

  map_dbl(
    seq_len(length(date_vec) - 1L),
    function(i) {
      accrual <- tryCatch(
        day_counter$yearFraction(date_vec[[i]], date_vec[[i + 1L]]),
        error = function(e) NA_real_
      )
      df_pay <- safe_discount(curve_obj, date_vec[[i + 1L]])
      accrual * df_pay
    }
  ) |>
    sum(na.rm = TRUE)
}

calc_forward_swap_rate <- function(schedule_obj, curve_obj, day_counter) {
  date_vec <- schedule_date_vec(schedule_obj)

  annuity <- calc_annuity_from_schedule(schedule_obj, curve_obj, day_counter)
  df_start <- safe_discount(curve_obj, date_vec[[1]])
  df_end <- safe_discount(curve_obj, date_vec[[length(date_vec)]])

  (df_start - df_end) / annuity
}

make_normal_calculator <- function(payoff_obj, maturity_date, forward, vol, rf_curve_obj) {
  trade_date_inner <- Settings_instance()$evaluationDate()
  maturity_inner <- Actual365Fixed()$yearFraction(trade_date_inner, maturity_date)
  option_sign_inner <- ifelse(payoff_obj$optionType() == "Put", -1, 1)
  strike_inner <- payoff_obj$strike()
  maturity_df_inner <- rf_curve_obj$discount(maturity_date)

  function(forward_new = forward, vol_new = vol, maturity_new = maturity_inner) {
    discount_factor <- if (abs(maturity_new - maturity_inner) < 1e-15) {
      maturity_df_inner
    } else {
      tryCatch(rf_curve_obj$discount(maturity_new), error = function(e) NA_real_)
    }

    normal_option_price(
      option_sign = option_sign_inner,
      forward = forward_new,
      strike = strike_inner,
      vol = vol_new,
      maturity = maturity_new,
      discount_factor = discount_factor
    )
  }
}

normal_calculator <- function(payoff_obj, maturity_date, forward, vol, rf_curve_obj) {
  npv_fun <- make_normal_calculator(
    payoff_obj = payoff_obj,
    maturity_date = maturity_date,
    forward = forward,
    vol = vol,
    rf_curve_obj = rf_curve_obj
  )

  structure(
    list(
      npv = npv_fun
    ),
    class = "qlr_normal_calculator"
  )
}

phi_tilde <- function(x) {
  pnorm(x) + dnorm(x) / x
}

inverse_phi_tilde <- function(phi_tilde_star) {
  if (phi_tilde_star < -0.001882039271) {
    g <- 1.0 / (phi_tilde_star - 0.5)

    xibar <- (
      0.032114372355 -
        g * g * (
          0.016969777977 -
            g * g * (2.6207332461e-3 - 9.6066952861e-5 * g * g)
        )
    ) / (
      1.0 -
        g * g * (
          0.6635646938 -
            g * g * (0.14528712196 - 0.010472855461 * g * g)
        )
    )

    xbar <- g * (0.3989422804014326 + xibar * g * g)
  } else {
    h <- sqrt(-log(-phi_tilde_star))

    xbar <- (
      9.4883409779 -
        h * (9.6320903635 - h * (0.58556997323 + 2.1464093351 * h))
    ) / (
      1.0 -
        h * (0.65174820867 + h * (1.5120247828 + 6.6437847132e-5 * h))
    )
  }

  q <- (phi_tilde(xbar) - phi_tilde_star) / dnorm(xbar)

  xbar + 3.0 * q * xbar * xbar * (2.0 - q * xbar * (2.0 + xbar * xbar)) /
    (
      6.0 +
        q * xbar * (
          -12.0 +
            xbar * (
              6.0 * q +
                xbar * (-6.0 + q * xbar * (3.0 + xbar * xbar))
            )
        )
    )
}

normal_vol_from_price <- function(
    option_sign,
    strike,
    forward,
    maturity,
    option_npv,
    discount_factor
) {
  option_npv_adj <- option_npv / discount_factor

  if (abs(strike - forward) < 1e-15) {
    return(option_npv_adj / (sqrt(maturity) * dnorm(0)))
  }

  time_value <- option_npv_adj - max(option_sign * (forward - strike), 0)

  if (abs(time_value) < 1e-15) {
    return(0)
  }

  phi_tilde_star <- -abs(time_value / (strike - forward))
  x_star <- inverse_phi_tilde(phi_tilde_star)

  abs((strike - forward) / (x_star * sqrt(maturity)))
}

# ------------------------------------------------------------
# 1. Normal model for futures option
# ------------------------------------------------------------

trade_date <- qlr_date("2023-09-26")
maturity_date <- qlr_date("2023-12-15")
qlr_set_eval_date(trade_date)

option_sign <- -1
futures_price <- 94.54
strike_price_fut <- 94.50

vol <- 50 / 10000
risk_free_rate <- 5.4 / 100
forward_rate <- (100 - futures_price) / 100
strike_rate <- (100 - strike_price_fut) / 100

dc_a365 <- Actual365Fixed()
dc_a360 <- Actual360()

rf_curve_obj <- FlatForward(
  trade_date,
  risk_free_rate,
  dc_a360,
  "Simple"
)

maturity_year_fraction <- dc_a365$yearFraction(trade_date, maturity_date)
std_dev <- vol * sqrt(maturity_year_fraction)
maturity_df <- rf_curve_obj$discount(maturity_date)

normal_greeks_tbl <- normal_option_greeks(
  option_sign = option_sign,
  forward = forward_rate,
  strike = strike_rate,
  vol = vol,
  maturity = maturity_year_fraction,
  discount_factor = maturity_df,
  risk_free_rate = risk_free_rate
)

normal_npv <- normal_greeks_tbl |>
  filter(metric == "npv") |>
  pull(value)

normal_summary_tbl <- tibble(
  metric = c(
    "option_npv",
    "delivery_amount",
    "delta",
    "gamma",
    "vega",
    "theta"
  ),
  value = c(
    normal_npv,
    normal_npv * 100 * 2500,
    normal_greeks_tbl$value[normal_greeks_tbl$metric == "delta"],
    normal_greeks_tbl$value[normal_greeks_tbl$metric == "gamma"],
    normal_greeks_tbl$value[normal_greeks_tbl$metric == "vega"],
    normal_greeks_tbl$value[normal_greeks_tbl$metric == "theta"]
  )
)

qlr_show_tbl(
  normal_summary_tbl,
  "Normal futures option summary",
  n = 20
)

# ------------------------------------------------------------
# 2. Implied normal volatility by root finding
# ------------------------------------------------------------

target_npv <- 0.1133 / 100

vol_solver <- function(vol_guess) {
  target_npv - normal_option_price(
    option_sign = option_sign,
    forward = forward_rate,
    strike = strike_rate,
    vol = vol_guess,
    maturity = maturity_year_fraction,
    discount_factor = maturity_df
  )
}

implied_vol <- uniroot(
  f = vol_solver,
  interval = c(5e-5, 0.1),
  tol = 1e-5
)$root

implied_vol_tbl <- tibble(
  metric = c("target_npv", "implied_normal_vol"),
  value = c(target_npv, implied_vol)
)

qlr_show_tbl(
  implied_vol_tbl,
  "Implied normal volatility",
  n = 20
)

# ------------------------------------------------------------
# 3. SOFR OIS curve via QuantiveRiemann
# ------------------------------------------------------------

trade_date_sofr <- "2023-09-26"

sofr_quotes <- tibble::tribble(
  ~kind,  ~tenor, ~rate_pct,
  "depo", "1d",   5.31,
  "ois",  "1m",   5.32,
  "ois",  "3m",   5.38,
  "ois",  "6m",   5.46,
  "ois",  "1y",   5.45,
  "ois",  "2y",   5.01,
  "ois",  "3y",   4.67
) |>
  mutate(
    as_of_date = as.Date("2023-09-26"),
    currency = "USD",
    instrument = "SOFR",
    rate = rate_pct / 100
  ) |>
  select(
    as_of_date,
    currency,
    instrument,
    kind,
    tenor,
    rate
  )

qlr_show_tbl(
  sofr_quotes,
  "SOFR input quotes",
  n = 20
)

sofr_bundle_list <- qlr_ir_build_ois_curve_envs(
  quotes = sofr_quotes,
  trade_date = trade_date_sofr,
  verbose = TRUE
)

sofr_bundle <- sofr_bundle_list[[1]]

sofr_curve_tbl <- qlr_curve_table(sofr_bundle$curve, n = 12)

qlr_show_tbl(
  sofr_curve_tbl,
  "SOFR curve table",
  n = 20
)
qlr_schedule_date_at_local <- function(schedule_obj, i_one_based) {
  idx0 <- as.integer(i_one_based - 1L)

  out <- tryCatch(schedule_obj$date(idx0), error = function(e) NULL)
  if (!is.null(out)) return(out)

  out <- tryCatch(schedule_obj$get(idx0), error = function(e) NULL)
  if (!is.null(out)) return(out)

  out <- tryCatch(schedule_obj[[i_one_based]][[1]], error = function(e) NULL)
  if (!is.null(out)) return(out)

  stop("Unable to access schedule date at index ", i_one_based)
}

make_schedule_tbl <- function(schedule_obj) {
  tibble::tibble(
    schedule_date = purrr::map_chr(
      seq_len(schedule_obj$size()),
      ~ qlr_iso(qlr_schedule_date_at_local(schedule_obj, .x))
    )
  )
}

schedule_date_vec <- function(schedule_obj) {
  purrr::map(
    seq_len(schedule_obj$size()),
    ~ qlr_schedule_date_at_local(schedule_obj, .x)
  )
}
# ------------------------------------------------------------
# 4. Term SOFR style curve for cap pricing
# ------------------------------------------------------------

qlr_set_eval_date(qlr_date("2023-09-26"))

term_sofr_3m <- 5.38558 / 100
term_sofr_basis_data <- tibble::tribble(
  ~tenor, ~spread,
  "6M",   0.0,
  "1Y",   0.0,
  "2Y",   0.0,
  "3Y",   0.0
)

term_sofr_basis_env <- qlr_ir_make_basis_curve(
  basis_data = term_sofr_basis_data,
  base_curve_env = sofr_bundle,
  spread_label = "term_sofr_basis",
  verbose = TRUE
)

term_sofr_basis_tbl <- qlr_ir_basis_table(
  term_sofr_basis_env,
  tenors = c("3M", "6M", "1Y", "2Y", "3Y")
)

qlr_show_tbl(
  tibble(
    metric = c("term_sofr_3m", "status"),
    value = c(term_sofr_3m, "ok")
  ),
  "Term SOFR style basis summary",
  n = 20
)

qlr_show_tbl(
  term_sofr_basis_tbl,
  "Term SOFR style basis table",
  n = 20
)

# explicit Term SOFR curve for cap section
term_sofr_curve_handle <- RelinkableYieldTermStructureHandle()

term_sofr_index <- IborIndex(
  "TermSofr",
  qlr_period_months(3),
  2L,
  USDCurrency(),
  UnitedStates("Settlement"),
  "ModifiedFollowing",
  FALSE,
  dc_a360,
  term_sofr_curve_handle
)

term_sofr_helpers <- RateHelperVector()

RateHelperVector_append(
  term_sofr_helpers,
  DepositRateHelper(
    QuoteHandle(SimpleQuote(term_sofr_3m)),
    term_sofr_index
  )
)

walk(
  seq_len(nrow(term_sofr_basis_data)),
  function(i) {
    RateHelperVector_append(
      term_sofr_helpers,
      OvernightIborBasisSwapRateHelper(
        QuoteHandle(SimpleQuote(term_sofr_basis_data$spread[i])),
        qlr_period(term_sofr_basis_data$tenor[i]),
        2L,
        UnitedStates("Settlement"),
        "ModifiedFollowing",
        FALSE,
        sofr_bundle$index,
        term_sofr_index,
        sofr_bundle$curve_handle
      )
    )
  }
)

term_sofr_curve <- PiecewiseLogLinearDiscount(
  2L,
  UnitedStates("Settlement"),
  term_sofr_helpers,
  dc_a360
)

RelinkableYieldTermStructureHandle_linkTo(term_sofr_curve_handle, term_sofr_curve)
TermStructure_enableExtrapolation(term_sofr_curve)

term_sofr_curve_tbl <- qlr_curve_table(term_sofr_curve, n = 10)

qlr_show_tbl(
  term_sofr_curve_tbl,
  "Term SOFR curve table",
  n = 20
)

# ------------------------------------------------------------
# 5. Cap pricing under Bachelier
# ------------------------------------------------------------

cap_effective_date <- qlr_date("2023-09-28")
cap_maturity_date <- qlr_date("2024-09-28")
cap_strike <- 0.05
notional <- 10000000
cap_vol <- 0.88 / 100

cap_schedule <- Schedule(
  cap_effective_date,
  cap_maturity_date,
  qlr_period_months(3),
  UnitedStates("Settlement"),
  "ModifiedFollowing",
  "ModifiedFollowing",
  "Backward",
  FALSE
)

qlr_show_tbl(
  make_schedule_tbl(cap_schedule),
  "Cap schedule",
  n = 20
)

cap_leg <- IborLeg(
  c(notional),
  cap_schedule,
  term_sofr_index,
  dc_a360
)

cap_obj <- Cap(cap_leg, c(cap_strike))
cap_engine <- BachelierCapFloorEngine(
  sofr_bundle$curve_handle,
  QuoteHandle(SimpleQuote(cap_vol))
)
cap_obj$setPricingEngine(cap_engine)

cap_summary_tbl <- tibble(
  metric = c("cap_npv"),
  value = c(cap_obj$NPV())
)

qlr_show_tbl(
  cap_summary_tbl,
  "Cap pricing summary",
  n = 20
)

caplet_tbl <- tibble(
  std_dev = as.numeric(cap_obj$optionletsStdDev()),
  atm_forward = as.numeric(cap_obj$optionletsAtmForward()),
  discount_factor = as.numeric(cap_obj$optionletsDiscountFactor()),
  npv = as.numeric(cap_obj$optionletsPrice())
)

cap_coupon_tbl <- tibble(
  coupon = map(
    seq_len(cap_leg$size()),
    ~ as_floating_rate_coupon(qlr_leg_cashflow_at(cap_leg, .x))
  )
) |>
  mutate(
    maturity_year = map_dbl(
      coupon,
      ~ dc_a365$yearFraction(trade_date, FloatingRateCoupon_fixingDate(.x))
    ),
    fixing_date = map_chr(coupon, ~ qlr_iso(FloatingRateCoupon_fixingDate(.x))),
    accrual_start = map_chr(coupon, ~ qlr_iso(Coupon_accrualStartDate(.x))),
    accrual_end = map_chr(coupon, ~ qlr_iso(Coupon_accrualEndDate(.x))),
    pay_date = map_chr(coupon, ~ qlr_iso(CashFlow_date(.x))),
    days = map_dbl(
      coupon,
      ~ dc_a360$dayCount(Coupon_accrualStartDate(.x), Coupon_accrualEndDate(.x))
    ),
    term_sofr_df = map_dbl(coupon, ~ term_sofr_curve$discount(CashFlow_date(.x)))
  ) |>
  select(
    maturity_year,
    fixing_date,
    accrual_start,
    accrual_end,
    pay_date,
    days,
    term_sofr_df
  ) |>
  bind_cols(caplet_tbl)

qlr_show_tbl(
  cap_coupon_tbl,
  "Caplet decomposition",
  n = 20
)

# ------------------------------------------------------------
# 6. Forward swap annuity and forward swap rate
# ------------------------------------------------------------

swap_effective_date <- qlr_date("2024-09-30")
swap_maturity_date <- qlr_date("2026-09-30")

fixed_schedule <- Schedule(
  swap_effective_date,
  swap_maturity_date,
  qlr_period_years(1),
  UnitedStates("Settlement"),
  "ModifiedFollowing",
  "ModifiedFollowing",
  "Backward",
  FALSE
)

fixed_dates <- schedule_date_vec(fixed_schedule)
fixed_dfs <- map_dbl(fixed_dates, ~ sofr_bundle$curve$discount(.x))

qlr_show_tbl(
  tibble(
    schedule_date = map_chr(fixed_dates, qlr_iso),
    discount_factor = fixed_dfs
  ),
  "Underlying swap fixed schedule / DFs",
  n = 20
)

annuity_value <- calc_annuity_from_schedule(
  fixed_schedule,
  sofr_bundle$curve,
  dc_a360
)

forward_swap_rate <- calc_forward_swap_rate(
  fixed_schedule,
  sofr_bundle$curve,
  dc_a360
)

swap_forward_tbl <- tibble(
  metric = c("annuity", "forward_swap_rate"),
  value = c(annuity_value, forward_swap_rate)
)

qlr_show_tbl(
  swap_forward_tbl,
  "Forward swap summary",
  n = 20
)

# ------------------------------------------------------------
# 7. Bachelier swaption pricing
# ------------------------------------------------------------

exercise_date <- qlr_date("2024-09-26")
swaption_vol <- 1.35 / 100
swaption_notional <- 10000000

underlying_swap <- OvernightIndexedSwap(
  Swap_Payer_get(),
  swaption_notional,
  fixed_schedule,
  forward_swap_rate,
  dc_a360,
  sofr_bundle$index,
  0.0,
  0L
)
ls("package:QuantLib", pattern = "Swap_")
swaption_obj <- Swaption(
  underlying_swap,
  EuropeanExercise(exercise_date)
)

swaption_engine <- BachelierSwaptionEngine(
  sofr_bundle$curve_handle,
  QuoteHandle(SimpleQuote(swaption_vol))
)

swaption_obj$setPricingEngine(swaption_engine)

swaption_tbl <- tibble(
  metric = c("npv", "delta_per_notional", "vega_per_notional", "annuity_per_notional"),
  value = c(
    swaption_obj$NPV(),
    swaption_obj$delta() / swaption_notional,
    swaption_obj$vega() / swaption_notional,
    swaption_obj$annuity() / swaption_notional
  )
)

qlr_show_tbl(
  swaption_tbl,
  "Bachelier swaption summary",
  n = 20
)
# ------------------------------------------------------------
# 8. Closure-based normal calculator
# ------------------------------------------------------------

make_normal_calculator <- function(
    option_sign,
    strike,
    maturity,
    discount_factor,
    forward,
    vol
) {
  function(forward_new = forward, vol_new = vol, maturity_new = maturity, discount_factor_new = discount_factor) {
    normal_option_price(
      option_sign = option_sign,
      forward = forward_new,
      strike = strike,
      vol = vol_new,
      maturity = maturity_new,
      discount_factor = discount_factor_new
    )
  }
}

normal_calculator <- function(
    option_sign,
    strike,
    maturity,
    discount_factor,
    forward,
    vol
) {
  npv_fun <- make_normal_calculator(
    option_sign = option_sign,
    strike = strike,
    maturity = maturity,
    discount_factor = discount_factor,
    forward = forward,
    vol = vol
  )

  structure(
    list(
      npv = npv_fun
    ),
    class = "qlr_normal_calculator"
  )
}

normal_calc_fn <- make_normal_calculator(
  option_sign = -1,
  strike = strike_rate,
  maturity = maturity_year_fraction,
  discount_factor = maturity_df,
  forward = forward_rate,
  vol = vol
)

forward_up <- forward_rate * 1.01
vol_up <- vol + 0.01

trade_date_plus_1 <- qlr_date("2023-09-27")
maturity_1d <- dc_a365$yearFraction(trade_date_plus_1, maturity_date)
discount_factor_1d <- exp(-risk_free_rate * maturity_1d)

closure_tbl <- tibble(
  scenario = c("base", "forward_up_1pct", "vol_up_1pct_abs", "one_day_passed"),
  npv = c(
    normal_calc_fn(),
    normal_calc_fn(forward_new = forward_up),
    normal_calc_fn(vol_new = vol_up),
    normal_calc_fn(
      maturity_new = maturity_1d,
      discount_factor_new = discount_factor_1d
    )
  )
)

qlr_show_tbl(
  closure_tbl,
  "Closure-based normal calculator",
  n = 20
)

# ------------------------------------------------------------
# 9. Class-like normal calculator
# ------------------------------------------------------------

normal_calc_obj <- normal_calculator(
  option_sign = -1,
  strike = strike_rate,
  maturity = maturity_year_fraction,
  discount_factor = maturity_df,
  forward = forward_rate,
  vol = vol
)

class_tbl <- tibble(
  scenario = c("base", "forward_up_1pct", "vol_up_1pct_abs", "one_day_passed"),
  npv = c(
    normal_calc_obj$npv(),
    normal_calc_obj$npv(forward_new = forward_up),
    normal_calc_obj$npv(vol_new = vol_up),
    normal_calc_obj$npv(
      maturity_new = maturity_1d,
      discount_factor_new = discount_factor_1d
    )
  )
)

qlr_show_tbl(
  class_tbl,
  "Class-like normal calculator",
  n = 20
)

# ------------------------------------------------------------
# 10. Normal vol inversion
# ------------------------------------------------------------

normal_vol_check <- normal_vol_from_price(
  option_sign = -1,
  strike = 0.055,
  forward = 0.0546,
  maturity = 0.21918,
  option_npv = 0.0011330,
  discount_factor = 0.98814
)

normal_vol_tbl <- tibble(
  metric = c("normal_vol_from_price"),
  value = c(normal_vol_check)
)

qlr_show_tbl(
  normal_vol_tbl,
  "Normal vol inversion",
  n = 20
)

cat("\nchapter07 normal model rewrite completed successfully.\n")
