# ============================================================
# chapter09_callable_hullwhite_rewrite.R
# ------------------------------------------------------------
# 第9章 Callable bond / Hull-White / Bermudan swaption
#
# Part 1
# 1. 5Y 3% bullet bond
# 2. Callable bond under Hull-White tree
# 3. Bermudan swaption under Hull-White tree
# 4. Compare callable bond price with 100 - swaption value
#
# Part 2
# 5. Vasicek curve illustration
# 6. Hull-White curve illustration
#
# Part 3
# 7. JPY Tibor-like curve build
# 8. Hull-White one-helper calibration
# 9. 1Yx5Y swaption engine comparison
# 10. Hull-White multi-helper calibration
# 11. Bermudan swaption under calibrated Hull-White
#
# Part 4A
# 12. One-step tree hand calculation
# 13. Compare hand calc with Part 3 engine outputs
#
# Part 4B
# 14. Tree-step convergence vs hand calculation
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
  library(dplyr)
  library(purrr)
  library(tibble)
  library(ggplot2)
  devtools::load_all(".")
})

# ------------------------------------------------------------
# 1. local helpers
# ------------------------------------------------------------

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
  tibble(
    schedule_date = map_chr(
      seq_len(schedule_obj$size()),
      ~ qlr_iso(qlr_schedule_date_at_local(schedule_obj, .x))
    )
  )
}

schedule_date_vec <- function(schedule_obj) {
  map(
    seq_len(schedule_obj$size()),
    ~ qlr_schedule_date_at_local(schedule_obj, .x)
  )
}

ql_date_vector <- function(date_list) {
  dv <- DateVector()

  purrr::walk(
    date_list,
    function(dd) {
      DateVector_append(dv, dd)
    }
  )

  dv
}

make_swaption_helper_vector <- function(helper_list) {
  vec <- CalibrationHelperVector()

  purrr::walk(
    helper_list,
    function(hh) {
      CalibrationHelperVector_append(vec, hh)
    }
  )

  vec
}

bond_clean_price_from_yield_safe <- function(
    bond_obj,
    yld,
    day_counter,
    compounding,
    frequency,
    settlement_date = NULL
) {
  if (is.null(settlement_date)) {
    tryCatch(
      bond_obj$cleanPrice(yld, day_counter, compounding, frequency),
      error = function(e) {
        tryCatch(
          bond_obj$cleanPrice(
            yld,
            day_counter,
            compounding,
            frequency,
            bond_obj$settlementDate()
          ),
          error = function(e2) NA_real_
        )
      }
    )
  } else {
    tryCatch(
      bond_obj$cleanPrice(yld, day_counter, compounding, frequency, settlement_date),
      error = function(e) NA_real_
    )
  }
}

safe_npv <- function(obj) {
  tryCatch(obj$NPV(), error = function(e) NA_real_)
}

safe_clean_price <- function(obj) {
  out <- tryCatch(obj$cleanPrice(), error = function(e) NULL)
  if (!is.null(out)) return(out)

  out <- tryCatch(obj$NPV(), error = function(e) NULL)
  if (!is.null(out)) return(out)

  out <- tryCatch(obj$dirtyPrice(), error = function(e) NULL)
  if (!is.null(out)) return(out)

  out <- tryCatch(obj$settlementValue(), error = function(e) NULL)
  if (!is.null(out)) return(out)

  NA_real_
}

safe_dirty_price <- function(obj) {
  tryCatch(obj$dirtyPrice(), error = function(e) NA_real_)
}

safe_settlement_value <- function(obj) {
  tryCatch(obj$settlementValue(), error = function(e) NA_real_)
}

safe_flat_curve <- function(reference_date, rate, day_counter, compounding = "Compounded") {
  FlatForward(
    reference_date,
    rate,
    day_counter,
    compounding
  )
}

safe_callable_schedule <- function(call_dates, call_price_clean = 100) {
  call_schedule <- CallabilitySchedule()

  call_price <- BondPrice(
    call_price_clean,
    BondPrice_Clean_get()
  )

  purrr::walk(
    call_dates,
    function(dd) {
      callability_obj <- Callability(
        call_price,
        Callability_Call_get(),
        dd
      )
      CallabilitySchedule_append(call_schedule, callability_obj)
    }
  )

  call_schedule
}

# ------------------------------------------------------------
# 2. Part 3 first: JPY Tibor-like curve build
# ------------------------------------------------------------

trade_date_calib <- qlr_date("2022-08-19")
calendar_jp <- Japan()
dc_a365 <- Actual365Fixed()

jpy_curve_quotes <- tibble::tribble(
  ~kind,  ~tenor, ~rate_pct,
  "depo", "6m",   0.13636,
  "swap", "1y",   0.15249,
  "swap", "18m",  0.18742,
  "swap", "2y",   0.20541,
  "swap", "3y",   0.23156,
  "swap", "4y",   0.25653,
  "swap", "5y",   0.28528,
  "swap", "6y",   0.32341,
  "swap", "7y",   0.36591,
  "swap", "8y",   0.40906,
  "swap", "9y",   0.45471,
  "swap", "10y",  0.50224
) |>
  mutate(rate = rate_pct / 100)

qlr_show_tbl(
  jpy_curve_quotes,
  "JPY Tibor-like input quotes",
  n = 20
)

qlr_set_eval_date(trade_date_calib)

jpy_curve_helpers <- RateHelperVector()

RateHelperVector_append(
  jpy_curve_helpers,
  DepositRateHelper(
    QuoteHandle(SimpleQuote(
      jpy_curve_quotes |>
        filter(kind == "depo", tenor == "6m") |>
        pull(rate)
    )),
    qlr_period_months(6),
    2L,
    calendar_jp,
    "ModifiedFollowing",
    FALSE,
    dc_a365
  )
)

swap_rows <- jpy_curve_quotes |>
  filter(kind == "swap")

purrr::walk(
  seq_len(nrow(swap_rows)),
  function(i) {
    RateHelperVector_append(
      jpy_curve_helpers,
      SwapRateHelper(
        QuoteHandle(SimpleQuote(swap_rows$rate[i])),
        qlr_period(swap_rows$tenor[i]),
        calendar_jp,
        Frequency_Semiannual_get(),
        "ModifiedFollowing",
        dc_a365,
        JPYLibor(qlr_period_months(6))
      )
    )
  }
)

jpy_curve_obj <- PiecewiseLogLinearDiscount(
  2L,
  calendar_jp,
  jpy_curve_helpers,
  dc_a365
)

TermStructure_enableExtrapolation(jpy_curve_obj)
jpy_curve_handle <- YieldTermStructureHandle(jpy_curve_obj)

jpy_index <- JPYLibor(
  qlr_period_months(6),
  jpy_curve_handle
)

curve_check_tbl <- tibble(
  maturity = seq(0.5, 10, by = 0.5),
  discount = map_dbl(seq(0.5, 10, by = 0.5), ~ jpy_curve_obj$discount(.x)),
  zero = map_dbl(seq(0.5, 10, by = 0.5), ~ -log(jpy_curve_obj$discount(.x)) / .x)
)

qlr_show_tbl(
  curve_check_tbl,
  "JPY Tibor-like curve check",
  n = 20
)

# ------------------------------------------------------------
# 3. Part 1: bullet / callable / bermudan
# ------------------------------------------------------------

trade_date <- qlr_date("2024-04-15")
issue_date <- qlr_date("2024-04-17")
maturity_date <- qlr_date("2029-04-17")
qlr_set_eval_date(trade_date)

coupon_rate <- 0.03
market_yield <- 0.03

settlement_days <- 2L
par_amount <- 100
par_price <- 100

calendar_obj <- WeekendsOnly()
day_counter_bond <- ActualActual("Bond")

compounding_cmp <- Compounding_Compounded_get()
freq_semiannual <- Frequency_Semiannual_get()

schedule_bond <- Schedule(
  issue_date,
  maturity_date,
  qlr_period_months(6),
  calendar_obj,
  "Unadjusted",
  "Unadjusted",
  "Backward",
  FALSE
)

qlr_show_tbl(
  make_schedule_tbl(schedule_bond),
  "Bullet bond schedule",
  n = 20
)

bullet_bond <- FixedRateBond(
  settlement_days,
  par_amount,
  schedule_bond,
  c(coupon_rate),
  day_counter_bond
)

bullet_clean_price <- bond_clean_price_from_yield_safe(
  bullet_bond,
  market_yield,
  day_counter_bond,
  compounding_cmp,
  freq_semiannual
)

bullet_summary_tbl <- tibble(
  metric = c(
    "coupon_rate",
    "market_yield_input",
    "clean_price_from_market_yield"
  ),
  value = c(
    coupon_rate,
    market_yield,
    bullet_clean_price
  )
)

qlr_show_tbl(
  bullet_summary_tbl,
  "Bullet bond summary",
  n = 20
)

nc_periods <- 2L
swap_spread <- 0.0
hw_a <- 0.03
hw_sigma <- 0.01
n_step_tree <- 12L * 5L

flat_curve_obj <- safe_flat_curve(
  reference_date = qlr_date("2024-04-17"),
  rate = market_yield,
  day_counter = day_counter_bond,
  compounding = "Compounded"
)

flat_curve_handle <- YieldTermStructureHandle(flat_curve_obj)

float_index <- Libor(
  "ffIX",
  qlr_period_months(6),
  settlement_days,
  JPYCurrency(),
  calendar_obj,
  day_counter_bond,
  flat_curve_handle
)

tryCatch(
  float_index$addFixing(trade_date, market_yield, TRUE),
  error = function(e) NULL
)

hw_model <- HullWhite(
  flat_curve_handle,
  hw_a,
  hw_sigma
)

hw_tbl <- tibble(
  metric = c("a", "sigma", "n_step_tree"),
  value = c(hw_a, hw_sigma, n_step_tree)
)

qlr_show_tbl(
  hw_tbl,
  "Hull-White parameters",
  n = 20
)

call_dates <- schedule_date_vec(schedule_bond)[
  (nc_periods + 1L):length(schedule_date_vec(schedule_bond))
]

call_schedule <- safe_callable_schedule(
  call_dates = call_dates,
  call_price_clean = par_price
)

callable_bond <- CallableFixedRateBond(
  settlement_days,
  par_amount,
  schedule_bond,
  c(coupon_rate),
  day_counter_bond,
  "Unadjusted",
  par_price,
  issue_date,
  call_schedule
)

callable_engine <- TreeCallableFixedRateBondEngine(
  hw_model,
  n_step_tree
)

callable_bond$setPricingEngine(callable_engine)

callable_price_proxy <- safe_clean_price(callable_bond)
callable_dirty_price <- safe_dirty_price(callable_bond)
callable_settlement_value <- safe_settlement_value(callable_bond)

callable_summary_tbl <- tibble(
  metric = c(
    "callable_price_proxy",
    "callable_dirty_price",
    "callable_settlement_value",
    "embedded_option_value_vs_100"
  ),
  value = c(
    callable_price_proxy,
    callable_dirty_price,
    callable_settlement_value,
    par_price - callable_price_proxy
  )
)

qlr_show_tbl(
  callable_summary_tbl,
  "Callable bond summary",
  n = 20
)

underlying_swap <- VanillaSwap(
  Swap_Payer_get(),
  par_amount,
  schedule_bond,
  coupon_rate,
  day_counter_bond,
  schedule_bond,
  float_index,
  swap_spread,
  day_counter_bond
)

bermudan_exercise_dates <- ql_date_vector(call_dates)
bermudan_exercise <- BermudanExercise(bermudan_exercise_dates)

bermudan_swaption <- Swaption(
  underlying_swap,
  bermudan_exercise
)

tree_swaption_engine <- TreeSwaptionEngine(
  hw_model,
  n_step_tree
)

bermudan_swaption$setPricingEngine(tree_swaption_engine)

bermudan_swaption_npv <- safe_npv(bermudan_swaption)

swaption_summary_tbl <- tibble(
  metric = c(
    "bermudan_swaption_npv",
    "100_minus_swaption_value"
  ),
  value = c(
    bermudan_swaption_npv,
    par_price - bermudan_swaption_npv / par_amount * 100
  )
)

qlr_show_tbl(
  swaption_summary_tbl,
  "Bermudan swaption summary",
  n = 20
)

comparison_tbl <- tibble(
  metric = c(
    "callable_bond_price_proxy",
    "100_minus_bermudan_swaption_value",
    "difference"
  ),
  value = c(
    callable_price_proxy,
    par_price - bermudan_swaption_npv / par_amount * 100,
    callable_price_proxy - (par_price - bermudan_swaption_npv / par_amount * 100)
  )
)

qlr_show_tbl(
  comparison_tbl,
  "Callable bond vs Bermudan swaption comparison",
  n = 20
)

cat("\nchapter09 callable / hull-white rewrite part1 completed successfully.\n")

# ------------------------------------------------------------
# 4. Part 2: Vasicek / Hull-White curve illustration
# ------------------------------------------------------------

vasicek_B <- function(t, T, a = 0.03) {
  (1 - exp(-a * (T - t))) / a
}

vasicek_A <- function(t, T, a = 0.03, sigma = 0.01, theta = 0.05) {
  a2 <- a^2
  sigma2 <- sigma^2

  (sigma2 / (2 * a2)) * (T - t - vasicek_B(t, T, a)) -
    (sigma2 * vasicek_B(t, T, a)^2) / (4 * a) -
    theta * (T - t - vasicek_B(t, T, a))
}

vasicek_R <- function(t, T, theta = 0.05, a = 0.03, sigma = 0.01, r0 = 0.05) {
  if (abs(T - t) < 1e-12) return(r0)
  -(vasicek_A(t, T, a, sigma, theta) - r0 * vasicek_B(t, T, a)) / (T - t)
}

time_grid_vasicek <- seq(0, 10, by = 0.5)

vasicek_tbl <- bind_rows(
  tibble(
    maturity = time_grid_vasicek,
    rate = map_dbl(time_grid_vasicek, ~ vasicek_R(0, .x, theta = 0.04, a = 2.0, sigma = 0.08, r0 = 0.02)),
    panel = "a = 2.0",
    scenario = "r0 = 2.0%"
  ),
  tibble(
    maturity = time_grid_vasicek,
    rate = map_dbl(time_grid_vasicek, ~ vasicek_R(0, .x, theta = 0.04, a = 2.0, sigma = 0.08, r0 = 0.025)),
    panel = "a = 2.0",
    scenario = "r0 = 2.5%"
  ),
  tibble(
    maturity = time_grid_vasicek,
    rate = map_dbl(time_grid_vasicek, ~ vasicek_R(0, .x, theta = 0.04, a = 0.3, sigma = 0.08, r0 = 0.02)),
    panel = "a = 0.3",
    scenario = "r0 = 2.0%"
  ),
  tibble(
    maturity = time_grid_vasicek,
    rate = map_dbl(time_grid_vasicek, ~ vasicek_R(0, .x, theta = 0.04, a = 0.3, sigma = 0.08, r0 = 0.025)),
    panel = "a = 0.3",
    scenario = "r0 = 2.5%"
  )
)

qlr_show_tbl(
  vasicek_tbl,
  "Vasicek curve table",
  n = 20
)

vasicek_plot <- ggplot(
  vasicek_tbl,
  aes(x = maturity, y = rate, color = scenario, linetype = scenario)
) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ panel, nrow = 1) +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  scale_y_continuous(labels = \(x) sprintf("%.2f%%", 100 * x)) +
  labs(
    title = "Vasicek curves",
    x = "Residual maturity (years)",
    y = "Continuously compounded rate"
  ) +
  theme_minimal()

print(vasicek_plot)

hw_B <- function(t, T, a) {
  (1 - exp(-a * (T - t))) / a
}

hw_V <- function(t, T, a, sigma) {
  (sigma^2 / a^2) * (
    (T - t) +
      (2 / a) * exp(-a * (T - t)) -
      (1 / (2 * a)) * exp(-2 * a * (T - t)) -
      3 / (2 * a)
  )
}

hw_A <- function(t, T, a, sigma) {
  0.5 * (hw_V(t, T, a, sigma) - hw_V(0, T, a, sigma) + hw_V(0, t, a, sigma))
}

hw_R <- function(t, T, curve_obj, a = 0.03, sigma = 0.01, x0 = 0) {
  if (abs(T - t) < 1e-12) {
    return(tryCatch(-log(curve_obj$discount(0.0001)) / 0.0001 + x0, error = function(e) x0))
  }

  df_T <- tryCatch(curve_obj$discount(T), error = function(e) NA_real_)
  df_t <- tryCatch(curve_obj$discount(t), error = function(e) 1.0)

  -(log(df_T / df_t) + hw_A(t, T, a, sigma) - x0 * hw_B(t, T, a)) / (T - t)
}

time_grid_hw <- seq(0, 10, by = 0.5)

hw_curve_tbl <- bind_rows(
  tibble(
    maturity = time_grid_hw,
    rate = map_dbl(time_grid_hw, ~ hw_R(0, .x, jpy_curve_obj, a = 2.0, sigma = 0.08, x0 = 0.000)),
    panel = "a = 2.0",
    scenario = "base curve"
  ),
  tibble(
    maturity = time_grid_hw,
    rate = map_dbl(time_grid_hw, ~ hw_R(0, .x, jpy_curve_obj, a = 2.0, sigma = 0.08, x0 = -0.003)),
    panel = "a = 2.0",
    scenario = "x0 = -0.3%"
  ),
  tibble(
    maturity = time_grid_hw,
    rate = map_dbl(time_grid_hw, ~ hw_R(0, .x, jpy_curve_obj, a = 2.0, sigma = 0.08, x0 = 0.003)),
    panel = "a = 2.0",
    scenario = "x0 = 0.3%"
  ),
  tibble(
    maturity = time_grid_hw,
    rate = map_dbl(time_grid_hw, ~ hw_R(0, .x, jpy_curve_obj, a = 0.3, sigma = 0.08, x0 = 0.000)),
    panel = "a = 0.3",
    scenario = "base curve"
  ),
  tibble(
    maturity = time_grid_hw,
    rate = map_dbl(time_grid_hw, ~ hw_R(0, .x, jpy_curve_obj, a = 0.3, sigma = 0.08, x0 = -0.003)),
    panel = "a = 0.3",
    scenario = "x0 = -0.3%"
  ),
  tibble(
    maturity = time_grid_hw,
    rate = map_dbl(time_grid_hw, ~ hw_R(0, .x, jpy_curve_obj, a = 0.3, sigma = 0.08, x0 = 0.003)),
    panel = "a = 0.3",
    scenario = "x0 = 0.3%"
  )
)

qlr_show_tbl(
  hw_curve_tbl,
  "Hull-White curve table",
  n = 20
)

hw_plot <- ggplot(
  hw_curve_tbl,
  aes(x = maturity, y = rate, color = scenario, linetype = scenario)
) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ panel, nrow = 1) +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +
  scale_y_continuous(labels = \(x) sprintf("%.2f%%", 100 * x)) +
  labs(
    title = "Hull-White curves",
    x = "Residual maturity (years)",
    y = "Continuously compounded rate"
  ) +
  theme_minimal()

print(hw_plot)

cat("\nchapter09 callable / hull-white rewrite part2 completed successfully.\n")

# ------------------------------------------------------------
# 5. Part 3: Hull-White calibration and swaption checks
# ------------------------------------------------------------

qlr_set_eval_date(trade_date_calib)

# 5.1 One-helper setup: first build 1Yx5Y underlying and ATM strike

swap_effective_date <- qlr_date("2023-08-23")
swap_maturity_date <- qlr_date("2028-08-23")
exercise_date <- calendar_jp$advance(trade_date_calib, 1L, "Years")
maturity_tenor <- qlr_period_years(5L)

fixed_schedule_1y5y <- Schedule(
  swap_effective_date,
  swap_maturity_date,
  qlr_period_months(6),
  calendar_jp,
  "ModifiedFollowing",
  "ModifiedFollowing",
  "Backward",
  FALSE
)

dt_1y5y <- schedule_date_vec(fixed_schedule_1y5y)

annuity_1y5y <- {
  accruals <- map_dbl(
    seq_len(length(dt_1y5y) - 1L),
    ~ dc_a365$yearFraction(dt_1y5y[[.x]], dt_1y5y[[.x + 1L]])
  )
  dfs <- map_dbl(dt_1y5y[-1], ~ jpy_curve_obj$discount(.x))
  sum(accruals * dfs)
}

forward_swap_rate_1y5y <- (
  jpy_curve_obj$discount(dt_1y5y[[1]]) -
    jpy_curve_obj$discount(dt_1y5y[[length(dt_1y5y)]])
) / annuity_1y5y

forward_swap_tbl <- tibble(
  metric = c("annuity", "forward_swap_rate"),
  value = c(annuity_1y5y, forward_swap_rate_1y5y)
)

qlr_show_tbl(
  forward_swap_tbl,
  "1Yx5Y forward swap summary",
  n = 20
)

# 5.2 One-helper Hull-White calibration with ATM strike

hw_a_1 <- 0.03
hw_sigma_init_1 <- 1 / 10000

hw_model_1 <- HullWhite(
  jpy_curve_handle,
  hw_a_1,
  hw_sigma_init_1
)

jam_engine_1 <- JamshidianSwaptionEngine(hw_model_1)

helper_1 <- SwaptionHelper(
  exercise_date,
  maturity_tenor,
  QuoteHandle(SimpleQuote(36.06 / 10000)),
  jpy_index,
  qlr_period_months(6),
  dc_a365,
  dc_a365,
  jpy_curve_handle,
  BlackCalibrationHelper_RelativePriceError_get(),
  forward_swap_rate_1y5y,
  1.0,
  VolatilityType_Normal_get()
)

helper_1$setPricingEngine(jam_engine_1)
helper_vec_1 <- make_swaption_helper_vector(list(helper_1))

end_criteria <- EndCriteria(10000L, 100L, 1e-6, 1e-8, 1e-8)

hw_model_1$calibrate(
  helper_vec_1,
  LevenbergMarquardt(),
  end_criteria,
  NoConstraint(),
  numeric(),
  c(TRUE, FALSE)
)

hw_param_1 <- hw_model_1$params()

hw_one_helper_tbl <- tibble(
  parameter = c("a_fixed", "sigma_calibrated"),
  value = c(hw_param_1[1], hw_param_1[2])
)

qlr_show_tbl(
  hw_one_helper_tbl,
  "Hull-White one-helper calibration",
  n = 20
)

# 5.3 1Yx5Y swaption engine comparison

swap_1y5y <- VanillaSwap(
  Swap_Payer_get(),
  1,
  fixed_schedule_1y5y,
  forward_swap_rate_1y5y,
  dc_a365,
  fixed_schedule_1y5y,
  jpy_index,
  0.0,
  dc_a365
)

swaption_1y5y <- Swaption(
  swap_1y5y,
  EuropeanExercise(exercise_date)
)

swaption_1y5y$setPricingEngine(jam_engine_1)
npv_jam <- swaption_1y5y$NPV()

tree_engine_1 <- TreeSwaptionEngine(hw_model_1, 1000L)
swaption_1y5y$setPricingEngine(tree_engine_1)
npv_tree <- swaption_1y5y$NPV()

normal_engine_1 <- BachelierSwaptionEngine(
  jpy_curve_handle,
  QuoteHandle(SimpleQuote(36.06 / 10000))
)
swaption_1y5y$setPricingEngine(normal_engine_1)
npv_normal <- swaption_1y5y$NPV()

engine_compare_tbl <- tibble(
  model = c("Jamshidian", "Tree", "Normal"),
  npv = c(npv_jam, npv_tree, npv_normal)
)

qlr_show_tbl(
  engine_compare_tbl,
  "1Yx5Y swaption engine comparison",
  n = 20
)

helper_detail_tbl_1 <- tibble(
  expiry_date = qlr_iso(exercise_date),
  swap_effective_date = qlr_iso(swap_effective_date),
  swap_maturity_date = qlr_iso(swap_maturity_date),
  coupon_rate = forward_swap_rate_1y5y,
  strike = forward_swap_rate_1y5y,
  market_normal_vol = 36.06 / 10000,
  bachelier_price = npv_normal,
  hw_price = npv_jam
)

qlr_show_tbl(
  helper_detail_tbl_1,
  "1Yx5Y helper detail",
  n = 20
)

# 5.4 Multi-helper preparation: fair rates first

vol_data_multi <- tibble(
  start_year = c(1L, 2L, 3L, 4L, 5L),
  swap_tenor_year = c(5L, 4L, 3L, 2L, 1L),
  normal_vol = c(36.06, 34.28, 34.14, 34.99, 37.15) / 10000
)

multi_swap_input_tbl <- purrr::map_dfr(
  seq_len(nrow(vol_data_multi)),
  function(i) {
    expiry_date_i <- calendar_jp$advance(
      trade_date_calib,
      vol_data_multi$start_year[i],
      "Years"
    )

    swap_effective_i <- expiry_date_i + 2L
    swap_maturity_i <- calendar_jp$advance(
      swap_effective_i,
      vol_data_multi$swap_tenor_year[i],
      "Years"
    )

    fixed_schedule_i <- Schedule(
      swap_effective_i,
      swap_maturity_i,
      qlr_period_months(6),
      calendar_jp,
      "ModifiedFollowing",
      "ModifiedFollowing",
      "Backward",
      FALSE
    )

    dt_i <- schedule_date_vec(fixed_schedule_i)

    accrual_i <- map_dbl(
      seq_len(length(dt_i) - 1L),
      ~ dc_a365$yearFraction(dt_i[[.x]], dt_i[[.x + 1L]])
    )

    dfs_i <- map_dbl(dt_i[-1], ~ jpy_curve_obj$discount(.x))
    annuity_i <- sum(accrual_i * dfs_i)

    fair_rate_i <- (
      jpy_curve_obj$discount(dt_i[[1]]) -
        jpy_curve_obj$discount(dt_i[[length(dt_i)]])
    ) / annuity_i

    tibble(
      row_id = i,
      expiry_date_obj = list(expiry_date_i),
      swap_effective_obj = list(swap_effective_i),
      swap_maturity_obj = list(swap_maturity_i),
      fixed_schedule_obj = list(fixed_schedule_i),
      expiry_date = qlr_iso(expiry_date_i),
      swap_effective_date = qlr_iso(swap_effective_i),
      swap_maturity_date = qlr_iso(swap_maturity_i),
      annuity = annuity_i,
      fair_rate = fair_rate_i,
      market_normal_vol = vol_data_multi$normal_vol[i]
    )
  }
)

# 5.5 Multi-helper Hull-White calibration with ATM strikes

hw_model_multi <- HullWhite(
  jpy_curve_handle,
  0.03,
  1 / 10000
)

jam_engine_multi <- JamshidianSwaptionEngine(hw_model_multi)

helper_list_multi <- purrr::map(
  seq_len(nrow(multi_swap_input_tbl)),
  function(i) {
    helper_i <- SwaptionHelper(
      multi_swap_input_tbl$expiry_date_obj[[i]],
      qlr_period_years(vol_data_multi$swap_tenor_year[i]),
      QuoteHandle(SimpleQuote(multi_swap_input_tbl$market_normal_vol[i])),
      jpy_index,
      qlr_period_months(6),
      dc_a365,
      dc_a365,
      jpy_curve_handle,
      BlackCalibrationHelper_RelativePriceError_get(),
      multi_swap_input_tbl$fair_rate[i],
      1.0,
      VolatilityType_Normal_get()
    )

    helper_i$setPricingEngine(jam_engine_multi)
    helper_i
  }
)

helper_vec_multi <- make_swaption_helper_vector(helper_list_multi)

hw_model_multi$calibrate(
  helper_vec_multi,
  LevenbergMarquardt(),
  end_criteria,
  NoConstraint(),
  numeric(),
  c(TRUE, FALSE)
)

hw_param_multi <- hw_model_multi$params()

hw_multi_tbl <- tibble(
  parameter = c("a_fixed", "sigma_calibrated"),
  value = c(hw_param_multi[1], hw_param_multi[2])
)

qlr_show_tbl(
  hw_multi_tbl,
  "Hull-White multi-helper calibration",
  n = 20
)

# 5.6 Multi-helper detail

helper_detail_multi_tbl <- purrr::map_dfr(
  seq_len(nrow(multi_swap_input_tbl)),
  function(i) {
    fixed_schedule_i <- multi_swap_input_tbl$fixed_schedule_obj[[i]]

    swap_i <- VanillaSwap(
      Swap_Payer_get(),
      1,
      fixed_schedule_i,
      multi_swap_input_tbl$fair_rate[i],
      dc_a365,
      fixed_schedule_i,
      jpy_index,
      0.0,
      dc_a365
    )

    swptn_i <- Swaption(
      swap_i,
      EuropeanExercise(multi_swap_input_tbl$expiry_date_obj[[i]])
    )

    swptn_i$setPricingEngine(
      BachelierSwaptionEngine(
        jpy_curve_handle,
        QuoteHandle(SimpleQuote(multi_swap_input_tbl$market_normal_vol[i]))
      )
    )
    bac_price_i <- swptn_i$NPV()

    swptn_i$setPricingEngine(JamshidianSwaptionEngine(hw_model_multi))
    hw_price_i <- swptn_i$NPV()

    tibble(
      swap_effective_date = multi_swap_input_tbl$swap_effective_date[i],
      swap_maturity_date = multi_swap_input_tbl$swap_maturity_date[i],
      expiry_date = multi_swap_input_tbl$expiry_date[i],
      coupon_rate = multi_swap_input_tbl$fair_rate[i],
      strike = multi_swap_input_tbl$fair_rate[i],
      market_normal_vol = multi_swap_input_tbl$market_normal_vol[i],
      bachelier_price = bac_price_i,
      hw_price = hw_price_i
    )
  }
)

qlr_show_tbl(
  helper_detail_multi_tbl,
  "Multi-helper detail",
  n = 20
)

# 5.7 Bermudan swaption under calibrated Hull-White

bermudan_ex_dates_1y5y <- ql_date_vector(
  purrr::map(
    dt_1y5y[-length(dt_1y5y)],
    ~ calendar_jp$advance(.x, -2L, "Days")
  )
)

bermudan_swaption_1y5y <- Swaption(
  swap_1y5y,
  BermudanExercise(bermudan_ex_dates_1y5y)
)

tree_engine_multi <- TreeSwaptionEngine(hw_model_multi, 12L * 6L)
bermudan_swaption_1y5y$setPricingEngine(tree_engine_multi)

bermudan_multi_tbl <- tibble(
  metric = c("bermudan_swaption_npv"),
  value = c(bermudan_swaption_1y5y$NPV())
)

qlr_show_tbl(
  bermudan_multi_tbl,
  "Bermudan swaption under calibrated Hull-White",
  n = 20
)

cat("\nchapter09 callable / hull-white rewrite part3 completed successfully.\n")

# ------------------------------------------------------------
# 6. Part 4A: one-step tree hand calculation
# ------------------------------------------------------------

qlr_set_eval_date(trade_date_calib)

aa_hand <- 0.03
sigma_hand <- 0.39609 / 100
expr_date_hand <- calendar_jp$advance(trade_date_calib, 1L, "Years")

swap_effective_hand <- qlr_date("2023-08-23")
swap_maturity_hand <- qlr_date("2028-08-23")

fix_schedule_hand <- Schedule(
  swap_effective_hand,
  swap_maturity_hand,
  qlr_period_months(6),
  calendar_jp,
  "ModifiedFollowing",
  "ModifiedFollowing",
  "Backward",
  FALSE
)

fix_dates_hand <- schedule_date_vec(fix_schedule_hand)

qlr_show_tbl(
  make_schedule_tbl(fix_schedule_hand),
  "1Yx5Y fixed schedule for hand calculation",
  n = 20
)

curve_ref_date <- jpy_curve_obj$referenceDate()

cf_year_tbl <- tibble(
  schedule_date = map_chr(fix_dates_hand, qlr_iso),
  cf_year = map_dbl(fix_dates_hand, ~ dc_a365$yearFraction(curve_ref_date, .x)),
  now_df = map_dbl(fix_dates_hand, ~ jpy_curve_obj$discount(.x))
) |>
  mutate(
    tenor_year = c(
      0,
      purrr::map_dbl(
        seq_len(n() - 1L),
        ~ dc_a365$yearFraction(fix_dates_hand[[.x]], fix_dates_hand[[.x + 1L]])
      )
    )
  )

qlr_show_tbl(
  cf_year_tbl,
  "Cashflow dates / year fractions / discount factors",
  n = 20
)

annuity_hand <- sum(cf_year_tbl$now_df * cf_year_tbl$tenor_year)
coupon_rate_hand <- (cf_year_tbl$now_df[1] - dplyr::last(cf_year_tbl$now_df)) / annuity_hand

forward_swap_hand_tbl <- tibble(
  metric = c("annuity", "forward_swap_rate"),
  value = c(annuity_hand, coupon_rate_hand)
)

qlr_show_tbl(
  forward_swap_hand_tbl,
  "Forward swap summary for hand calculation",
  n = 20
)

hw_BB <- function(t, T, a) {
  (1 - exp(-a * (T - t))) / a
}

hw_VV <- function(t, T, a, sigma) {
  (sigma^2 / a^2) * (
    (T - t) +
      (2 / a) * exp(-a * (T - t)) -
      (1 / (2 * a)) * exp(-2 * a * (T - t)) -
      3 / (2 * a)
  )
}

hw_AA <- function(t, T, a, sigma) {
  0.5 * (hw_VV(t, T, a, sigma) - hw_VV(0, T, a, sigma) + hw_VV(0, t, a, sigma))
}

hw_forward_drift <- function(s, t, T, a, sigma) {
  item_a <- 1 - exp(-a * (t - s))
  item_b <- exp(-a * (T - t)) - exp(-a * (T + t - 2 * s))
  (sigma^2 / a^2) * (item_a - 0.5 * item_b)
}

dT_hand <- dc_a365$yearFraction(trade_date_calib, expr_date_hand)
dX_hand <- sigma_hand * sqrt(3 / (2 * aa_hand) * (1 - exp(-2 * aa_hand * dT_hand)))

x1_prob <- c(1 / 6, 4 / 6, 1 / 6)
x1_val_raw <- c(dX_hand, 0, -dX_hand)

one_step_state_tbl <- tibble(
  state = c("up", "mid", "down"),
  x_value = x1_val_raw,
  probability = x1_prob
)

qlr_show_tbl(
  one_step_state_tbl,
  "One-step x states (raw)",
  n = 20
)

mkt_curve_ratio <- cf_year_tbl$now_df / cf_year_tbl$now_df[1]
bb_vec <- purrr::map_dbl(cf_year_tbl$cf_year, ~ hw_BB(cf_year_tbl$cf_year[1], .x, aa_hand))
aa_vec <- purrr::map_dbl(cf_year_tbl$cf_year, ~ hw_AA(cf_year_tbl$cf_year[1], .x, aa_hand, sigma_hand))

x1_df_raw <- purrr::map(
  x1_val_raw,
  ~ mkt_curve_ratio * exp(aa_vec - .x * bb_vec)
)

x1_df_raw_tbl <- purrr::imap_dfr(
  x1_df_raw,
  function(df_vec, i) {
    tibble(
      state = c("up", "mid", "down")[i],
      cf_year = cf_year_tbl$cf_year,
      node_df = df_vec
    )
  }
)

qlr_show_tbl(
  x1_df_raw_tbl,
  "Node discount factors (raw x)",
  n = 30
)

x1_annuity_raw <- purrr::map_dbl(
  x1_df_raw,
  ~ sum(.x * cf_year_tbl$tenor_year)
)

itm_rate_raw <- purrr::map_dbl(
  x1_df_raw,
  ~ (1 - dplyr::last(.x)) / sum(.x * cf_year_tbl$tenor_year) - coupon_rate_hand
)

itm_rate_raw_pos <- pmax(itm_rate_raw, 0)

expr_df_raw <- exp(
  -(
    0.5 * dT_hand * x1_val_raw +
      (-log(jpy_curve_obj$discount(expr_date_hand)) + 0.5 * hw_VV(0, dT_hand, aa_hand, sigma_hand)) * dT_hand
  )
)

swaption_npv_raw <- sum(itm_rate_raw_pos * x1_annuity_raw * expr_df_raw * x1_prob)

one_step_raw_tbl <- tibble(
  state = c("up", "mid", "down"),
  x_value = x1_val_raw,
  probability = x1_prob,
  annuity = x1_annuity_raw,
  itm_rate_before_floor = itm_rate_raw,
  itm_rate_after_floor = itm_rate_raw_pos,
  exercise_df = expr_df_raw
)

qlr_show_tbl(
  one_step_raw_tbl,
  "One-step swaption decomposition (raw x)",
  n = 20
)

one_step_raw_summary_tbl <- tibble(
  metric = c("dT", "dX", "coupon_rate", "one_step_swaption_npv_raw"),
  value = c(dT_hand, dX_hand, coupon_rate_hand, swaption_npv_raw)
)

qlr_show_tbl(
  one_step_raw_summary_tbl,
  "One-step hand calculation summary (raw x)",
  n = 20
)

t_fwd_drift <- hw_forward_drift(
  s = 0,
  t = dT_hand,
  T = dT_hand,
  a = aa_hand,
  sigma = sigma_hand
)

x1_val_adj <- x1_val_raw - t_fwd_drift

x1_df_adj <- purrr::map(
  x1_val_adj,
  ~ mkt_curve_ratio * exp(aa_vec - .x * bb_vec)
)

x1_annuity_adj <- purrr::map_dbl(
  x1_df_adj,
  ~ sum(.x * cf_year_tbl$tenor_year)
)

itm_rate_adj <- purrr::map_dbl(
  x1_df_adj,
  ~ (1 - dplyr::last(.x)) / sum(.x * cf_year_tbl$tenor_year) - coupon_rate_hand
)

itm_rate_adj_pos <- pmax(itm_rate_adj, 0)

swaption_npv_adj <- sum(itm_rate_adj_pos * x1_annuity_adj * x1_prob) * jpy_curve_obj$discount(expr_date_hand)

one_step_adj_tbl <- tibble(
  state = c("up", "mid", "down"),
  x_value_adjusted = x1_val_adj,
  probability = x1_prob,
  annuity = x1_annuity_adj,
  itm_rate_before_floor = itm_rate_adj,
  itm_rate_after_floor = itm_rate_adj_pos
)

qlr_show_tbl(
  one_step_adj_tbl,
  "One-step swaption decomposition (forward-drift adjusted x)",
  n = 20
)

one_step_adj_summary_tbl <- tibble(
  metric = c(
    "forward_drift_adjustment",
    "discount_to_expiry",
    "one_step_swaption_npv_adjusted"
  ),
  value = c(
    t_fwd_drift,
    jpy_curve_obj$discount(expr_date_hand),
    swaption_npv_adj
  )
)

qlr_show_tbl(
  one_step_adj_summary_tbl,
  "One-step hand calculation summary (forward-drift adjusted x)",
  n = 20
)

part3_vs_hand_tbl <- tibble(
  method = c(
    "one_step_raw",
    "one_step_forward_adjusted",
    "jamshidian_part3",
    "tree_part3",
    "normal_part3"
  ),
  npv = c(
    swaption_npv_raw,
    swaption_npv_adj,
    npv_jam,
    npv_tree,
    npv_normal
  )
)

qlr_show_tbl(
  part3_vs_hand_tbl,
  "Part 3 vs Part 4A comparison",
  n = 20
)

cat("\nchapter09 callable / hull-white rewrite part4A completed successfully.\n")

# ------------------------------------------------------------
# 7. Part 4B: tree-step convergence vs hand calculation
# ------------------------------------------------------------

qlr_set_eval_date(trade_date_calib)

tree_step_grid <- c(1L, 2L, 3L, 6L, 12L, 24L, 60L, 120L, 240L, 1000L)

tree_convergence_tbl <- purrr::map_dfr(
  tree_step_grid,
  function(n_step) {
    eng_i <- TreeSwaptionEngine(hw_model_1, n_step)
    swaption_1y5y$setPricingEngine(eng_i)

    tibble(
      method = paste0("Tree_", n_step, "_step"),
      step = n_step,
      npv = swaption_1y5y$NPV()
    )
  }
)

qlr_show_tbl(
  tree_convergence_tbl,
  "Tree convergence table",
  n = 20
)

part4b_compare_tbl <- bind_rows(
  tibble(
    method = c(
      "Hand_1step_raw",
      "Hand_1step_forward_adjusted",
      "Tree_1step",
      "Tree_2step",
      "Tree_3step",
      "Tree_6step",
      "Tree_12step",
      "Tree_24step",
      "Tree_60step",
      "Tree_120step",
      "Tree_240step",
      "Tree_1000step",
      "Jamshidian",
      "Normal"
    ),
    npv = c(
      swaption_npv_raw,
      swaption_npv_adj,
      tree_convergence_tbl$npv[tree_convergence_tbl$step == 1L],
      tree_convergence_tbl$npv[tree_convergence_tbl$step == 2L],
      tree_convergence_tbl$npv[tree_convergence_tbl$step == 3L],
      tree_convergence_tbl$npv[tree_convergence_tbl$step == 6L],
      tree_convergence_tbl$npv[tree_convergence_tbl$step == 12L],
      tree_convergence_tbl$npv[tree_convergence_tbl$step == 24L],
      tree_convergence_tbl$npv[tree_convergence_tbl$step == 60L],
      tree_convergence_tbl$npv[tree_convergence_tbl$step == 120L],
      tree_convergence_tbl$npv[tree_convergence_tbl$step == 240L],
      tree_convergence_tbl$npv[tree_convergence_tbl$step == 1000L],
      npv_jam,
      npv_normal
    )
  )
) |>
  mutate(
    diff_vs_normal = npv - npv_normal,
    diff_vs_jam = npv - npv_jam
  )

qlr_show_tbl(
  part4b_compare_tbl,
  "Part 4B comparison: hand vs tree vs analytic",
  n = 30
)

part4b_summary_tbl <- tibble(
  metric = c(
    "hand_raw_minus_tree_1step",
    "hand_adj_minus_tree_1step",
    "tree_1step_minus_tree_1000step",
    "tree_60step_minus_tree_1000step",
    "tree_1000step_minus_jam",
    "jam_minus_normal"
  ),
  value = c(
    swaption_npv_raw -
      tree_convergence_tbl$npv[tree_convergence_tbl$step == 1L],
    swaption_npv_adj -
      tree_convergence_tbl$npv[tree_convergence_tbl$step == 1L],
    tree_convergence_tbl$npv[tree_convergence_tbl$step == 1L] -
      tree_convergence_tbl$npv[tree_convergence_tbl$step == 1000L],
    tree_convergence_tbl$npv[tree_convergence_tbl$step == 60L] -
      tree_convergence_tbl$npv[tree_convergence_tbl$step == 1000L],
    tree_convergence_tbl$npv[tree_convergence_tbl$step == 1000L] - npv_jam,
    npv_jam - npv_normal
  )
)

qlr_show_tbl(
  part4b_summary_tbl,
  "Part 4B summary diagnostics",
  n = 20
)

part4b_plot_tbl <- bind_rows(
  tree_convergence_tbl |>
    transmute(
      step = as.numeric(step),
      npv = npv,
      series = "Tree"
    ),
  tibble(
    step = max(tree_step_grid),
    npv = npv_jam,
    series = "Jamshidian"
  ),
  tibble(
    step = max(tree_step_grid),
    npv = npv_normal,
    series = "Normal"
  ),
  tibble(
    step = 1,
    npv = swaption_npv_raw,
    series = "Hand raw"
  ),
  tibble(
    step = 1,
    npv = swaption_npv_adj,
    series = "Hand adjusted"
  )
)

part4b_plot <- ggplot(
  part4b_plot_tbl,
  aes(x = step, y = npv, color = series)
) +
  geom_line(
    data = dplyr::filter(part4b_plot_tbl, series == "Tree"),
    linewidth = 0.7
  ) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = tree_step_grid) +
  labs(
    title = "Part 4B: hand calculation vs tree-step convergence",
    x = "Tree steps",
    y = "Swaption NPV"
  ) +
  theme_minimal()

print(part4b_plot)

cat("\nchapter09 callable / hull-white rewrite part4B completed successfully.\n")
# Hull-White one-factor model was calibrated to ATM normal swaption volatility using a
# SwaptionHelper with strike set to the forward swap rate. After this correction,
# the calibrated model reproduced the European 1Yx5Y swaption price consistently a
# cross Jamshidian, lattice, and Bachelier pricing.
#
# The one-step hand calculation produced a lower price than the full model price,
# but Part 4B showed that this is not a bug. The hand calculation is close to the one-step tree result,
# while the tree price converges upward toward the Jamshidian and Bachelier values as the number of steps
# increases. Therefore, the difference between Part 3 and Part 4A is explained by discretization error
# from the coarse one-step approximation.


# ------------------------------------------------------------
# Part 5: Hull-White path simulation for 10Y zero rate
# ------------------------------------------------------------

qlr_set_eval_date(trade_date_calib)

# ------------------------------------------------------------
# 5.1 parameters
# ------------------------------------------------------------

a_path <- hw_param_1[1]
sigma_path <- hw_param_1[2]

n_path <- 30L
n_step <- 24L * 5L
horizon_year <- 5

time_grid <- seq(0, horizon_year, length.out = n_step + 1L)
dt <- diff(time_grid)[1]

set.seed(123)

# ------------------------------------------------------------
# 5.2 HW helper functions
# ------------------------------------------------------------

hw_B_path <- function(t, T, a) {
  (1 - exp(-a * (T - t))) / a
}

hw_V_path <- function(t, T, a, sigma) {
  (sigma^2 / a^2) * (
    (T - t) +
      (2 / a) * exp(-a * (T - t)) -
      (1 / (2 * a)) * exp(-2 * a * (T - t)) -
      3 / (2 * a)
  )
}

hw_A_path <- function(t, T, a, sigma) {
  0.5 * (
    hw_V_path(t, T, a, sigma) -
      hw_V_path(0, T, a, sigma) +
      hw_V_path(0, t, a, sigma)
  )
}

hw_discount_given_x_path <- function(t, T, x_t, curve_obj, a, sigma) {
  p0t <- curve_obj$discount(t)
  p0T <- curve_obj$discount(T)
  A_tT <- hw_A_path(t, T, a, sigma)
  B_tT <- hw_B_path(t, T, a)

  (p0T / p0t) * exp(A_tT - x_t * B_tT)
}

hw_zero_given_x_path <- function(t, tau, x_t, curve_obj, a, sigma) {
  T <- t + tau
  ptT <- hw_discount_given_x_path(t, T, x_t, curve_obj, a, sigma)
  -log(ptT) / tau
}

# OU state process approximation for x_t
simulate_hw_x_path <- function(time_grid, a, sigma) {
  x <- numeric(length(time_grid))
  for (i in 2:length(time_grid)) {
    dt_i <- time_grid[i] - time_grid[i - 1]
    z <- rnorm(1)
    x[i] <- x[i - 1] * exp(-a * dt_i) +
      sigma * sqrt((1 - exp(-2 * a * dt_i)) / (2 * a)) * z
  }
  x
}

# ------------------------------------------------------------
# 5.3 simulate x_t paths
# ------------------------------------------------------------

x_path_mat <- replicate(
  n_path,
  simulate_hw_x_path(time_grid, a = a_path, sigma = sigma_path)
)

x_path_tbl <- tibble(
  time = rep(time_grid, times = n_path),
  path = rep(seq_len(n_path), each = length(time_grid)),
  x_t = as.vector(x_path_mat)
)

qlr_show_tbl(
  x_path_tbl,
  "Hull-White state variable paths",
  n = 40
)

x_path_plot <- ggplot(
  x_path_tbl,
  aes(x = time, y = x_t, group = path)
) +
  geom_line(linewidth = 0.5, alpha = 0.7) +
  scale_y_continuous(labels = \(x) sprintf("%.2f%%", 100 * x)) +
  labs(
    title = "Hull-White state variable paths",
    x = "Time (years)",
    y = "x(t)"
  ) +
  theme_minimal()

print(x_path_plot)

# ------------------------------------------------------------
# 5.4 10Y zero-rate paths
# ------------------------------------------------------------

tau_target <- 10

zero_10y_tbl <- purrr::map_dfr(
  seq_len(n_path),
  function(p) {
    tibble(
      time = time_grid,
      path = p,
      zero_10y = purrr::map_dbl(
        seq_along(time_grid),
        function(i) {
          hw_zero_given_x_path(
            t = time_grid[i],
            tau = tau_target,
            x_t = x_path_mat[i, p],
            curve_obj = jpy_curve_obj,
            a = a_path,
            sigma = sigma_path
          )
        }
      )
    )
  }
)

qlr_show_tbl(
  zero_10y_tbl,
  "10Y zero-rate paths",
  n = 40
)

print(zero_10y_plot)
zero_10y_plot <- ggplot(
  zero_10y_tbl,
  aes(x = time, y = zero_10y, group = path, color = factor(path))
) +
  geom_line(linewidth = 0.5, alpha = 0.7, show.legend = FALSE) +
  scale_y_continuous(labels = \(x) sprintf("%.2f%%", 100 * x)) +
  labs(
    title = "Simulated 10Y zero-rate paths under Hull-White",
    subtitle = "Each line is one path",
    x = "Observation time (years)",
    y = "10Y zero rate"
  ) +
  theme_minimal()

print(zero_10y_plot)

# ------------------------------------------------------------
# 5.5 average / percentile summary of 10Y rate
# ------------------------------------------------------------

zero_10y_summary_tbl <- zero_10y_tbl |>
  group_by(time) |>
  summarise(
    mean = mean(zero_10y),
    p10 = quantile(zero_10y, 0.10),
    p50 = quantile(zero_10y, 0.50),
    p90 = quantile(zero_10y, 0.90),
    .groups = "drop"
  )

qlr_show_tbl(
  zero_10y_summary_tbl,
  "10Y zero-rate path summary",
  n = 40
)


zero_10y_summary_plot <- ggplot(zero_10y_summary_tbl, aes(x = time)) +
  geom_line(aes(y = mean, color = "mean", linetype = "mean"), linewidth = 0.9) +
  geom_line(aes(y = p10,  color = "p10",  linetype = "p10"),  linewidth = 0.7) +
  geom_line(aes(y = p50,  color = "p50",  linetype = "p50"),  linewidth = 0.7) +
  geom_line(aes(y = p90,  color = "p90",  linetype = "p90"),  linewidth = 0.7) +
  scale_y_continuous(labels = \(x) sprintf("%.2f%%", 100 * x)) +
  scale_color_manual(
    values = c(
      mean = "black",
      p10  = "blue",
      p50  = "darkgreen",
      p90  = "red"
    )
  ) +
  labs(
    title = "10Y zero-rate summary paths",
    subtitle = "Mean / percentile evolution under Hull-White",
    x = "Observation time (years)",
    y = "10Y zero rate",
    color = "Series",
    linetype = "Series"
  ) +
  theme_minimal()

print(zero_10y_summary_plot)

cat("\nchapter09 callable / hull-white rewrite part5 completed successfully.\n")
