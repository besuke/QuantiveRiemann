# ============================================================
# chapter06_black_model_rewrite.R
# ------------------------------------------------------------
# 第6章 Blackモデル
# Python / QuantLib notebook を、
# 「QuantiveRiemann + QuantLib(SWIG for R)」前提で書き直した版。
#
# 内容:
# 1. Black calculator
# 2. Black process + analytic European pricing
# 3. Monte Carlo European pricing (hand-made)
# 4. RNG / path generator checks
# 5. Monte Carlo paths with ggplot2
# 6. Hand check of one MC path
# 7. BSM European / American option pricing
# 8. Sobol paths
# 9. Longstaff-Schwartz style hand calculation
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

library(QuantLib)
library(ggplot2)
devtools::load_all(".")

# ------------------------------------------------------------
# 0. local helpers
# ------------------------------------------------------------

fmt_pct <- function(x, digits = 4) {
  sprintf(paste0("%.", digits, "f%%"), 100 * x)
}

.qlr_path_value_at <- function(path_obj, i_zero_based) {
  out <- tryCatch(path_obj$value(i_zero_based), error = function(e) NULL)
  if (!is.null(out)) {
    return(as.numeric(out))
  }

  out <- tryCatch(path_obj$get(i_zero_based), error = function(e) NULL)
  if (!is.null(out)) {
    return(as.numeric(out))
  }

  out <- tryCatch(path_obj[[i_zero_based + 1]][[1]], error = function(e) NULL)
  if (!is.null(out)) {
    return(as.numeric(out))
  }

  stop("Unable to access path value at index ", i_zero_based)
}

.qlr_path_tbl <- function(path_obj, path_id = 1L) {
  n_point <- tryCatch(path_obj$length(), error = function(e) NULL)

  if (is.null(n_point) || is.na(n_point) || n_point <= 1L) {
    stop("Unable to determine path length correctly")
  }

  tibble::tibble(
    path_id = path_id,
    step = seq_len(n_point) - 1L,
    time = purrr::map_dbl(seq_len(n_point) - 1L, ~ path_obj$time(.x)),
    price = purrr::map_dbl(seq_len(n_point) - 1L, ~ .qlr_path_value_at(path_obj, .x))
  )
}

# ------------------------------------------------------------
# 1. Black calculator
# ------------------------------------------------------------

trade_date <- qlr_date("2023-07-21")
maturity_date <- qlr_date("2023-08-26")
option_type_call <- Option_Call_get()

dc_a365 <- Actual365Fixed()
qlr_set_eval_date(trade_date)

spot_price <- 107 + 6.75 / 32
strike_price <- 107.5
vol <- 0.052
risk_free_rate <- 0.05

maturity_year_fraction <- dc_a365$yearFraction(trade_date, maturity_date)

discount_curve <- FlatForward(
  trade_date,
  risk_free_rate,
  dc_a365,
  "Continuous"
)

payoff <- PlainVanillaPayoff(option_type_call, strike_price)
std_dev <- vol * sqrt(maturity_year_fraction)

black_calc <- BlackCalculator(
  payoff,
  spot_price,
  std_dev,
  discount_curve$discount(maturity_date)
)

black_calc_tbl <- tibble::tibble(
  metric = c("npv", "delta", "gamma", "vega", "theta", "theta_per_day"),
  value = c(
    black_calc$value(),
    black_calc$delta(spot_price),
    black_calc$gamma(spot_price),
    black_calc$vega(maturity_year_fraction),
    black_calc$theta(spot_price, maturity_year_fraction),
    black_calc$thetaPerDay(spot_price, maturity_year_fraction)
  )
)

qlr_show_tbl(
  black_calc_tbl,
  "Black calculator summary",
  n = 20
)

# ------------------------------------------------------------
# 2. Black process + analytic European pricing
# ------------------------------------------------------------

spot_handle <- QuoteHandle(SimpleQuote(spot_price))

rf_curve_obj <- FlatForward(
  trade_date,
  risk_free_rate,
  dc_a365,
  "Continuous"
)
rf_curve_handle <- YieldTermStructureHandle(rf_curve_obj)

vol_obj <- BlackConstantVol(
  trade_date,
  NullCalendar(),
  vol,
  dc_a365
)
vol_handle <- BlackVolTermStructureHandle(vol_obj)

black_process <- BlackProcess(
  spot_handle,
  rf_curve_handle,
  vol_handle
)

analytic_engine <- AnalyticEuropeanEngine(black_process)

euro_option <- VanillaOption(
  payoff,
  EuropeanExercise(maturity_date)
)

euro_option$setPricingEngine(analytic_engine)

npv_begin <- euro_option$NPV()

black_process_tbl <- tibble::tibble(
  metric = c("npv", "delta", "gamma", "vega", "theta_per_day", "implied_vol"),
  value = c(
    euro_option$NPV(),
    euro_option$delta(),
    euro_option$gamma(),
    euro_option$vega(),
    euro_option$thetaPerDay(),
    euro_option$impliedVolatility(npv_begin, black_process)
  )
)

qlr_show_tbl(
  black_process_tbl,
  "Black process analytic European summary",
  n = 20
)

# ------------------------------------------------------------
# 3. Monte Carlo European pricing (hand-made for Black process)
# ------------------------------------------------------------

n_step_mc <- 3L
n_path_mc <- 100000L
seed_mc <- 1L

uniform_rng_mc <- UniformRandomGenerator(seed_mc)
uniform_seq_rng_mc <- UniformRandomSequenceGenerator(n_step_mc, uniform_rng_mc)
gaussian_seq_rng_mc <- GaussianRandomSequenceGenerator(uniform_seq_rng_mc)

path_generator_mc <- GaussianPathGenerator(
  black_process,
  maturity_year_fraction,
  n_step_mc,
  gaussian_seq_rng_mc,
  FALSE
)

mc_terminal_tbl <- purrr::map_dfr(
  seq_len(n_path_mc),
  function(i) {
    one_path <- path_generator_mc$`next`()$value()
    n_point <- one_path$length()

    tibble::tibble(
      path_id = i,
      terminal_price = .qlr_path_value_at(one_path, n_point - 1L)
    )
  }
) |>
  dplyr::mutate(
    payoff = pmax(terminal_price - strike_price, 0)
  )

mc_npv <- mean(mc_terminal_tbl$payoff) * rf_curve_obj$discount(maturity_date)

mc_euro_tbl <- tibble::tibble(
  metric = c("analytic_npv", "mc_npv"),
  value = c(
    npv_begin,
    mc_npv
  )
)

qlr_show_tbl(
  mc_euro_tbl,
  "Monte Carlo European summary",
  n = 20
)

euro_option$setPricingEngine(analytic_engine)

# ------------------------------------------------------------
# 4. RNG / path generator checks
# ------------------------------------------------------------

uniform_seq_rng <- UniformRandomSequenceGenerator(
  3,
  UniformRandomGenerator(1)
)

uniform_seq_tbl <- tibble::tibble(
  draw = 1:3,
  uniform_value = as.numeric(uniform_seq_rng$nextSequence()$value())
)

qlr_show_tbl(
  uniform_seq_tbl,
  "Uniform sequence RNG values",
  n = 20
)

gaussian_seq_rng_demo <- GaussianRandomSequenceGenerator(
  UniformRandomSequenceGenerator(3, UniformRandomGenerator(1))
)

gaussian_seq_tbl <- tibble::tibble(
  draw = 1:3,
  gaussian_value = as.numeric(gaussian_seq_rng_demo$nextSequence()$value())
)

qlr_show_tbl(
  gaussian_seq_tbl,
  "Gaussian sequence RNG values",
  n = 20
)
# ------------------------------------------------------------
# 5. Monte Carlo paths with ggplot2
# ------------------------------------------------------------

maturity_year_plot <- 3
n_step_path <- 12L          # 3年 × 4 = 12 steps
n_path_plot <- 120L
seed_path <- 1L

uniform_rng_path <- UniformRandomGenerator(seed_path)
uniform_seq_rng_path <- UniformRandomSequenceGenerator(n_step_path, uniform_rng_path)
gaussian_seq_rng_path <- GaussianRandomSequenceGenerator(uniform_seq_rng_path)

path_generator <- GaussianPathGenerator(
  black_process,
  maturity_year_plot,
  n_step_path,
  gaussian_seq_rng_path,
  FALSE
)

mc_path_tbl <- purrr::map_dfr(
  seq_len(n_path_plot),
  function(i) {
    one_path <- path_generator$`next`()$value()
    .qlr_path_tbl(one_path, path_id = i)
  }
)

qlr_show_tbl(
  mc_path_tbl,
  "Monte Carlo path table",
  n = 10
)

mc_path_plot <- ggplot2::ggplot(
  mc_path_tbl,
  ggplot2::aes(
    x = time,
    y = price,
    group = path_id,
    color = factor(path_id)
  )
) +
  ggplot2::geom_line(
    alpha = 0.55,
    linewidth = 0.45
  ) +
  ggplot2::scale_color_viridis_d(
    option = "C",
    guide = "none"
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(0, 3, by = 0.25)
  ) +
  ggplot2::labs(
    title = "Black model Monte Carlo paths",
    x = "Year",
    y = "Futures price"
  ) +
  ggplot2::theme_minimal()

print(mc_path_plot)

terminal_tbl <- mc_path_tbl |>
  dplyr::group_by(path_id) |>
  dplyr::slice_tail(n = 1) |>
  dplyr::ungroup()

terminal_hist_plot <- ggplot2::ggplot(
  terminal_tbl,
  ggplot2::aes(x = price)
) +
  ggplot2::geom_histogram(
    bins = 24,
    fill = "deepskyblue3",
    color = "white",
    linewidth = 0.35
  ) +
  ggplot2::labs(
    title = "Terminal futures price distribution",
    x = "Terminal futures price",
    y = "Count"
  ) +
  ggplot2::theme_minimal()

print(terminal_hist_plot)

maturity_year_plot <- 3
n_step_path <- 12L          # 3年 × 4 = 12 steps
n_path_plot <- 1200L
seed_path <- 1L

uniform_rng_path <- UniformRandomGenerator(seed_path)
uniform_seq_rng_path <- UniformRandomSequenceGenerator(n_step_path, uniform_rng_path)
gaussian_seq_rng_path <- GaussianRandomSequenceGenerator(uniform_seq_rng_path)

path_generator <- GaussianPathGenerator(
  black_process,
  maturity_year_plot,
  n_step_path,
  gaussian_seq_rng_path,
  FALSE
)

mc_path_tbl <- purrr::map_dfr(
  seq_len(n_path_plot),
  function(i) {
    one_path <- path_generator$`next`()$value()
    .qlr_path_tbl(one_path, path_id = i)
  }
)

qlr_show_tbl(
  mc_path_tbl,
  "Monte Carlo path table",
  n = 10
)

mc_path_plot <- ggplot2::ggplot(
  mc_path_tbl,
  ggplot2::aes(
    x = time,
    y = price,
    group = path_id,
    color = factor(path_id)
  )
) +
  ggplot2::geom_line(
    alpha = 0.55,
    linewidth = 0.45
  ) +
  ggplot2::scale_color_viridis_d(
    option = "C",
    guide = "none"
  ) +
  ggplot2::scale_x_continuous(
    breaks = seq(0, 3, by = 0.25)
  ) +
  ggplot2::labs(
    title = "Black model Monte Carlo paths",
    x = "Year",
    y = "Futures price"
  ) +
  ggplot2::theme_minimal()

print(mc_path_plot)

terminal_tbl <- mc_path_tbl |>
  dplyr::group_by(path_id) |>
  dplyr::slice_tail(n = 1) |>
  dplyr::ungroup()

terminal_hist_plot <- ggplot2::ggplot(
  terminal_tbl,
  ggplot2::aes(x = price)
) +
  ggplot2::geom_histogram(
    bins = 24,
    fill = "deepskyblue3",
    color = "white",
    linewidth = 0.35
  ) +
  ggplot2::labs(
    title = "Terminal futures price distribution",
    x = "Terminal futures price",
    y = "Count"
  ) +
  ggplot2::theme_minimal()

print(terminal_hist_plot)
# ------------------------------------------------------------
# 6. Hand check of one MC path
# ------------------------------------------------------------

n_step_small <- 3L
n_path_small <- 10L
seed_small <- 1L

uniform_rng_small <- UniformRandomGenerator(seed_small)
uniform_seq_rng_small <- UniformRandomSequenceGenerator(n_step_small, uniform_rng_small)
gaussian_seq_rng_small <- GaussianRandomSequenceGenerator(uniform_seq_rng_small)

path_generator_small <- GaussianPathGenerator(
  black_process,
  maturity_year_fraction,
  n_step_small,
  gaussian_seq_rng_small,
  FALSE
)

path_list_small <- vector("list", n_path_small)

for (i in seq_len(n_path_small)) {
  path_list_small[[i]] <- path_generator_small$`next`()$value()
}

one_path_small <- path_list_small[[1]]
one_path_tbl <- .qlr_path_tbl(one_path_small, path_id = 1L)

time_grid <- one_path_tbl$time
dt_vec <- diff(time_grid)

gaussian_seq_rng_check <- GaussianRandomSequenceGenerator(
  UniformRandomSequenceGenerator(n_step_small, UniformRandomGenerator(seed_small))
)

d_w <- as.numeric(gaussian_seq_rng_check$nextSequence()$value())

price_path_hand <- numeric(length(d_w) + 1L)
price_path_hand[1] <- spot_price

exp_part <- exp(-0.5 * vol^2 * dt_vec + vol * sqrt(dt_vec) * d_w)

for (i in seq_along(exp_part)) {
  price_path_hand[i + 1L] <- price_path_hand[i] * exp_part[i]
}

hand_path_tbl <- tibble::tibble(
  step = seq_along(price_path_hand) - 1L,
  time = time_grid,
  generated_price = one_path_tbl$price,
  hand_price = price_path_hand
)

qlr_show_tbl(
  hand_path_tbl,
  "One-path hand check",
  n = 20
)

# ------------------------------------------------------------
# 7. BSM European / American option pricing
# ------------------------------------------------------------

trade_date_bsm <- qlr_date("2024-03-01")
qlr_set_eval_date(trade_date_bsm)

calendar_nl <- TARGET()
maturity_date_bsm <- calendar_nl$advance(trade_date_bsm, 1L, "Years")

spot_price_bsm <- 100
strike_price_bsm <- 100
vol_bsm <- 0.20
risk_free_rate_bsm <- 0.05
dividend_rate_bsm <- 0.0
option_type_put <- Option_Put_get()

spot_handle_bsm <- QuoteHandle(SimpleQuote(spot_price_bsm))

rf_curve_bsm <- FlatForward(
  trade_date_bsm,
  risk_free_rate_bsm,
  dc_a365,
  "Continuous"
)
rf_handle_bsm <- YieldTermStructureHandle(rf_curve_bsm)

div_curve_bsm <- FlatForward(
  trade_date_bsm,
  dividend_rate_bsm,
  dc_a365,
  "Continuous"
)
div_handle_bsm <- YieldTermStructureHandle(div_curve_bsm)

vol_obj_bsm <- BlackConstantVol(
  trade_date_bsm,
  calendar_nl,
  vol_bsm,
  dc_a365
)
vol_handle_bsm <- BlackVolTermStructureHandle(vol_obj_bsm)

bsm_process <- BlackScholesMertonProcess(
  spot_handle_bsm,
  div_handle_bsm,
  rf_handle_bsm,
  vol_handle_bsm
)

payoff_put <- PlainVanillaPayoff(option_type_put, strike_price_bsm)

# European put
euro_option_bsm <- VanillaOption(
  payoff_put,
  EuropeanExercise(maturity_date_bsm)
)

crr_engine_3 <- BinomialCRRVanillaEngine(bsm_process, 3L)
euro_option_bsm$setPricingEngine(crr_engine_3)
euro_crr_3 <- euro_option_bsm$NPV()

analytic_engine_bsm <- AnalyticEuropeanEngine(bsm_process)
euro_option_bsm$setPricingEngine(analytic_engine_bsm)
euro_analytic <- euro_option_bsm$NPV()

crr_engine_250 <- BinomialCRRVanillaEngine(bsm_process, 250L)
euro_option_bsm$setPricingEngine(crr_engine_250)
euro_crr_250 <- euro_option_bsm$NPV()

euro_compare_tbl <- tibble::tibble(
  method = c("crr_3", "analytic", "crr_250"),
  npv = c(euro_crr_3, euro_analytic, euro_crr_250)
)

qlr_show_tbl(
  euro_compare_tbl,
  "European put comparison",
  n = 20
)

# American put
american_option_bsm <- VanillaOption(
  payoff_put,
  AmericanExercise(trade_date_bsm, maturity_date_bsm)
)

american_option_bsm$setPricingEngine(crr_engine_3)
am_tree_3 <- american_option_bsm$NPV()

baw_engine <- BaroneAdesiWhaleyApproximationEngine(bsm_process)
american_option_bsm$setPricingEngine(baw_engine)
am_baw <- american_option_bsm$NPV()

bjs_engine <- BjerksundStenslandApproximationEngine(bsm_process)
american_option_bsm$setPricingEngine(bjs_engine)
am_bjs <- american_option_bsm$NPV()

american_option_bsm$setPricingEngine(crr_engine_250)
am_tree_250 <- american_option_bsm$NPV()

american_compare_tbl <- tibble::tibble(
  method = c(
    "tree_3",
    "barone_adesi_whaley",
    "bjerksund_stensland",
    "tree_250"
  ),
  npv = c(
    am_tree_3,
    am_baw,
    am_bjs,
    am_tree_250
  )
)

qlr_show_tbl(
  american_compare_tbl,
  "American put comparison",
  n = 20
)

# ------------------------------------------------------------
# 8. Sobol paths
# ------------------------------------------------------------

trade_date_sobol <- qlr_date("2024-03-01")
qlr_set_eval_date(trade_date_sobol)

maturity_year_sobol <- 1
maturity_date_sobol <- calendar_nl$advance(trade_date_sobol, maturity_year_sobol, "Years")

spot_price_sobol <- 100
strike_price_sobol <- 100
vol_sobol <- 0.20
risk_free_rate_sobol <- 0.05
dividend_rate_sobol <- 0.0
n_step_sobol <- 3L
n_path_sobol <- 8L
seed_sobol <- 1L

spot_handle_sobol <- QuoteHandle(SimpleQuote(spot_price_sobol))

rf_curve_sobol <- FlatForward(
  trade_date_sobol,
  risk_free_rate_sobol,
  dc_a365,
  "Continuous"
)
rf_handle_sobol <- YieldTermStructureHandle(rf_curve_sobol)

div_curve_sobol <- FlatForward(
  trade_date_sobol,
  dividend_rate_sobol,
  dc_a365,
  "Continuous"
)
div_handle_sobol <- YieldTermStructureHandle(div_curve_sobol)

vol_obj_sobol <- BlackConstantVol(
  trade_date_sobol,
  calendar_nl,
  vol_sobol,
  dc_a365
)
vol_handle_sobol <- BlackVolTermStructureHandle(vol_obj_sobol)

bsm_process_sobol <- BlackScholesMertonProcess(
  spot_handle_sobol,
  div_handle_sobol,
  rf_handle_sobol,
  vol_handle_sobol
)

uniform_ld_seq_rng <- UniformLowDiscrepancySequenceGenerator(
  n_step_sobol,
  seed_sobol
)

gaussian_ld_seq_rng <- GaussianLowDiscrepancySequenceGenerator(
  uniform_ld_seq_rng
)

sobol_path_generator <- GaussianSobolPathGenerator(
  bsm_process_sobol,
  maturity_year_sobol,
  n_step_sobol,
  gaussian_ld_seq_rng,
  FALSE
)

sobol_path_tbl <- purrr::map_dfr(
  seq_len(n_path_sobol),
  function(i) {
    one_path <- sobol_path_generator$`next`()$value()
    .qlr_path_tbl(one_path, path_id = i)
  }
)

qlr_show_tbl(
  sobol_path_tbl,
  "Sobol path table",
  n = 20
)

sobol_path_plot <- ggplot2::ggplot(
  sobol_path_tbl,
  ggplot2::aes(x = time, y = price, group = path_id)
) +
  ggplot2::geom_line(alpha = 0.6, linewidth = 0.45) +
  ggplot2::labs(
    title = "BSM Sobol paths",
    x = "Year",
    y = "Stock price"
  ) +
  ggplot2::theme_minimal()

print(sobol_path_plot)
# ------------------------------------------------------------
# 9. Longstaff-Schwartz style hand calculation
# ------------------------------------------------------------

mtrx_pt <- sobol_path_tbl |>
  dplyr::select(path_id, step, price) |>
  tidyr::pivot_wider(
    names_from = step,
    values_from = price
  ) |>
  dplyr::arrange(path_id) |>
  dplyr::select(-path_id) |>
  dplyr::mutate(
    dplyr::across(dplyr::everything(), as.numeric)
  ) |>
  as.matrix()

time_grid_lsm <- sobol_path_tbl |>
  dplyr::filter(path_id == 1) |>
  dplyr::arrange(step) |>
  dplyr::pull(time)

col_df <- purrr::map_dbl(
  time_grid_lsm[-1],
  ~ rf_curve_sobol$discount(.x)
)

step_df <- col_df[1]

mtrx_cf <- pmax(strike_price_sobol - mtrx_pt, 0)

qlr_show_tbl(
  tibble::as_tibble(mtrx_cf),
  "Initial exercise-value matrix",
  n = 20
)

purrr::walk(
  seq(from = n_step_sobol - 1L, to = 1L, by = -1L),
  function(ss) {
    x_prc <- mtrx_pt[, ss + 1L]
    x_exe <- mtrx_cf[, ss + 1L]

    itm_id <- which(x_exe > 0)
    y_exe <- rep(0, n_path_sobol)

    purrr::walk(
      ss:(n_step_sobol - 1L),
      function(ii) {
        y_exe <<- y_exe + mtrx_cf[, ii + 2L] * step_df^(ii + 1L - ss)
      }
    )

    if (length(itm_id) >= 3) {
      x_itm <- x_prc[itm_id]
      y_itm <- y_exe[itm_id]

      coef <- stats::lm(y_itm ~ poly(x_itm, 2, raw = TRUE)) |>
        stats::coef()

      cont_value <- coef[1] + coef[2] * x_prc + coef[3] * x_prc^2
      cont_value <- ifelse(cont_value < 0, 0, cont_value)

      exe_id <- x_exe > cont_value

      mtrx_cf[exe_id, (ss + 2L):(n_step_sobol + 1L)] <<- 0
      mtrx_cf[!exe_id, ss + 1L] <<- 0
    }
  }
)

qlr_show_tbl(
  tibble::as_tibble(mtrx_cf),
  "Exercise-value matrix after backward induction",
  n = 20
)

lsm_npv <- sum(
  mtrx_cf[, -1, drop = FALSE] *
    matrix(
      rep(col_df, each = n_path_sobol),
      nrow = n_path_sobol
    )
) / n_path_sobol

lsm_tbl <- tibble::tibble(
  metric = "lsm_hand_npv",
  value = lsm_npv
)

qlr_show_tbl(
  lsm_tbl,
  "Longstaff-Schwartz hand calculation",
  n = 20
)

cat("\nchapter06 black model rewrite completed successfully.\n")

