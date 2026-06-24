# ============================================================
# chapter08_sabr_model_rewrite.R
# ------------------------------------------------------------
# 第8章 SABR calibration
# - Black SABR calibration
# - parameter sensitivity
# - shifted SABR normal volatility approximation
# - shifted SABR calibration
# - numerical Jacobian history
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

rmse_vec <- function(x, y) {
  sqrt(mean((x - y)^2))
}

safe_sabr_vol <- function(strike, forward, maturity, alpha, beta, volvol, rho) {
  tryCatch(
    sabrVolatility(strike, forward, maturity, alpha, beta, volvol, rho),
    error = function(e) NA_real_
  )
}

approx_fprime_r <- function(par, fn, epsilon = 1e-8) {
  base_value <- fn(par)

  map_dbl(
    seq_along(par),
    function(i) {
      par_up <- par
      par_up[i] <- par_up[i] + epsilon
      (fn(par_up) - base_value) / epsilon
    }
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
        g^2 * (
          0.016969777977 -
            g^2 * (2.6207332461e-3 - 9.6066952861e-5 * g^2)
        )
    ) / (
      1.0 -
        g^2 * (
          0.6635646938 -
            g^2 * (0.14528712196 - 0.010472855461 * g^2)
        )
    )

    xbar <- g * (0.3989422804014326 + xibar * g^2)
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

  xbar + 3.0 * q * xbar^2 * (2.0 - q * xbar * (2.0 + xbar^2)) /
    (
      6.0 +
        q * xbar * (
          -12.0 +
            xbar * (
              6.0 * q +
                xbar * (-6.0 + q * xbar * (3.0 + xbar^2))
            )
        )
    )
}

normal_vol_hagan <- function(K, F, TT, beta, alpha, volvol, rho) {
  eps <- 1e-7

  A1 <- 1 + log(F / K)^2 / 24 + log(F / K)^4 / 1920
  A2 <- 1 +
    ((1 - beta)^2) * log(F / K)^2 / 24 +
    ((1 - beta)^4) * log(F / K)^4 / 1920

  AA <- alpha * (F * K)^(beta / 2) * A1 / A2

  ZZ <- (volvol / alpha) * (F * K)^((1 - beta) / 2) * log(F / K)
  XX <- log(((1 - 2 * rho * ZZ + ZZ^2)^0.5 - rho + ZZ) / (1 - rho))

  BB <- ifelse(abs(ZZ) > eps, ZZ / XX, 1.0)

  C1 <- -beta * (2 - beta) * alpha^2 / (24 * (F * K)^(1 - beta))
  C2 <- rho * alpha * volvol * beta / (4 * (F * K)^((1 - beta) / 2))
  C3 <- (2 - 3 * rho^2) * volvol^2 / 24

  CC <- 1 + (C1 + C2 + C3) * TT

  AA * BB * CC
}

shifted_normal_vol_hagan <- function(K, F, TT, beta, alpha, volvol, rho, shift = 0.025) {
  normal_vol_hagan(
    K = K + shift,
    F = F + shift,
    TT = TT,
    beta = beta,
    alpha = alpha,
    volvol = volvol,
    rho = rho
  )
}

# ------------------------------------------------------------
# 1. Black SABR calibration
# ------------------------------------------------------------

strikes_black <- c(0.06, 0.31, 0.56, 0.81, 1.06, 1.56, 2.56) / 100
market_vol_black <- c(90.78, 46.09, 45.30, 50.17, 53.85, 58.42, 62.72) / 100

forward_black <- 0.0056
maturity_black <- 2.0
beta_black <- 0.5
init_par_black <- c(alpha = 0.1, volvol = 0.1, rho = 0.1)

objective_sabr_black <- function(par) {
  alpha <- par[1]
  volvol <- par[2]
  rho <- par[3]

  if (alpha <= 0 || volvol < 0 || rho <= -0.9999 || rho >= 0.9999) {
    return(1e6)
  }

  calc_vol <- map_dbl(
    strikes_black,
    ~ safe_sabr_vol(.x, forward_black, maturity_black, alpha, beta_black, volvol, rho)
  )

  if (any(is.na(calc_vol))) {
    return(1e6)
  }

  rmse_vec(calc_vol, market_vol_black)
}

fit_black <- optim(
  par = init_par_black,
  fn = objective_sabr_black,
  method = "L-BFGS-B",
  lower = c(0.0001, 0.0, -0.9999),
  upper = c(Inf, Inf, 0.9999)
)

par_black <- fit_black$par

calc_vol_black <- map_dbl(
  strikes_black,
  ~ safe_sabr_vol(.x, forward_black, maturity_black, par_black[1], beta_black, par_black[2], par_black[3])
)

black_calibration_tbl <- tibble(
  parameter = c("alpha", "beta", "volvol", "rho", "rmse"),
  value = c(par_black[1], beta_black, par_black[2], par_black[3], fit_black$value)
)

qlr_show_tbl(
  black_calibration_tbl,
  "Black SABR calibration summary",
  n = 20
)

black_smile_tbl <- tibble(
  strike = strikes_black,
  market_vol = market_vol_black,
  sabr_vol = calc_vol_black
)

qlr_show_tbl(
  black_smile_tbl,
  "Black SABR smile table",
  n = 20
)

black_smile_plot_tbl <- black_smile_tbl |>
  tidyr::pivot_longer(
    cols = c(market_vol, sabr_vol),
    names_to = "series",
    values_to = "vol"
  )

black_smile_plot <- ggplot(
  black_smile_plot_tbl,
  aes(x = strike, y = vol, color = series, shape = series)
) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 2.2) +
  scale_x_continuous(labels = \(x) sprintf("%.2f%%", 100 * x)) +
  scale_y_continuous(labels = \(x) sprintf("%.0f%%", 100 * x)) +
  labs(
    title = "Black SABR calibration",
    subtitle = paste0(
      "alpha=", fmt_num(par_black[1], 5),
      ", beta=", fmt_num(beta_black, 3),
      ", volvol=", fmt_num(par_black[2], 5),
      ", rho=", fmt_num(par_black[3], 5)
    ),
    x = "Strike",
    y = "Black Vol"
  ) +
  theme_minimal()

print(black_smile_plot)

# ------------------------------------------------------------
# 2. SABR parameter sensitivity
# ------------------------------------------------------------

base_par_4 <- c(
  alpha = par_black[1],
  beta = beta_black,
  volvol = par_black[2],
  rho = par_black[3]
)

shift_par_4 <- c(
  alpha = 0.02,
  beta = 0.2,
  volvol = 0.4,
  rho = 0.44
)

param_names <- names(base_par_4)

sabr_sensitivity_tbl <- map_dfr(
  seq_along(base_par_4),
  function(i) {
    par_up <- base_par_4
    par_dn <- base_par_4

    par_up[i] <- par_up[i] + shift_par_4[i]
    par_dn[i] <- par_dn[i] - shift_par_4[i]

    tibble(
      strike = rep(strikes_black, 3),
      vol = c(
        map_dbl(strikes_black, ~ safe_sabr_vol(.x, forward_black, maturity_black, base_par_4[1], base_par_4[2], base_par_4[3], base_par_4[4])),
        map_dbl(strikes_black, ~ safe_sabr_vol(.x, forward_black, maturity_black, par_up[1], par_up[2], par_up[3], par_up[4])),
        map_dbl(strikes_black, ~ safe_sabr_vol(.x, forward_black, maturity_black, par_dn[1], par_dn[2], par_dn[3], par_dn[4]))
      ),
      scenario = rep(c("base", "up", "down"), each = length(strikes_black)),
      parameter = param_names[i],
      base_value = base_par_4[i],
      up_value = par_up[i],
      down_value = par_dn[i]
    )
  }
)

sabr_sensitivity_plot <- ggplot(
  sabr_sensitivity_tbl,
  aes(x = strike, y = vol, color = scenario, shape = scenario)
) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  facet_wrap(~ parameter, scales = "free_y") +
  scale_x_continuous(labels = \(x) sprintf("%.2f%%", 100 * x)) +
  scale_y_continuous(labels = \(x) sprintf("%.0f%%", 100 * x)) +
  labs(
    title = "SABR parameter sensitivity",
    x = "Strike",
    y = "Black Vol"
  ) +
  theme_minimal()

print(sabr_sensitivity_plot)

# ------------------------------------------------------------
# 3. Shifted SABR normal calibration
# ------------------------------------------------------------

strikes_normal <- c(-0.95, -0.45, -0.20, 0.05, 0.30, 0.55, 1.05) / 100
market_vol_normal <- c(41.94, 38.64, 37.71, 37.80, 39.05, 41.37, 47.81) / 10000

forward_normal <- 0.0005
maturity_normal <- 2.0
beta_normal <- 0.5
init_par_normal <- c(alpha = 0.1, volvol = 0.1, rho = 0.1)

objective_sabr_normal <- function(par) {
  alpha <- par[1]
  volvol <- par[2]
  rho <- par[3]

  if (alpha <= 0.001 || volvol < 0 || rho <= -0.999 || rho >= 0.999) {
    return(1e6)
  }

  calc_vol <- map_dbl(
    strikes_normal,
    ~ shifted_normal_vol_hagan(
      K = .x,
      F = forward_normal,
      TT = maturity_normal,
      beta = beta_normal,
      alpha = alpha,
      volvol = volvol,
      rho = rho
    )
  )

  if (any(is.na(calc_vol)) || any(!is.finite(calc_vol))) {
    return(1e6)
  }

  rmse_vec(calc_vol, market_vol_normal)
}

fit_normal <- optim(
  par = init_par_normal,
  fn = objective_sabr_normal,
  method = "L-BFGS-B",
  lower = c(0.001, 0.0, -0.999),
  upper = c(Inf, Inf, 0.999)
)

par_normal <- fit_normal$par

calc_vol_normal <- map_dbl(
  strikes_normal,
  ~ shifted_normal_vol_hagan(
    K = .x,
    F = forward_normal,
    TT = maturity_normal,
    beta = beta_normal,
    alpha = par_normal[1],
    volvol = par_normal[2],
    rho = par_normal[3]
  )
)

normal_calibration_tbl <- tibble(
  parameter = c("alpha", "beta", "volvol", "rho", "rmse"),
  value = c(par_normal[1], beta_normal, par_normal[2], par_normal[3], fit_normal$value)
)

qlr_show_tbl(
  normal_calibration_tbl,
  "Shifted SABR normal calibration summary",
  n = 20
)

normal_smile_tbl <- tibble(
  strike = strikes_normal,
  market_vol = market_vol_normal,
  sabr_vol = calc_vol_normal
)

qlr_show_tbl(
  normal_smile_tbl,
  "Shifted SABR normal smile table",
  n = 20
)

normal_smile_plot_tbl <- normal_smile_tbl |>
  tidyr::pivot_longer(
    cols = c(market_vol, sabr_vol),
    names_to = "series",
    values_to = "vol"
  )

normal_smile_plot <- ggplot(
  normal_smile_plot_tbl,
  aes(x = strike, y = vol, color = series, shape = series)
) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 2.2) +
  scale_x_continuous(labels = \(x) sprintf("%.2f%%", 100 * x)) +
  scale_y_continuous(labels = \(x) sprintf("%.2f%%", 100 * x)) +
  labs(
    title = "Shifted SABR normal calibration",
    subtitle = paste0(
      "alpha=", fmt_num(par_normal[1], 5),
      ", beta=", fmt_num(beta_normal, 3),
      ", volvol=", fmt_num(par_normal[2], 5),
      ", rho=", fmt_num(par_normal[3], 5)
    ),
    x = "Strike",
    y = "Normal Vol"
  ) +
  theme_minimal()

print(normal_smile_plot)

# ------------------------------------------------------------
# 4. OTM check
# ------------------------------------------------------------

otm_check_tbl <- tibble(
  strike = c(-0.0195, 0.0205),
  vol = c(
    shifted_normal_vol_hagan(-0.0195, forward_normal, maturity_normal, beta_normal, par_normal[1], par_normal[2], par_normal[3]),
    shifted_normal_vol_hagan(0.0205, forward_normal, maturity_normal, beta_normal, par_normal[1], par_normal[2], par_normal[3])
  )
)

qlr_show_tbl(
  otm_check_tbl,
  "Shifted SABR OTM check",
  n = 20
)

# ------------------------------------------------------------
# 5. Numerical Jacobian history
# ------------------------------------------------------------

jac_history <- list()

objective_sabr_normal_with_history <- function(par) {
  objective_sabr_normal(par)
}

callback_store_jac <- function(par) {
  jac_history[[length(jac_history) + 1L]] <<- approx_fprime_r(
    par = par,
    fn = objective_sabr_normal_with_history,
    epsilon = 1e-8
  )
}

fit_normal_hist <- optim(
  par = init_par_normal,
  fn = function(par) {
    value <- objective_sabr_normal_with_history(par)
    callback_store_jac(par)
    value
  },
  method = "L-BFGS-B",
  lower = c(0.001, 0.0, -0.999),
  upper = c(Inf, Inf, 0.999)
)

jac_history_mat <- do.call(rbind, jac_history)

jac_history_tbl <- as_tibble(jac_history_mat)
names(jac_history_tbl) <- c("d_alpha", "d_volvol", "d_rho")
jac_history_tbl <- jac_history_tbl |>
  mutate(iteration = row_number()) |>
  select(iteration, everything())

qlr_show_tbl(
  slice_head(jac_history_tbl, n = 10),
  "Jacobian history (top rows)",
  n = 20
)

qlr_show_tbl(
  slice_tail(jac_history_tbl, n = 5),
  "Jacobian history (last rows)",
  n = 20
)

jac_history_plot_tbl <- jac_history_tbl |>
  pivot_longer(
    cols = c(d_alpha, d_volvol, d_rho),
    names_to = "gradient",
    values_to = "value"
  )

jac_history_plot <- ggplot(
  jac_history_plot_tbl,
  aes(x = iteration, y = value, color = gradient)
) +
  geom_line(linewidth = 0.6) +
  labs(
    title = "Shifted SABR Jacobian history",
    x = "Iteration",
    y = "Numerical gradient"
  ) +
  theme_minimal()

print(jac_history_plot)

cat("\nchapter08 sabr model rewrite completed successfully.\n")
