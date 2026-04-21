# ============================================================
# QuantiveRiemann - short_rate.R
# Short-rate models: Hull-White / Vasicek / CIR
# ============================================================

# ------------------------------------------------------------
# 1. Hull-White
# ------------------------------------------------------------

qlr_sr_hw_discount <- function(
    curve_handle,
    a = 0.03,
    sigma = 0.01,
    t0 = 0,
    t1 = 5
) {
  hw <- qlr_make_hw(curve_handle, a = a, sigma = sigma)
  qlr_discount_bond(hw, t0, t1)
}

# ------------------------------------------------------------
# 2. Vasicek simulation
# ------------------------------------------------------------

qlr_sr_vasicek_path <- function(
    a = 0.1,
    b = 0.05,
    theta = 0.03,
    sigma = 0.01,
    r0 = 0.03,
    n = 100,
    horizon = 1
) {
  dt <- horizon / n
  r <- numeric(n)
  r[1] <- r0

  for (i in 2:n) {
    r[i] <- r[i - 1] +
      a * (theta - r[i - 1]) * dt +
      sigma * sqrt(dt) * stats::rnorm(1)
  }

  tibble::tibble(
    t = seq_len(n) * dt,
    r = r
  )
}

# ------------------------------------------------------------
# 3. CIR simulation
# ------------------------------------------------------------

qlr_sr_cir_path <- function(
    a = 0.1,
    b = 0.05,
    theta = 0.03,
    sigma = 0.02,
    r0 = 0.03,
    n = 100,
    horizon = 1
) {
  dt <- horizon / n
  r <- numeric(n)
  r[1] <- r0

  for (i in 2:n) {
    r[i] <- r[i - 1] +
      a * (theta - r[i - 1]) * dt +
      sigma * sqrt(abs(r[i - 1])) * sqrt(dt) * stats::rnorm(1)
  }

  tibble::tibble(
    t = seq_len(n) * dt,
    r = r
  )
}
