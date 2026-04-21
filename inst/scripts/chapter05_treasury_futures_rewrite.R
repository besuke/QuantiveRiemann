# ============================================================
# chapter05_treasury_futures_rewrite.R
# ------------------------------------------------------------
# 第5章 Treasury Futures
# Python / QuantLib notebook を、
# 「QuantiveRiemann + QuantLib(SWIG for R)」前提で書き直した版。
#
# 内容:
# 1. US Treasury bond setup
# 2. price from yield
# 3. accrued interest hand calculation
# 4. deliverable basket / gross basis
# 5. net basis / carry / implied repo
# 6. deliverable basket summary
#
# 前提:
# - devtools::load_all(".") 済み、または package install 済み
# - utility.R / wrapper_lib.R / bond.R の関数が使える
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
devtools::load_all(".")

# ------------------------------------------------------------
# 1. local formatting helper
# ------------------------------------------------------------

fmt_pct <- function(x, digits = 4) {
  sprintf(paste0("%.", digits, "f%%"), 100 * x)
}

# ------------------------------------------------------------
# 2. calendars / counters / enums
# ------------------------------------------------------------

cal_us_gov <- UnitedStates("GovernmentBond")
dc_act_act_bond <- ActualActual("Bond")
dc_act_360 <- Actual360()

cmpd_compounded <- Compounding_Compounded_get()
freq_semiannual <- Frequency_Semiannual_get()

settlement_days_t1 <- 1L

# ------------------------------------------------------------
# 3. US Treasury bond helper example
# ------------------------------------------------------------

bond_bundle <- qlr_us_treasury_bond(
  effective_date = "2022-09-30",
  maturity_date = "2027-09-30",
  coupon_rate_pct = 4.125,
  face_amount = 100,
  settlement_days = settlement_days_t1,
  calendar = cal_us_gov,
  day_counter = dc_act_act_bond
)

bond_obj <- bond_bundle$bond

qlr_show_tbl(
  qlr_schedule_dates(bond_bundle$schedule),
  "US Treasury bond schedule",
  n = 20
)

cf_prc <- bond_obj$cleanPrice(
  6 / 100,
  dc_act_act_bond,
  cmpd_compounded,
  freq_semiannual,
  qlr_date("2023-07-06")
)

cat(
  "Yield 6%, settle 2023-07-06 clean price:",
  qlr_fmt_num(cf_prc, 4),
  "\n"
)

# ------------------------------------------------------------
# 4. Yield 3.70% -> price
# ------------------------------------------------------------

trade_date <- qlr_date("2023-04-20")
bond_yield <- 3.7 / 100
futures_price <- 109 + 10 / 32

qlr_set_eval_date(trade_date)

settle_date <- qlr_advance_days(
  calendar_obj = cal_us_gov,
  date_obj = trade_date,
  n_days = settlement_days_t1
)

accrued_amount <- bond_obj$accruedAmount(settle_date)

clean_price <- bond_obj$cleanPrice(
  bond_yield,
  dc_act_act_bond,
  cmpd_compounded,
  freq_semiannual,
  settle_date
)

i_rate_obj <- InterestRate(
  bond_yield,
  dc_act_act_bond,
  cmpd_compounded,
  freq_semiannual
)

qlr_show_tbl(
  tibble::tibble(
    settle_date = qlr_iso(settle_date),
    bond_yield = bond_yield,
    accrued_amount = accrued_amount,
    clean_price = clean_price
  ),
  "Bond price from yield",
  n = 20
)

bond_cf_tbl <- qlr_bond_cashflow_table_ql(bond_obj) |>
  dplyr::mutate(
    pay_date_r = as.Date(pay_date),
    settle_date_r = as.Date(qlr_iso(settle_date)),
    discount_factor = dplyr::if_else(
      pay_date_r >= settle_date_r,
      purrr::map_dbl(
        pay_date,
        ~ tryCatch(
          i_rate_obj$discountFactor(settle_date, qlr_date(.x)),
          error = function(e) NA_real_
        )
      ),
      NA_real_
    ),
    pv = amount * discount_factor
  ) |>
  dplyr::select(
    pay_date,
    coupon_rate,
    accrual_start,
    accrual_end,
    amount,
    discount_factor,
    pv,
    cashflow_type
  )
tmp_cf <- qlr_bond_cashflow_table_ql(bond_obj)

tmp_cf

str(tmp_cf)

qlr_show_tbl(
  bond_cf_tbl,
  "Bond cashflow table (top rows)",
  n = 3
)

# ------------------------------------------------------------
# 5. Accrued interest hand calculation
# ------------------------------------------------------------

mar31 <- qlr_date("2023-03-31")
apr21 <- qlr_date("2023-04-21")
sep30 <- qlr_date("2023-09-30")

days_to_apr21 <- dc_act_act_bond$dayCount(mar31, apr21)
days_coupon_period <- dc_act_act_bond$dayCount(mar31, sep30)
accrued_interest_hand <- 4.125 / 2 * days_to_apr21 / days_coupon_period

qlr_show_tbl(
  tibble::tibble(
    days_to_apr21 = days_to_apr21,
    days_coupon_period = days_coupon_period,
    accrued_interest_hand = accrued_interest_hand
  ),
  "Accrued interest hand calculation",
  n = 20
)

# ------------------------------------------------------------
# 6. Deliverable basket / gross basis
# ------------------------------------------------------------

deliverable_tbl <- tibble::tribble(
  ~issue_date,   ~maturity_date, ~coupon_rate_pct, ~conversion_factor, ~market_yield_pct,
  "2022-09-30", "2027-09-30", 4.125,             0.9305,             3.70,
  "2022-08-31", "2027-08-31", 3.125,             0.8953,             3.69,
  "2023-01-31", "2028-01-31", 3.500,             0.9011,             3.65
)

gross_basis_tbl <- purrr::pmap_dfr(
  deliverable_tbl,
  ~ qlr_bond_futures_gross_basis_row(
    issue_date = ..1,
    maturity_date = ..2,
    coupon_rate_pct = ..3,
    conversion_factor = ..4,
    market_yield_pct = ..5,
    settlement_date = settle_date,
    futures_price = futures_price,
    settlement_days = settlement_days_t1,
    calendar = cal_us_gov,
    day_counter = dc_act_act_bond,
    compounding = cmpd_compounded,
    frequency = freq_semiannual
  )
)

cat(
  "Futures price:", qlr_fmt_num(futures_price, 6),
  ", spot settlement date:", qlr_iso(settle_date),
  "\n"
)

qlr_show_tbl(
  gross_basis_tbl |>
    dplyr::select(-bond_obj),
  "Gross basis table",
  n = 20
)

# ------------------------------------------------------------
# 7. Net basis / forward / implied repo
# ------------------------------------------------------------

repo_rate <- 5.10 / 100
repo_end_date <- qlr_date("2023-07-06")
repo_year_frac <- dc_act_360$yearFraction(settle_date, repo_end_date)
repo_days <- dc_act_360$dayCount(settle_date, repo_end_date)

net_basis_tbl <- purrr::pmap_dfr(
  list(
    bond_obj = gross_basis_tbl$bond_obj,
    conversion_factor = gross_basis_tbl$conversion_factor,
    clean_price = gross_basis_tbl$clean_price,
    dirty_price = gross_basis_tbl$dirty_price,
    gross_basis = gross_basis_tbl$gross_basis
  ),
  ~ qlr_bond_futures_net_basis_row(
    bond_obj = ..1,
    conversion_factor = ..2,
    clean_price = ..3,
    dirty_price = ..4,
    gross_basis = ..5,
    settlement_date = settle_date,
    repo_end_date = repo_end_date,
    repo_rate = repo_rate,
    repo_day_counter = dc_act_360,
    carry_day_counter = dc_act_360,
    futures_price = futures_price
  )
)

cat(
  "Repo rate:", fmt_pct(repo_rate),
  ", repo end date:", qlr_iso(repo_end_date),
  ", repo days:", repo_days,
  ", repo year fraction:", qlr_fmt_num(repo_year_frac, 4),
  "\n"
)

qlr_show_tbl(
  net_basis_tbl,
  "Net basis table",
  n = 20
)

# ------------------------------------------------------------
# 8. Combined deliverable basket view
# ------------------------------------------------------------

basket_summary_tbl <- gross_basis_tbl |>
  dplyr::select(-bond_obj) |>
  dplyr::bind_cols(
    net_basis_tbl |>
      dplyr::select(-dirty_price)
  ) |>
  dplyr::mutate(
    ctd_proxy = rank(net_basis, ties.method = "first") == 1
  ) |>
  dplyr::arrange(net_basis)
qlr_show_tbl(
  basket_summary_tbl,
  "Deliverable basket summary",
  n = 20
)

cat("\nchapter05 treasury futures rewrite completed successfully.\n")
