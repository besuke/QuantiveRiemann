# ============================================================
# chapter10_cds_rewrite.R
# ------------------------------------------------------------
# 第10章 CDS pricing
#
# Part 1
# 1. Flat discount / flat hazard CDS
# 2. MidPoint engine pricing
# 3. Cash-flow table
# 4. Hand check of coupon leg / default leg / fair spread
#
# Part 2
# 5. 5Y CDS with TONA discount curve
# 6. ISDA engine pricing
# 7. Cash-flow table
# 8. Hand check (midpoint-style approximation vs ISDA result)
# ============================================================

suppressPackageStartupMessages({
  library(QuantLib)
  library(dplyr)
  library(purrr)
  library(tibble)
  devtools::load_all(".")
})

# ------------------------------------------------------------
# 0. local helpers
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

schedule_date_vec <- function(schedule_obj) {
  purrr::map(
    seq_len(schedule_obj$size()),
    ~ qlr_schedule_date_at_local(schedule_obj, .x)
  )
}

make_schedule_tbl <- function(schedule_obj) {
  tibble(
    schedule_date = purrr::map_chr(
      seq_len(schedule_obj$size()),
      ~ qlr_iso(qlr_schedule_date_at_local(schedule_obj, .x))
    )
  )
}

mid_date_between <- function(dt1, dt2) {
  d1 <- as.Date(qlr_iso(dt1))
  d2 <- as.Date(qlr_iso(dt2))
  mid_r <- d1 + floor(as.numeric(d2 - d1) / 2)
  qlr_date(format(mid_r, "%Y-%m-%d"))
}

cds_buyer_side <- function() {
  out <- tryCatch(Side_Buyer_get(), error = function(e) NULL)
  if (!is.null(out)) return(out)

  out <- tryCatch(Protection_Buyer_get(), error = function(e) NULL)
  if (!is.null(out)) return(out)

  stop("Could not find CDS buyer-side enum in this QuantLib build.")
}

make_cds_cashflow_tbl_from_schedule <- function(
    cds_schedule,
    protection_start_date,
    coupon_rate,
    hazard_curve_obj,
    discount_curve_obj,
    trade_date,
    notional
) {
  sch_dates <- schedule_date_vec(cds_schedule)
  n_dates <- length(sch_dates)

  if (n_dates < 2L) {
    stop("cds_schedule must have at least 2 dates.")
  }

  n_rows <- n_dates
  pay_date <- vector("list", n_rows)
  accrual_start <- vector("list", n_rows)
  accrual_end <- vector("list", n_rows)
  coupon <- rep(NA_real_, n_rows)
  days <- rep(NA_real_, n_rows)
  YF <- rep(NA_real_, n_rows)
  amount <- rep(NA_real_, n_rows)

  pay_date[[1]] <- protection_start_date
  accrual_start[[1]] <- NULL
  accrual_end[[1]] <- protection_start_date

  for (i in 2:n_rows) {
    accrual_start[[i]] <- sch_dates[[i - 1L]]
    accrual_end[[i]] <- sch_dates[[i]]
    pay_date[[i]] <- sch_dates[[i]]
    coupon[i] <- coupon_rate

    acc_start_r <- as.Date(qlr_iso(accrual_start[[i]]))
    acc_end_r <- as.Date(qlr_iso(accrual_end[[i]]))
    day_count_days <- as.numeric(acc_end_r - acc_start_r)

    days[i] <- day_count_days
    YF[i] <- day_count_days / 365
    amount[i] <- notional * coupon_rate * day_count_days / 360
  }

  DF <- rep(NA_real_, n_rows)
  Q <- rep(NA_real_, n_rows)

  trade_date_r <- as.Date(qlr_iso(trade_date))

  for (i in seq_len(n_rows)) {
    dt_i <- accrual_end[[i]]

    if (is.null(dt_i)) {
      DF[i] <- 1.0
      Q[i] <- 1.0
    } else {
      dt_i_r <- as.Date(qlr_iso(dt_i))

      if (dt_i_r <= trade_date_r) {
        DF[i] <- 1.0
        Q[i] <- 1.0
      } else {
        DF[i] <- discount_curve_obj$discount(dt_i)
        Q[i] <- hazard_curve_obj$survivalProbability(dt_i)
      }
    }
  }

  dQ <- c(0, -diff(Q))

  m_date <- vector("list", n_rows)
  mDF <- rep(NA_real_, n_rows)
  mQ <- rep(NA_real_, n_rows)

  m_date[[1]] <- NULL

  for (i in 2:n_rows) {
    dt1 <- pay_date[[i - 1L]]
    dt2 <- accrual_end[[i]]

    if (!is.null(dt1) && !is.null(dt2)) {
      md <- mid_date_between(dt1, dt2)
      m_date[[i]] <- md
      mDF[i] <- discount_curve_obj$discount(md)
      mQ[i] <- hazard_curve_obj$survivalProbability(md)
    }
  }

  safe_iso <- function(x) {
    if (is.null(x)) return(NA_character_)
    qlr_iso(x)
  }

  tibble(
    payDate = vapply(pay_date, safe_iso, character(1)),
    coupon = coupon,
    accStt = vapply(accrual_start, safe_iso, character(1)),
    accEnd = vapply(accrual_end, safe_iso, character(1)),
    days = days,
    YF = YF,
    amount = amount,
    DF = DF,
    Q = Q,
    dQ = dQ,
    mDate = vapply(m_date, safe_iso, character(1)),
    mDF = mDF,
    mQ = mQ
  )
}

# ------------------------------------------------------------
# 1. Part 1: flat discount / flat hazard CDS
# ------------------------------------------------------------

trade_date <- qlr_date("2022-09-19")
qlr_set_eval_date(trade_date)

maturity_date <- qlr_date("2023-09-20")
notional <- 10000000
rf_rate <- 0.10
cds_quote <- 1.30 / 100
recovery_rate <- 0.40
buy_protection <- cds_buyer_side()

coupon_100bp <- 0.01
hazard_rate_input <- cds_quote / (1 - recovery_rate)

calendar_wk <- WeekendsOnly()
dc_a365 <- Actual365Fixed()
dc_a360 <- Actual360()

cds_schedule_1 <- Schedule(
  trade_date + 1L,
  maturity_date,
  qlr_period_months(3),
  calendar_wk,
  "Following",
  "Unadjusted",
  "Forward",
  FALSE
)

qlr_show_tbl(
  make_schedule_tbl(cds_schedule_1),
  "CDS schedule (flat example)",
  n = 20
)

discount_curve_1 <- FlatForward(
  trade_date,
  QuoteHandle(SimpleQuote(rf_rate)),
  dc_a365
)
discount_curve_handle_1 <- YieldTermStructureHandle(discount_curve_1)

hazard_curve_1 <- FlatHazardRate(
  trade_date,
  QuoteHandle(SimpleQuote(hazard_rate_input)),
  dc_a365
)
hazard_curve_handle_1 <- DefaultProbabilityTermStructureHandle(hazard_curve_1)

curve_check_tbl_1 <- tibble(
  metric = c(
    "discount_factor_at_maturity",
    "survival_probability_at_maturity",
    "hazard_rate_at_maturity"
  ),
  value = c(
    discount_curve_1$discount(maturity_date),
    hazard_curve_1$survivalProbability(maturity_date),
    hazard_curve_1$hazardRate(maturity_date)
  )
)

qlr_show_tbl(
  curve_check_tbl_1,
  "Flat curve / hazard check",
  n = 20
)

cds_obj_1 <- CreditDefaultSwap(
  buy_protection,
  notional,
  coupon_100bp,
  cds_schedule_1,
  "Following",
  dc_a360
)

mid_engine_1 <- MidPointCdsEngine(
  hazard_curve_handle_1,
  recovery_rate,
  discount_curve_handle_1
)

cds_obj_1$setPricingEngine(mid_engine_1)

implied_hazard_rate_1 <- cds_obj_1$impliedHazardRate(
  cds_obj_1$NPV(),
  discount_curve_handle_1,
  dc_a365,
  recovery_rate
)

cds_summary_tbl_1 <- tibble(
  metric = c(
    "coupon_leg_npv",
    "default_leg_npv",
    "npv",
    "implied_hazard_rate",
    "fair_spread"
  ),
  value = c(
    cds_obj_1$couponLegNPV(),
    cds_obj_1$defaultLegNPV(),
    cds_obj_1$NPV(),
    implied_hazard_rate_1,
    cds_obj_1$fairSpread()
  )
)

qlr_show_tbl(
  cds_summary_tbl_1,
  "CDS summary (MidPoint, flat curves)",
  n = 20
)

cds_npv0_obj_1 <- CreditDefaultSwap(
  buy_protection,
  notional,
  cds_quote,
  cds_schedule_1,
  "Following",
  dc_a360
)

implied_hazard_rate_zero_npv_1 <- cds_npv0_obj_1$impliedHazardRate(
  0.0,
  discount_curve_handle_1,
  dc_a365,
  recovery_rate
)

qlr_show_tbl(
  tibble(
    metric = c("quoted_spread", "implied_hazard_rate_zero_npv"),
    value = c(cds_quote, implied_hazard_rate_zero_npv_1)
  ),
  "Implied hazard from zero-NPV CDS",
  n = 20
)

df_cds_1 <- make_cds_cashflow_tbl_from_schedule(
  cds_schedule = cds_schedule_1,
  protection_start_date = cds_obj_1$protectionStartDate(),
  coupon_rate = coupon_100bp,
  hazard_curve_obj = hazard_curve_1,
  discount_curve_obj = discount_curve_1,
  trade_date = trade_date,
  notional = notional
)

qlr_show_tbl(
  df_cds_1,
  "CDS cash-flow table (flat example)",
  n = 20
)

hc_coupon_leg_1 <- -(df_cds_1$amount * df_cds_1$DF * df_cds_1$mQ) |>
  sum(na.rm = TRUE)

hc_default_leg_unit_1 <- (1 - recovery_rate) * (df_cds_1$mDF * df_cds_1$dQ) |>
  sum(na.rm = TRUE)

hc_npv_1 <- hc_coupon_leg_1 + hc_default_leg_unit_1 * notional

hc_rpv01_1 <- (df_cds_1$YF * df_cds_1$DF * df_cds_1$mQ) |>
  sum(na.rm = TRUE)

hc_fair_spread_1 <- hc_default_leg_unit_1 / (hc_rpv01_1 * 365 / 360)

hand_check_tbl_1 <- tibble(
  metric = c(
    "hc_coupon_leg_npv",
    "hc_default_leg_npv",
    "hc_npv",
    "hc_rpv01",
    "hc_fair_spread"
  ),
  value = c(
    hc_coupon_leg_1,
    hc_default_leg_unit_1 * notional,
    hc_npv_1,
    hc_rpv01_1,
    hc_fair_spread_1
  )
)

qlr_show_tbl(
  hand_check_tbl_1,
  "Hand check (flat example)",
  n = 20
)

cat("\nchapter10 CDS part 1 completed successfully.\n")

# ------------------------------------------------------------
# 2. Part 2: 5Y CDS with TONA curve and ISDA engine
# ------------------------------------------------------------

trade_date_2 <- qlr_date("2022-08-19")
qlr_set_eval_date(trade_date_2)

cds_tenor_2 <- qlr_period_years(5)
cds_quote_2 <- 30.426 / 10000
notional_2 <- 10000000
coupon_100bp_2 <- 0.01
recovery_rate_2 <- 0.40
buy_protection_2 <- cds_buyer_side()

calendar_jp <- WeekendsOnly()

jpn_holidays <- c(
  qlr_date("2024-03-20"), qlr_date("2025-03-20"), qlr_date("2026-03-20"),
  qlr_date("2027-03-22"), qlr_date("2026-09-21"), qlr_date("2026-09-22"),
  qlr_date("2026-09-23")
)

purrr::walk(
  jpn_holidays,
  ~ tryCatch(calendar_jp$addHoliday(.x), error = function(e) NULL)
)

maturity_date_2 <- cdsMaturity(
  trade_date_2,
  cds_tenor_2,
  "CDS2015"
)

cds_schedule_2 <- Schedule(
  trade_date_2,
  maturity_date_2,
  qlr_period_months(3),
  calendar_jp,
  "Following",
  "Unadjusted",
  "CDS2015",
  FALSE
)

qlr_show_tbl(
  make_schedule_tbl(cds_schedule_2),
  "CDS schedule (5Y Sony-like example)",
  n = 30
)

tona_quotes <- tibble::tribble(
  ~kind,  ~tenor, ~rate_pct,
  "depo", "1d",   -0.009,
  "ois",  "1m",   -0.01807,
  "ois",  "6m",   -0.01043,
  "ois",  "12m",   0.01250,
  "ois",  "18m",   0.03125,
  "ois",  "2y",    0.04875,
  "ois",  "3y",    0.07375,
  "ois",  "5y",    0.11854,
  "ois",  "7y",    0.19146
) |>
  dplyr::mutate(
    as_of_date = as.Date("2022-08-19"),
    currency = "JPY",
    instrument = "TONA",
    rate = rate_pct / 100
  ) |>
  dplyr::select(as_of_date, currency, instrument, kind, tenor, rate)

qlr_show_tbl(
  tona_quotes,
  "TONA input quotes",
  n = 20
)

tona_bundle <- qlr_ir_build_ois_curve_envs(
  quotes = tona_quotes,
  trade_date = "2022-08-19",
  verbose = TRUE
)[[1]]

discount_curve_2 <- tona_bundle$curve
discount_curve_handle_2 <- tona_bundle$curve_handle

pricing_date_2 <- discount_curve_2$referenceDate()
qlr_set_eval_date(pricing_date_2)

cds_npv0_obj_2 <- CreditDefaultSwap(
  buy_protection_2,
  notional_2,
  cds_quote_2,
  cds_schedule_2,
  "Following",
  dc_a360,
  TRUE,
  TRUE,
  trade_date_2,
  FaceValueClaim(),
  Actual360()
)

hazard_rate_2 <- cds_npv0_obj_2$impliedHazardRate(
  0.0,
  discount_curve_handle_2,
  dc_a365,
  recovery_rate_2,
  1e-10,
  "ISDA"
)

hazard_curve_2 <- FlatHazardRate(
  0L,
  calendar_jp,
  QuoteHandle(SimpleQuote(hazard_rate_2)),
  dc_a365
)

hazard_curve_handle_2 <- DefaultProbabilityTermStructureHandle(hazard_curve_2)

curve_check_tbl_2 <- tibble(
  metric = c("maturity_date", "hazard_rate"),
  value = c(NA_real_, hazard_rate_2),
  text = c(qlr_iso(maturity_date_2), NA_character_)
)

qlr_show_tbl(
  curve_check_tbl_2,
  "Maturity / hazard check (5Y example)",
  n = 20
)

cds_obj_2 <- CreditDefaultSwap(
  buy_protection_2,
  notional_2,
  coupon_100bp_2,
  cds_schedule_2,
  "Following",
  dc_a360,
  TRUE,
  TRUE,
  trade_date_2,
  FaceValueClaim(),
  Actual360()
)

isda_engine_2 <- IsdaCdsEngine(
  hazard_curve_handle_2,
  recovery_rate_2,
  discount_curve_handle_2
)

cds_obj_2$setPricingEngine(isda_engine_2)

implied_hazard_rate_2 <- cds_obj_2$impliedHazardRate(
  cds_obj_2$NPV(),
  discount_curve_handle_2,
  dc_a365,
  recovery_rate_2
)

net_two_leg_2 <- cds_obj_2$couponLegNPV() +
  cds_obj_2$defaultLegNPV() +
  cds_obj_2$accrualRebateNPV()

settle_amount_2 <- cds_obj_2$fairUpfront() * notional_2 +
  cds_obj_2$accrualRebate()$amount() * (-1)

cds_summary_tbl_2 <- tibble(
  metric = c(
    "coupon_leg_npv",
    "accrual_rebate_npv",
    "default_leg_npv",
    "npv",
    "check_sum",
    "implied_hazard_rate",
    "fair_spread",
    "fair_upfront",
    "upfront_amount",
    "accrual_rebate_amount",
    "settle_amount"
  ),
  value = c(
    cds_obj_2$couponLegNPV(),
    cds_obj_2$accrualRebateNPV(),
    cds_obj_2$defaultLegNPV(),
    cds_obj_2$NPV(),
    net_two_leg_2,
    implied_hazard_rate_2,
    cds_obj_2$fairSpread(),
    cds_obj_2$fairUpfront(),
    cds_obj_2$fairUpfront() * notional_2,
    cds_obj_2$accrualRebate()$amount() * (-1),
    settle_amount_2
  )
)

qlr_show_tbl(
  cds_summary_tbl_2,
  "CDS summary (ISDA, 5Y example)",
  n = 30
)

df_cds_2 <- make_cds_cashflow_tbl_from_schedule(
  cds_schedule = cds_schedule_2,
  protection_start_date = cds_obj_2$protectionStartDate(),
  coupon_rate = coupon_100bp_2,
  hazard_curve_obj = hazard_curve_2,
  discount_curve_obj = discount_curve_2,
  trade_date = pricing_date_2,
  notional = notional_2
)

qlr_show_tbl(
  df_cds_2,
  "CDS cash-flow table (5Y example)",
  n = 30
)

# midpoint-style approximation for comparison with ISDA engine
# exact equality is not expected
hc_coupon_leg_2 <- -(df_cds_2$amount * df_cds_2$DF * df_cds_2$mQ) |>
  sum(na.rm = TRUE)

hc_default_leg_unit_2 <- (1 - recovery_rate_2) * (df_cds_2$mDF * df_cds_2$dQ) |>
  sum(na.rm = TRUE)

hc_npv_2 <- hc_coupon_leg_2 +
  hc_default_leg_unit_2 * notional_2 +
  cds_obj_2$accrualRebateNPV()

hc_rpv01_2 <- ((92 - 61) / 365) * (df_cds_2$DF[2] * df_cds_2$mQ[2]) +
  sum(
    df_cds_2$YF[3:nrow(df_cds_2)] *
      df_cds_2$DF[3:nrow(df_cds_2)] *
      df_cds_2$mQ[3:nrow(df_cds_2)],
    na.rm = TRUE
  )

hc_fair_spread_2 <- hc_default_leg_unit_2 / (hc_rpv01_2 * 365 / 360)

hand_check_tbl_2 <- tibble(
  metric = c(
    "hc_coupon_leg_npv",
    "hc_default_leg_npv",
    "hc_npv",
    "hc_rpv01",
    "hc_fair_spread"
  ),
  value = c(
    hc_coupon_leg_2,
    hc_default_leg_unit_2 * notional_2,
    hc_npv_2,
    hc_rpv01_2,
    hc_fair_spread_2
  )
)

qlr_show_tbl(
  hand_check_tbl_2,
  "Hand check (5Y example)",
  n = 20
)

cat("\nchapter10 cds rewrite completed successfully.\n")
