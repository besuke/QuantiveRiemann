# ============================================================
# chapter04_bonds_rewrite.R
# ------------------------------------------------------------
# 第4章 Bonds
# Python / QuantLib notebook を、
# 「QuantiveRiemann + QuantLib(SWIG for R)」前提で書き直した版。
#
# 内容:
# 1. fixed-rate bond setup
# 2. clean price -> yield
# 3. yield -> clean / dirty price
# 4. duration / bpv / convexity
# 5. hand calculation checks
# 6. bond cashflow table
# 7. flat-yield pricing
# 8. TONA curve pricing
# 9. z-spread
# 10. asset swap
#
# 注意:
# - bondYield() は ver 1.39 以降で clean price wrapper が必要な場合あり
# - zSpread() も同様
# - AssetSwap() は RFR index 使用時、floating schedule を明示指定
#
# 前提:
# - devtools::load_all(".") 済み、または package install 済み
# - utility.R / wrapper_lib.R / bond.R の関数が使える
# - QuantLib SWIG build で Bond / OIS / AssetSwap まわりが利用可能
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

# ============================================================
# chapter04_bonds_rewrite.R
# ------------------------------------------------------------
# 第4章 Bonds
# Python / QuantLib notebook を、
# 「QuantiveRiemann + QuantLib(SWIG for R)」前提で書き直した版。
#
# 内容:
# 1. fixed-rate bond setup
# 2. clean price -> yield
# 3. yield -> clean / dirty price
# 4. duration / bpv / convexity
# 5. hand calculation checks
# 6. bond cashflow table
# 7. TONA curve for spread analysis
# 8. flat yield / TONA discounting
# 9. z-spread
# 10. asset swap
#
# 注意:
# - bondYield() は ver 1.39 以降で clean price wrapper が必要な build あり
# - zSpread() も同様
# - AssetSwap() は RFR index 使用時、floating schedule を明示指定
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
# 1. enums / calendars / counters
# ------------------------------------------------------------

jp_calendar <- Japan()

dc_a365 <- Actual365Fixed()
dc_a360 <- Actual360()
dc_30 <- Thirty360("BondBasis")

cmpd_compounded <- Compounding_Compounded_get()
cmpd_simple <- Compounding_Simple_get()

freq_annual <- Frequency_Annual_get()
freq_semiannual <- Frequency_Semiannual_get()

# ------------------------------------------------------------
# 2. fixed-rate bond setup
# ------------------------------------------------------------

trade_date <- qlr_date("2022-08-19")
settlement_days <- 2L
effective_date <- qlr_date("2022-07-28")
maturity_date <- qlr_date("2025-07-28")

face_amount <- 100
coupon_rate <- 0.00370
clean_price_input <- 97.0

qlr_set_eval_date(trade_date)
settlement_date <- qlr_advance_days(
  calendar_obj = jp_calendar,
  date_obj = trade_date,
  n_days = settlement_days
)

bond_res <- qlr_fixed_rate_bond(
  trade_date = trade_date,
  settlement_days = settlement_days,
  effective_date = effective_date,
  maturity_date = maturity_date,
  face_amount = face_amount,
  coupon_rate = coupon_rate,
  calendar = jp_calendar,
  schedule_frequency = qlr_period_months(6),
  accrual_day_counter = dc_30,
  payment_convention = "Unadjusted",
  maturity_convention = "Unadjusted",
  date_generation = "Backward",
  end_of_month = FALSE
)

bond_schedule <- bond_res$schedule
bond_obj <- bond_res$bond

qlr_show_tbl(
  qlr_schedule_dates(bond_schedule),
  "Bond schedule",
  n = 20
)

# ------------------------------------------------------------
# 3. clean price -> yield
# ------------------------------------------------------------

price_to_yield <- qlr_bond_yield_from_clean_price(
  bond = bond_obj,
  clean_price = clean_price_input,
  day_counter = dc_30,
  compounding = cmpd_compounded,
  frequency = freq_semiannual
)

cat(
  "Price 97.0 -> yield:",
  sprintf("%.6f%%", 100 * price_to_yield),
  "\n"
)

# ------------------------------------------------------------
# 4. yield -> clean / dirty price
# ------------------------------------------------------------

price_tbl <- qlr_bond_price_measures(
  bond = bond_obj,
  yield = price_to_yield,
  day_counter = dc_30,
  compounding = cmpd_compounded,
  frequency = freq_semiannual,
  settlement_date = settlement_date,
  schedule = bond_schedule
)

qlr_show_tbl(
  price_tbl,
  "Yield -> price summary",
  n = 20
)

dirty_price_from_yield <- price_tbl |>
  dplyr::filter(metric == "dirty_price") |>
  dplyr::pull(value)
# ------------------------------------------------------------
# 5. risk measures
# ------------------------------------------------------------

risk_tbl <- qlr_bond_risk_measures(
  bond = bond_obj,
  yield = price_to_yield,
  day_counter = dc_30,
  compounding = cmpd_compounded,
  frequency = freq_semiannual
)

qlr_show_tbl(
  risk_tbl,
  "Bond risk measures",
  n = 20
)


modified_duration <- risk_tbl |>
  dplyr::filter(metric == "modified_duration") |>
  dplyr::pull(value) |>
  as.numeric()

convexity_value <- risk_tbl |>
  dplyr::filter(metric == "convexity") |>
  dplyr::pull(value) |>
  as.numeric()

# ------------------------------------------------------------
# 6. hand calculation checks
# ------------------------------------------------------------

handcalc_tbl <- qlr_bond_risk_handcalc(
  bond = bond_obj,
  yield = price_to_yield,
  dirty_price = dirty_price_from_yield,
  modified_duration = modified_duration,
  convexity = convexity_value,
  day_counter = dc_30,
  compounding = cmpd_compounded,
  frequency = freq_semiannual
)

qlr_show_tbl(
  handcalc_tbl,
  "Hand calculations",
  n = 20
)

# ------------------------------------------------------------
# 7. bond cashflow table under flat yield
# ------------------------------------------------------------

flat_yield_handle <- qlr_bond_flat_forward_handle(
  settlement_date = settlement_date,
  rate = price_to_yield,
  day_counter = dc_30,
  compounding = cmpd_compounded,
  frequency = freq_semiannual
)

flat_engine <- DiscountingBondEngine(flat_yield_handle)
Instrument_setPricingEngine(bond_obj, flat_engine)

bond_cf_tbl_flat <- qlr_bond_cashflow_table_ql(
  bond = bond_obj,
  curve = flat_yield_handle
)

qlr_show_tbl(
  bond_cf_tbl_flat,
  "Bond cashflow table (flat yield discounting)",
  n = 20
)

cat(
  "Hand dirty price from CF table:",
  sprintf("%.8f", sum(bond_cf_tbl_flat$pv, na.rm = TRUE)),
  "\n"
)

macaulay_hand <- bond_cf_tbl_flat |>
  dplyr::filter(!is.na(pv), pay_date > qlr_iso(settlement_date)) |>
  dplyr::mutate(
    year_fraction = purrr::map_dbl(
      pay_date,
      ~ dc_30$yearFraction(settlement_date, qlr_date(.x))
    )
  ) |>
  dplyr::summarise(value = sum(year_fraction * pv) / dirty_price_from_yield) |>
  dplyr::pull(value)

cat(
  "Macaulay duration (hand):",
  sprintf("%.8f", macaulay_hand),
  "\n"
)

bond_cf_2025_tbl <- bond_cf_tbl_flat |>
  dplyr::filter(as.Date(pay_date) >= as.Date("2025-01-01"))

qlr_show_tbl(
  bond_cf_2025_tbl,
  "Bond cashflows from 2025 onward",
  n = 20
)

# ------------------------------------------------------------
# 8. TONA curve for spread analysis
# ※ ここは package 側に curve builder が入ったら差し替え
# ------------------------------------------------------------

make_tona_curve <- function(curve_data, trade_date = "2022-08-19") {
  trade_date_ql <- qlr_date(trade_date)
  qlr_set_eval_date(trade_date_ql)

  tona_curve_handle <- RelinkableYieldTermStructureHandle()
  tona_index <- OvernightIndex(
    "TONA",
    0,
    JPYCurrency(),
    jp_calendar,
    dc_a365,
    tona_curve_handle
  )

  helper_list <- purrr::map(curve_data, function(x) {
    kind <- as.character(x[[1]])
    tenor <- as.character(x[[2]])
    rate_pct <- as.numeric(x[[3]])

    if (kind == "depo") {
      return(
        DepositRateHelper(
          QuoteHandle(SimpleQuote(rate_pct / 100)),
          tona_index
        )
      )
    }

    if (kind == "swap") {
      return(
        OISRateHelper(
          2,
          qlr_period(tenor),
          QuoteHandle(SimpleQuote(rate_pct / 100)),
          tona_index
        )
      )
    }

    stop("Unsupported helper type: ", kind)
  })

  helper_vec <- RateHelperVector()
  purrr::walk(helper_list, ~ RateHelperVector_append(helper_vec, .x))

  tona_curve_obj <- PiecewiseLogLinearDiscount(
    0,
    jp_calendar,
    helper_vec,
    dc_a365
  )

  TermStructure_enableExtrapolation(tona_curve_obj)
  RelinkableYieldTermStructureHandle_linkTo(tona_curve_handle, tona_curve_obj)

  list(
    index = tona_index,
    curve = tona_curve_obj,
    curve_handle = tona_curve_handle,
    quote_tbl = tibble::tibble(
      kind = purrr::map_chr(curve_data, 1),
      tenor = purrr::map_chr(curve_data, 2),
      rate_pct = purrr::map_dbl(curve_data, ~ as.numeric(.x[[3]]))
    )
  )
}

tona_curve_data <- list(
  c("depo", "1d", -0.00900),
  c("swap", "1m", -0.01807),
  c("swap", "6m", -0.01043),
  c("swap", "12m", 0.01250),
  c("swap", "18m", 0.03125),
  c("swap", "2y", 0.04875),
  c("swap", "3y", 0.07375),
  c("swap", "5y", 0.11854),
  c("swap", "7y", 0.19146)
)

tona_bundle <- make_tona_curve(
  tona_curve_data,
  trade_date = "2022-08-19"
)

qlr_show_tbl(tona_bundle$quote_tbl, "TONA input quotes")
qlr_show_tbl(qlr_curve_tbl(tona_bundle$curve), "TONA curve table")

curve_proxy_tbl <- qlr_curve_tbl(tona_bundle$curve, n = 500)
mat_time <- dc_a365$yearFraction(
  tona_bundle$curve$referenceDate(),
  maturity_date
)

interp_rate <- approx(
  curve_proxy_tbl$time,
  curve_proxy_tbl$zero,
  xout = mat_time,
  rule = 2
)$y

cat(
  "Interpolated TONA rate at maturity:",
  sprintf("%.6f%%", 100 * interp_rate),
  ", bond yield:",
  sprintf("%.6f%%", 100 * price_to_yield),
  ", I-spread:",
  sprintf("%.6f%%", 100 * (price_to_yield - interp_rate)),
  "\n"
)

# ------------------------------------------------------------
# 9. discounting with flat forward and TONA curve
# ------------------------------------------------------------

flat_forward_price <- qlr_bond_npv_with_flat_yield(
  bond = bond_obj,
  settlement_date = settlement_date,
  rate = 0.01418713,
  day_counter = dc_30,
  compounding = cmpd_compounded,
  frequency = freq_semiannual
)

cat(
  "Dirty price with 1.418713% flat yield:",
  sprintf("%.6f", flat_forward_price),
  "\n"
)

tona_discount_price <- qlr_bond_npv_with_curve(
  bond = bond_obj,
  curve_handle = tona_bundle$curve_handle
)

cat(
  "Dirty price discounted on TONA curve:",
  sprintf("%.6f", tona_discount_price),
  "\n"
)

# ------------------------------------------------------------
# 10. z-spread / zero-spreaded curve
# ------------------------------------------------------------

z_spread_value <- qlr_bond_zspread(
  bond = bond_obj,
  clean_price = 97.0,
  curve = tona_bundle$curve,
  day_counter = dc_30,
  compounding = cmpd_compounded,
  frequency = freq_semiannual
)

cat(
  "z-spread for clean price 97.0:",
  sprintf("%.6f%%", 100 * z_spread_value),
  "\n"
)

z_spread_price_res <- qlr_bond_npv_with_zspread(
  bond = bond_obj,
  base_curve_handle = tona_bundle$curve_handle,
  z_spread = z_spread_value,
  compounding = cmpd_compounded,
  frequency = freq_annual,
  day_counter = dc_a365
)

cat(
  "Dirty price on TONA + z-spread curve:",
  sprintf("%.6f", z_spread_price_res$npv),
  "\n"
)

bond_cf_tbl_spread <- qlr_bond_cashflow_table_ql(
  bond = bond_obj,
  curve = z_spread_price_res$spread_curve_handle
)

qlr_show_tbl(
  bond_cf_tbl_spread,
  "Bond cashflow table (TONA + z-spread)",
  n = 20
)

cat(
  "Hand price on TONA + z-spread:",
  sprintf("%.6f", sum(bond_cf_tbl_spread$pv, na.rm = TRUE)),
  "\n"
)

# ------------------------------------------------------------
# 11. AS spread scenarios
# ------------------------------------------------------------

bond_cf_tbl_tona <- qlr_bond_cashflow_table_ql(
  bond = bond_obj,
  curve = tona_bundle$curve_handle
) |>
  dplyr::filter(cashflow_type == "coupon")

yr_frac_vec <- purrr::map_dbl(
  bond_cf_tbl_tona$pay_date,
  ~ dc_a365$yearFraction(settlement_date, qlr_date(.x))
)

tenor_flow <- diff(c(0, yr_frac_vec))
tona_annuity <- sum(bond_cf_tbl_tona$discount_factor * tenor_flow, na.rm = TRUE)

benchmark_dirty_price <- 90.0256

as_spread_input_tbl <- tibble::tibble(
  price_label = c("model_dirty_price", "benchmark_dirty_price"),
  target_dirty_price = c(dirty_price_from_yield, benchmark_dirty_price)
)

as_spread_result_tbl <- as_spread_input_tbl |>
  dplyr::mutate(
    discount_price = tona_discount_price,
    annuity = tona_annuity,
    as_spread = (discount_price - target_dirty_price) / annuity
  )

qlr_show_tbl(
  tibble::tibble(
    metric = c("tona_discount_price", "tona_annuity"),
    value = c(tona_discount_price, tona_annuity)
  ),
  "AS spread base inputs",
  n = 20
)

qlr_show_tbl(
  as_spread_result_tbl,
  "AS spread scenarios",
  n = 20
)

# ------------------------------------------------------------
# 12. AssetSwap
# ------------------------------------------------------------

tibor6m_for_asw <- Tibor(
  qlr_period_months(6),
  tona_bundle$curve_handle
)

asset_swap_tbl <- qlr_asset_swap_analysis(
  bond = bond_obj,
  clean_price = clean_price_asw,
  ibor_index = tibor6m_for_asw,
  spread = credit_spread_asw,
  settlement_date = settlement_date,
  maturity_date = maturity_date,
  calendar = jp_calendar,
  floating_schedule_frequency = qlr_period_years(1),
  payment_convention = "Unadjusted",
  date_generation = "Backward",
  end_of_month = FALSE,
  floating_day_counter = dc_a365,
  pay_fixed_rate = pay_fixed_rate,
  par_asset_swap = is_par_asset_swap,
  discount_curve_handle = tona_bundle$curve_handle
)
asset_swap_tbl_display <- asset_swap_tbl |>
  dplyr::mutate(
    value = purrr::map2_chr(metric, value, function(metric, x) {
      if (length(x) == 0 || all(is.na(x))) return(NA_character_)
      if (metric == "fair_spread") return(sprintf("%.6f%%", 100 * as.numeric(x[[1]])))
      if (metric == "fair_clean_price") return(sprintf("%.6f", as.numeric(x[[1]])))
      as.character(x[[1]])
    })
  )

qlr_show_tbl(asset_swap_tbl_display, "AssetSwap result", n = 20)
