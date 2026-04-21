# ============================================================
# QuantiveRiemann - demo_ois_curve_and_swap.R
# ============================================================

suppressMessages({
  library(QuantLib)
  library(tidyverse)
})

library(QuantLib)

devtools::load_all(".")

.libPaths(c("~/Library/R/arm64/4.5/library", .libPaths()))

library(QuantLib)
dir("~/Library/R", recursive = FALSE)

dir("~/Library/R/arm64", recursive = FALSE)

dir("~/Library/R/x86_64", recursive = FALSE)
------------------------------------------------------------
# 1. Demo market quotes
# ------------------------------------------------------------

make_ois_quotes <- function(trade_date = "2024-09-19") {
  tibble::tribble(
    ~currency, ~instrument, ~tenor, ~rate,
    "JPY", "TONA",  "1M",  0.0010,
    "JPY", "TONA",  "3M",  0.0012,
    "JPY", "TONA",  "6M",  0.0015,
    "JPY", "TONA",  "1Y",  0.0020,
    "JPY", "TONA",  "2Y",  0.0030,
    "JPY", "TONA",  "3Y",  0.0040,
    "JPY", "TONA",  "5Y",  0.0060,
    "JPY", "TONA",  "7Y",  0.0075,
    "JPY", "TONA",  "10Y", 0.0090,
    "JPY", "TONA",  "15Y", 0.0105,
    "JPY", "TONA",  "20Y", 0.0115,
    "JPY", "TONA",  "30Y", 0.0125,
    "JPY", "TONA",  "40Y", 0.0130,
    "USD", "SOFR",  "1M",  0.0530,
    "USD", "SOFR",  "3M",  0.0525,
    "USD", "SOFR",  "6M",  0.0518,
    "USD", "SOFR",  "1Y",  0.0505,
    "USD", "SOFR",  "2Y",  0.0475,
    "USD", "SOFR",  "3Y",  0.0455,
    "USD", "SOFR",  "5Y",  0.0425,
    "USD", "SOFR",  "7Y",  0.0408,
    "USD", "SOFR",  "10Y", 0.0395,
    "USD", "SOFR",  "15Y", 0.0388,
    "USD", "SOFR",  "20Y", 0.0385,
    "USD", "SOFR",  "30Y", 0.0380,
    "USD", "SOFR",  "40Y", 0.0375,
    "EUR", "ESTR",  "1M",  0.0310,
    "EUR", "ESTR",  "3M",  0.0308,
    "EUR", "ESTR",  "6M",  0.0305,
    "EUR", "ESTR",  "1Y",  0.0300,
    "EUR", "ESTR",  "2Y",  0.0285,
    "EUR", "ESTR",  "3Y",  0.0275,
    "EUR", "ESTR",  "5Y",  0.0260,
    "EUR", "ESTR",  "7Y",  0.0255,
    "EUR", "ESTR",  "10Y", 0.0250,
    "EUR", "ESTR",  "15Y", 0.0248,
    "EUR", "ESTR",  "20Y", 0.0247,
    "EUR", "ESTR",  "30Y", 0.0245,
    "EUR", "ESTR",  "40Y", 0.0244,
    "GBP", "SONIA", "1M",  0.0500,
    "GBP", "SONIA", "3M",  0.0495,
    "GBP", "SONIA", "6M",  0.0488,
    "GBP", "SONIA", "1Y",  0.0480,
    "GBP", "SONIA", "2Y",  0.0455,
    "GBP", "SONIA", "3Y",  0.0438,
    "GBP", "SONIA", "5Y",  0.0415,
    "GBP", "SONIA", "7Y",  0.0402,
    "GBP", "SONIA", "10Y", 0.0392,
    "GBP", "SONIA", "15Y", 0.0387,
    "GBP", "SONIA", "20Y", 0.0383,
    "GBP", "SONIA", "30Y", 0.0378,
    "GBP", "SONIA", "40Y", 0.0374
  ) |>
    dplyr::mutate(
      as_of_date = as.Date(trade_date),
      kind = "ois"
    ) |>
    dplyr::select(as_of_date, currency, instrument, kind, tenor, rate)
}

jpy_fixings_tbl <- tibble::tribble(
  ~date,         ~currency, ~instrument, ~fixing,
  "2024-09-17",  "JPY",     "TONA",      0.0010,
  "2024-09-18",  "JPY",     "TONA",      0.0010,
  "2024-09-19",  "JPY",     "TONA",      0.0011
)

gbp_fixings_tbl <- tibble::tribble(
  ~date,         ~currency, ~instrument, ~fixing,
  "2024-09-17",  "GBP",     "SONIA",     0.0503,
  "2024-09-18",  "GBP",     "SONIA",     0.0502,
  "2024-09-19",  "GBP",     "SONIA",     0.0501
)

# ------------------------------------------------------------
# 2. OIS quotes -> curve envs -> curve tables
# ------------------------------------------------------------

trade_date <- "2024-09-19"
ois_quotes <- make_ois_quotes(trade_date)

message("=== OIS market quotes ===")
print(dplyr::slice_head(ois_quotes, n = 12))

ois_curve_envs <- qlr_ir_build_ois_curve_envs(
  quotes = ois_quotes,
  trade_date = trade_date,
  verbose = TRUE
)

message("=== built OIS curves ===")
print(names(ois_curve_envs))

curve_tables <- qlr_ir_build_ois_curve_tables(ois_curve_envs)

jpy_ois_curve_env <- ois_curve_envs[["JPY::TONA"]]
gbp_ois_curve_env <- ois_curve_envs[["GBP::SONIA"]]

jpy_quotes <- ois_quotes |>
  dplyr::filter(currency == "JPY", instrument == "TONA")

jpy_curve_summary_tbl <- qlr_ir_show_curve_summary_table(
  curve_env = jpy_ois_curve_env,
  curve_tables = curve_tables[["JPY::TONA"]],
  input_quotes = jpy_quotes,
  title = "JPY::TONA swap / zero / DF / forward summary"
)

# ------------------------------------------------------------
# 3. JPY 20Y IRS-like trade
# ------------------------------------------------------------

jpy_irs_20y_trade <- qlr_trade_irs_ois_swap(
  curve_env = jpy_ois_curve_env,
  effective = "2024-09-19",
  maturity = "2044-09-19",
  fixed_rate = 0.0115,
  verbose = TRUE
)

message("=== JPY fixed leg cashflows BEFORE fixings ===")
print(
  qlr_swap_fixed_leg_table(jpy_irs_20y_trade$swap, jpy_ois_curve_env$curve) |>
    dplyr::slice_head(n = 8)
)

message("=== JPY floating leg cashflows BEFORE fixings ===")
jpy_float_before_tbl <- qlr_swap_float_leg_table(
  jpy_irs_20y_trade$swap,
  jpy_ois_curve_env$curve
)
print(dplyr::slice_head(jpy_float_before_tbl, n = 8))

qlr_ir_apply_fixings(
  index_obj = jpy_irs_20y_trade$index,
  fixings_tbl = jpy_fixings_tbl,
  currency = "JPY",
  instrument = "TONA"
)

message("=== JPY floating leg cashflows AFTER fixings ===")
jpy_float_after_tbl <- qlr_swap_float_leg_table(
  jpy_irs_20y_trade$swap,
  jpy_ois_curve_env$curve
)
print(dplyr::slice_head(jpy_float_after_tbl, n = 8))

qlr_ir_show_fixing_before_after(
  before_tbl = jpy_float_before_tbl,
  after_tbl = jpy_float_after_tbl,
  title = "JPY TONA fixing before / after",
  n = 8
)

qlr_ir_show_swap_summary(
  trade_env = jpy_irs_20y_trade,
  title = "JPY 20Y IRS summary AFTER fixings"
)

# ------------------------------------------------------------
# 4. JPY 20Y true OIS trade
# ------------------------------------------------------------

jpy_ois_20y_trade <- qlr_trade_ois_swap(
  curve_env = jpy_ois_curve_env,
  effective = "2024-09-19",
  maturity = "2044-09-19",
  fixed_rate = 0.0115,
  verbose = TRUE
)

qlr_ir_show_swap_summary(
  trade_env = jpy_ois_20y_trade,
  title = "JPY 20Y OIS summary"
)

jpy_ois_20y_fixed_cf_tbl <- qlr_swap_fixed_leg_table(
  jpy_ois_20y_trade$swap,
  jpy_ois_curve_env$curve
)

jpy_ois_20y_float_cf_tbl <- qlr_swap_float_leg_table(
  jpy_ois_20y_trade$swap,
  jpy_ois_curve_env$curve
)

message("=== JPY 20Y OIS fixed leg cashflows ===")
print(dplyr::slice_head(jpy_ois_20y_fixed_cf_tbl, n = 10))

message("=== JPY 20Y OIS floating leg cashflows ===")
print(dplyr::slice_head(jpy_ois_20y_float_cf_tbl, n = 10))

# ------------------------------------------------------------
# 5. GBP 20Y IRS-like trade and fixing diagnostics
# ------------------------------------------------------------

gbp_irs_20y_trade <- qlr_trade_irs_ois_swap(
  curve_env = gbp_ois_curve_env,
  effective = "2024-09-17",
  maturity = "2044-09-17",
  fixed_rate = 0.0383,
  verbose = TRUE
)

qlr_ir_apply_fixings(
  index_obj = gbp_irs_20y_trade$index,
  fixings_tbl = gbp_fixings_tbl,
  currency = "GBP",
  instrument = "SONIA"
)

message("=== GBP SONIA fixings ===")
print(gbp_fixings_tbl)

message("=== GBP SONIA fixing diagnostics ===")
print(
  qlr_ir_fixing_table(
    swap = gbp_irs_20y_trade$swap,
    curve = gbp_ois_curve_env$curve,
    index = gbp_irs_20y_trade$index
  ) |>
    dplyr::slice_head(n = 10)
)
