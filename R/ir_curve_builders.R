# ============================================================
# QuantiveRiemann - ir_curve_builders.R
# Interest-rate curve builders
# Internal contract:
#   input rates must be decimal rates
#   e.g. 3.00% -> 0.03
# ============================================================

# ------------------------------------------------------------
# 1. Common helpers
# ------------------------------------------------------------

.qlr_ir_validate_curve_data <- function(curve_data) {
  required_cols <- c("kind", "tenor", "rate")
  missing_cols <- setdiff(required_cols, names(curve_data))

  if (length(missing_cols) > 0) {
    stop("curve_data is missing columns: ", paste(missing_cols, collapse = ", "))
  }

  invisible(TRUE)
}

.qlr_ir_normalize_curve_data <- function(curve_data) {
  curve_data |>
    dplyr::transmute(
      kind = tolower(trimws(kind)),
      tenor = toupper(trimws(tenor)),
      rate = as.numeric(rate)
    )
}

.qlr_ir_make_deposit_helper <- function(rate, tenor, calendar, fixing_days, day_counter) {
  DepositRateHelper(
    QuoteHandle(SimpleQuote(rate)),
    qlr_period(tenor),
    fixing_days,
    calendar,
    "ModifiedFollowing",
    FALSE,
    day_counter
  )
}

.qlr_ir_make_ois_helper <- function(rate, tenor, settlement_days, index) {
  OISRateHelper(
    settlement_days,
    qlr_period(tenor),
    QuoteHandle(SimpleQuote(rate)),
    index
  )
}

.qlr_ir_make_swap_helper <- function(rate, tenor, calendar, fixed_freq, fixed_bdc, day_counter, index) {
  SwapRateHelper(
    QuoteHandle(SimpleQuote(rate)),
    qlr_period(tenor),
    calendar,
    fixed_freq,
    fixed_bdc,
    day_counter,
    index
  )
}

# ------------------------------------------------------------
# 2. OIS conventions / builders
# ------------------------------------------------------------

.qlr_ir_get_ois_convention <- function(currency, instrument) {
  currency <- toupper(trimws(currency))
  instrument <- toupper(trimws(instrument))

  key <- paste(currency, instrument, sep = "::")

  switch(
    key,
    "JPY::TONA" = list(
      currency = "JPY",
      instrument = "TONA",
      calendar = Japan(),
      day_counter = Actual365Fixed(),
      fixing_days = 2,
      settlement_days = 2
    ),
    "USD::SOFR" = list(
      currency = "USD",
      instrument = "SOFR",
      calendar = UnitedStates("SOFR"),
      day_counter = Actual360(),
      fixing_days = 2,
      settlement_days = 2
    ),
    "EUR::ESTR" = list(
      currency = "EUR",
      instrument = "ESTR",
      calendar = TARGET(),
      day_counter = Actual360(),
      fixing_days = 2,
      settlement_days = 2
    ),
    "GBP::SONIA" = list(
      currency = "GBP",
      instrument = "SONIA",
      calendar = UnitedKingdom("Settlement"),
      day_counter = Actual365Fixed(),
      fixing_days = 0,
      settlement_days = 0
    ),
    stop("Unsupported OIS convention for ", currency, " / ", instrument)
  )
}

.qlr_ir_make_ois_index <- function(currency, instrument, curve_handle) {
  currency <- toupper(trimws(currency))
  instrument <- toupper(trimws(instrument))

  key <- paste(currency, instrument, sep = "::")

  switch(
    key,
    "JPY::TONA" = OvernightIndex(
      "TONA",
      2,
      JPYCurrency(),
      Japan(),
      Actual365Fixed(),
      curve_handle
    ),
    "USD::SOFR" = Sofr(curve_handle),
    "EUR::ESTR" = Estr(curve_handle),
    "GBP::SONIA" = Sonia(curve_handle),
    stop("Unsupported OIS index for ", currency, " / ", instrument)
  )
}

qlr_ir_make_ois_curve <- function(
    curve_data,
    trade_date,
    currency,
    instrument,
    verbose = TRUE
) {
  .qlr_ir_validate_curve_data(curve_data)

  .qlr_ir_msg(
    verbose,
    "[qlr_ir_make_ois_curve] start: ",
    toupper(trimws(currency)), "::", toupper(trimws(instrument)),
    " @ ", as.character(as.Date(trade_date))
  )

  trade_date_ql <- qlr_date(trade_date)
  qlr_set_eval_date(trade_date_ql)

  conv <- .qlr_ir_get_ois_convention(currency, instrument)

  settle_date <- qlr_advance_days(
    conv$calendar,
    trade_date_ql,
    conv$settlement_days
  )

  curve_handle <- RelinkableYieldTermStructureHandle()
  index_obj <- .qlr_ir_make_ois_index(currency, instrument, curve_handle)

  curve_data2 <- .qlr_ir_normalize_curve_data(curve_data)

  if (any(is.na(curve_data2$tenor) | curve_data2$tenor == "")) {
    stop("curve_data contains missing or empty tenor values")
  }

  if (any(is.na(curve_data2$rate))) {
    stop("curve_data contains non-numeric or missing rate values")
  }

  helper_list <- purrr::pmap(
    curve_data2,
    function(kind, tenor, rate) {
      if (kind == "ois") {
        .qlr_ir_make_ois_helper(
          rate = rate,
          tenor = tenor,
          settlement_days = conv$settlement_days,
          index = index_obj
        )
      } else if (kind == "depo") {
        .qlr_ir_make_deposit_helper(
          rate = rate,
          tenor = tenor,
          calendar = conv$calendar,
          fixing_days = conv$fixing_days,
          day_counter = conv$day_counter
        )
      } else {
        stop("Unsupported helper type for OIS curve: ", kind)
      }
    }
  )

  helper_vec <- RateHelperVector()
  purrr::walk(helper_list, ~ RateHelperVector_append(helper_vec, .x))

  curve_obj <- PiecewiseLogLinearDiscount(
    settle_date,
    helper_vec,
    conv$day_counter
  )

  TermStructure_enableExtrapolation(curve_obj)
  RelinkableYieldTermStructureHandle_linkTo(curve_handle, curve_obj)

  out <- list(
    trade_date = as.character(as.Date(trade_date)),
    currency = toupper(trimws(currency)),
    instrument = toupper(trimws(instrument)),
    settle_date = qlr_iso(settle_date),
    curve = curve_obj,
    curve_handle = curve_handle,
    index = index_obj
  )

  .qlr_ir_msg(
    verbose,
    "[qlr_ir_make_ois_curve] done: ",
    out$currency, "::", out$instrument,
    ", settle_date = ", out$settle_date
  )

  out
}

# ------------------------------------------------------------
# 3. IBOR conventions / builders
# ------------------------------------------------------------

.qlr_ir_get_ibor_convention <- function(currency, instrument) {
  currency <- toupper(trimws(currency))
  instrument <- toupper(trimws(instrument))

  key <- paste(currency, instrument, sep = "::")

  switch(
    key,
    "JPY::TIBOR6M" = list(
      currency = "JPY",
      instrument = "TIBOR6M",
      calendar = Japan(),
      day_counter = Actual365Fixed(),
      fixing_days = 2,
      settlement_days = 2,
      fixed_freq = Frequency_Semiannual_get(),
      fixed_bdc = "ModifiedFollowing"
    ),
    "EUR::EURIBOR6M" = list(
      currency = "EUR",
      instrument = "EURIBOR6M",
      calendar = TARGET(),
      day_counter = Actual360(),
      fixing_days = 2,
      settlement_days = 2,
      fixed_freq = Frequency_Annual_get(),
      fixed_bdc = "ModifiedFollowing"
    ),
    stop("Unsupported IBOR convention for ", currency, " / ", instrument)
  )
}

.qlr_ir_make_ibor_index <- function(currency, instrument, curve_handle) {
  currency <- toupper(trimws(currency))
  instrument <- toupper(trimws(instrument))

  key <- paste(currency, instrument, sep = "::")

  switch(
    key,
    "JPY::TIBOR6M" = Tibor(qlr_period_months(6), curve_handle),
    "EUR::EURIBOR6M" = Euribor6M(curve_handle),
    stop("Unsupported IBOR index for ", currency, " / ", instrument)
  )
}

qlr_ir_make_ibor_curve <- function(
    curve_data,
    trade_date,
    currency,
    instrument,
    verbose = TRUE
) {
  .qlr_ir_validate_curve_data(curve_data)

  .qlr_ir_msg(
    verbose,
    "[qlr_ir_make_ibor_curve] start: ",
    toupper(trimws(currency)), "::", toupper(trimws(instrument)),
    " @ ", as.character(as.Date(trade_date))
  )

  trade_date_ql <- qlr_date(trade_date)
  qlr_set_eval_date(trade_date_ql)

  conv <- .qlr_ir_get_ibor_convention(currency, instrument)

  settle_date <- qlr_advance_days(
    conv$calendar,
    trade_date_ql,
    conv$settlement_days
  )

  curve_handle <- RelinkableYieldTermStructureHandle()
  index_obj <- .qlr_ir_make_ibor_index(currency, instrument, curve_handle)

  curve_data2 <- .qlr_ir_normalize_curve_data(curve_data)

  if (any(is.na(curve_data2$tenor) | curve_data2$tenor == "")) {
    stop("curve_data contains missing or empty tenor values")
  }

  if (any(is.na(curve_data2$rate))) {
    stop("curve_data contains non-numeric or missing rate values")
  }

  helper_list <- purrr::pmap(
    curve_data2,
    function(kind, tenor, rate) {
      if (kind == "depo") {
        .qlr_ir_make_deposit_helper(
          rate = rate,
          tenor = tenor,
          calendar = conv$calendar,
          fixing_days = conv$fixing_days,
          day_counter = conv$day_counter
        )
      } else if (kind == "swap") {
        .qlr_ir_make_swap_helper(
          rate = rate,
          tenor = tenor,
          calendar = conv$calendar,
          fixed_freq = conv$fixed_freq,
          fixed_bdc = conv$fixed_bdc,
          day_counter = conv$day_counter,
          index = index_obj
        )
      } else {
        stop("Unsupported helper type for IBOR curve: ", kind)
      }
    }
  )

  helper_vec <- RateHelperVector()
  purrr::walk(helper_list, ~ RateHelperVector_append(helper_vec, .x))

  curve_obj <- PiecewiseLogLinearDiscount(
    settle_date,
    helper_vec,
    conv$day_counter
  )

  TermStructure_enableExtrapolation(curve_obj)
  RelinkableYieldTermStructureHandle_linkTo(curve_handle, curve_obj)

  out <- list(
    trade_date = as.character(as.Date(trade_date)),
    currency = toupper(trimws(currency)),
    instrument = toupper(trimws(instrument)),
    settle_date = qlr_iso(settle_date),
    curve = curve_obj,
    curve_handle = curve_handle,
    index = index_obj
  )

  .qlr_ir_msg(
    verbose,
    "[qlr_ir_make_ibor_curve] done: ",
    out$currency, "::", out$instrument,
    ", settle_date = ", out$settle_date
  )

  out
}


# ============================================================
# QuantiveRiemann - curve_tables.R
# Curve table helpers
# ============================================================

qlr_curve_tables <- function(
    curve_env,
    tenors = c("1M", "3M", "6M", "1Y", "2Y", "3Y", "5Y", "7Y", "10Y", "15Y", "20Y", "30Y", "40Y")
) {
  ref_date <- curve_env$curve$referenceDate()

  zero_tbl <- tibble::tibble(
    tenor = tenors,
    date = purrr::map_chr(tenors, ~ qlr_iso(ref_date + qlr_period(.x))),
    zero_rate = purrr::map_dbl(
      tenors,
      ~ qlr_zero_rate_date(curve_env$curve, ref_date + qlr_period(.x))
    )
  )

  df_tbl <- tibble::tibble(
    tenor = tenors,
    date = purrr::map_chr(tenors, ~ qlr_iso(ref_date + qlr_period(.x))),
    discount_factor = purrr::map_dbl(
      tenors,
      ~ qlr_discount_date(curve_env$curve, ref_date + qlr_period(.x))
    )
  )

  forward_tbl <- tibble::tibble(
    tenor = tenors,
    start_date = qlr_iso(ref_date),
    end_date = purrr::map_chr(tenors, ~ qlr_iso(ref_date + qlr_period(.x))),
    forward_rate = purrr::map_dbl(
      tenors,
      ~ qlr_forward_rate_date(curve_env$curve, ref_date, ref_date + qlr_period(.x))
    )
  )

  list(
    zero_tbl = zero_tbl,
    df_tbl = df_tbl,
    forward_tbl = forward_tbl
  )
}

.qlr_guess_helper_kind <- function(currency, instrument) {
  currency <- toupper(trimws(currency))
  instrument <- toupper(trimws(instrument))

  key <- paste(currency, instrument, sep = "::")

  if (key %in% c("JPY::TONA", "USD::SOFR", "EUR::ESTR", "GBP::SONIA")) {
    return("ois")
  }

  if (key %in% c("EUR::EURIBOR6M")) {
    return("swap")
  }

  stop("Cannot guess helper kind for ", key)
}

# ------------------------------------------------------------
# public API
# ------------------------------------------------------------

qlr_normalize_market_quotes <- function(
    raw_tbl,
    as_of_date,
    currency,
    instrument,
    kind = NULL,
    quote_col = "MID",
    rate_scale = 1e-4,
    verbose = TRUE
) {
  if (!is.data.frame(raw_tbl)) {
    stop("raw_tbl must be a data.frame or tibble")
  }

  raw2 <- dplyr::rename_with(raw_tbl, ~ toupper(.x))
  quote_col2 <- toupper(trimws(quote_col))

  if (!("TENOR" %in% names(raw2))) {
    stop("raw_tbl must contain a TENOR column")
  }

  if (!(quote_col2 %in% names(raw2))) {
    stop("raw_tbl must contain quote column: ", quote_col2)
  }

  if (is.null(kind)) {
    kind <- .qlr_guess_helper_kind(currency, instrument)
  }

  out <- raw2 %>%
    dplyr::transmute(
      as_of_date = as.Date(.env$as_of_date),
      currency = toupper(trimws(.env$currency)),
      instrument = toupper(trimws(.env$instrument)),
      kind = tolower(trimws(.env$kind)),
      tenor = toupper(trimws(as.character(.data$TENOR))),
      rate = as.numeric(.data[[quote_col2]]) * rate_scale
    )

  if (isTRUE(verbose)) {
    message(
      "[qlr_normalize_market_quotes] ",
      toupper(currency), "::", toupper(instrument),
      " rows = ", nrow(out),
      ", quote_col = ", quote_col2,
      ", kind = ", tolower(trimws(kind))
    )
  }

  out
}
# ============================================================
# QuantiveRiemann - ir_curve_builders.R
# OIS curve batch builders
# ============================================================

qlr_ir_build_ois_curve_envs <- function(
    quotes,
    trade_date = NULL,
    verbose = TRUE
) {
  required_cols <- c("as_of_date", "currency", "instrument", "kind", "tenor", "rate")
  missing_cols <- setdiff(required_cols, names(quotes))

  if (length(missing_cols) > 0) {
    stop("quotes is missing columns: ", paste(missing_cols, collapse = ", "))
  }

  quotes2 <- quotes |>
    dplyr::transmute(
      as_of_date = as.Date(as_of_date),
      currency = toupper(trimws(currency)),
      instrument = toupper(trimws(instrument)),
      kind = tolower(trimws(kind)),
      tenor = toupper(trimws(tenor)),
      rate = as.numeric(rate)
    )

  if (is.null(trade_date)) {
    trade_date <- unique(quotes2$as_of_date)

    if (length(trade_date) != 1L) {
      stop("trade_date is NULL but quotes contain multiple as_of_date values")
    }

    trade_date <- as.character(trade_date)
  }

  quotes3 <- quotes2 |>
    dplyr::filter(as_of_date == as.Date(trade_date))

  keys <- quotes3 |>
    dplyr::distinct(currency, instrument)

  .qlr_ir_msg(
    verbose,
    "[qlr_ir_build_ois_curve_envs] curves to build = ",
    nrow(keys),
    " @ ",
    as.character(as.Date(trade_date))
  )

  out <- purrr::pmap(
    keys,
    function(currency, instrument) {
      curve_data <- quotes3 |>
        dplyr::filter(
          currency == .env$currency,
          instrument == .env$instrument
        ) |>
        dplyr::select(kind, tenor, rate)

      qlr_ir_make_ois_curve(
        curve_data = curve_data,
        trade_date = trade_date,
        currency = currency,
        instrument = instrument,
        verbose = verbose
      )
    }
  )

  names(out) <- paste(keys$currency, keys$instrument, sep = "::")
  out
}

qlr_ir_build_ois_curve_tables <- function(
    curve_envs,
    tenors = c("1M", "3M", "6M", "1Y", "2Y", "3Y", "5Y", "7Y", "10Y", "15Y", "20Y", "30Y", "40Y")
) {
  purrr::map(curve_envs, ~ qlr_curve_tables(.x, tenors = tenors))
}
