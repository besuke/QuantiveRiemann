# ============================================================
# QuantiveRiemann - ir_reporting.R
# Console reporting helpers for IR demos
# ============================================================
# ============================================================
# QuantiveRiemann - ir_reporting.R
# Curve summary table
# ============================================================

qlr_ir_curve_summary_table <- function(
    curve_env,
    curve_tables = NULL,
    input_quotes = NULL,
    tenors = c("1M", "3M", "6M", "1Y", "2Y", "3Y", "5Y", "7Y", "10Y", "15Y", "20Y", "30Y", "40Y")
) {
  if (is.null(curve_tables)) {
    curve_tables <- qlr_curve_tables(curve_env, tenors = tenors)
  }

  out <- curve_tables$zero_tbl |>
    dplyr::rename(zero_rate = zero_rate) |>
    dplyr::left_join(
      curve_tables$df_tbl |>
        dplyr::rename(discount_factor = discount_factor),
      by = c("tenor", "date")
    ) |>
    dplyr::left_join(
      curve_tables$forward_tbl |>
        dplyr::select(tenor, end_date, forward_rate) |>
        dplyr::rename(date = end_date),
      by = c("tenor", "date")
    )

  if (!is.null(input_quotes)) {
    quotes2 <- input_quotes |>
      dplyr::transmute(
        tenor = toupper(trimws(tenor)),
        input_kind = tolower(trimws(kind)),
        input_rate = as.numeric(rate)
      ) |>
      dplyr::group_by(tenor) |>
      dplyr::summarise(
        input_kind = dplyr::first(input_kind),
        input_rate = dplyr::first(input_rate),
        .groups = "drop"
      )

    out <- out |>
      dplyr::left_join(quotes2, by = "tenor") |>
      dplyr::select(
        tenor,
        date,
        input_kind,
        input_rate,
        zero_rate,
        discount_factor,
        forward_rate
      )
  } else {
    out <- out |>
      dplyr::select(
        tenor,
        date,
        zero_rate,
        discount_factor,
        forward_rate
      )
  }

  out
}

qlr_ir_show_curve_summary_table <- function(
    curve_env,
    curve_tables = NULL,
    input_quotes = NULL,
    tenors = c("1M", "3M", "6M", "1Y", "2Y", "3Y", "5Y", "7Y", "10Y", "15Y", "20Y", "30Y", "40Y"),
    title = "Curve summary table"
) {
  tbl <- qlr_ir_curve_summary_table(
    curve_env = curve_env,
    curve_tables = curve_tables,
    input_quotes = input_quotes,
    tenors = tenors
  )

  message("========================================")
  message(title)
  message("========================================")
  print(tbl)

  invisible(tbl)
}

qlr_ir_show_curve_tables <- function(
    curve_tables,
    title = "Curve tables",
    n = NULL
) {
  message("========================================")
  message(title)
  message("========================================")

  message("--- Zero / Spot table ---")
  zero_tbl <- curve_tables$zero_tbl
  if (!is.null(n)) {
    zero_tbl <- dplyr::slice_head(zero_tbl, n = n)
  }
  print(zero_tbl)

  message("--- Discount factor table ---")
  df_tbl <- curve_tables$df_tbl
  if (!is.null(n)) {
    df_tbl <- dplyr::slice_head(df_tbl, n = n)
  }
  print(df_tbl)

  message("--- Forward table ---")
  forward_tbl <- curve_tables$forward_tbl
  if (!is.null(n)) {
    forward_tbl <- dplyr::slice_head(forward_tbl, n = n)
  }
  print(forward_tbl)

  invisible(curve_tables)
}

qlr_ir_show_swap_legs <- function(
    trade_env,
    curve_obj,
    title = "Swap cashflow tables",
    n = NULL
) {
  message("========================================")
  message(title)
  message("========================================")

  fixed_tbl <- qlr_swap_fixed_leg_table(trade_env$swap, curve_obj)
  float_tbl <- qlr_swap_float_leg_table(trade_env$swap, curve_obj)

  message("--- Fixed leg ---")
  if (!is.null(n)) {
    print(dplyr::slice_head(fixed_tbl, n = n))
  } else {
    print(fixed_tbl)
  }

  message("--- Floating leg ---")
  if (!is.null(n)) {
    print(dplyr::slice_head(float_tbl, n = n))
  } else {
    print(float_tbl)
  }

  invisible(
    list(
      fixed_tbl = fixed_tbl,
      float_tbl = float_tbl
    )
  )
}

qlr_ir_show_swap_summary <- function(trade_env, title = "Swap summary") {
  message("========================================")
  message(title)
  message("========================================")
  message("NPV: ", qlr_swap_npv(trade_env$swap))
  message("Fair rate: ", qlr_swap_fair_rate(trade_env$swap))

  invisible(
    list(
      npv = qlr_swap_npv(trade_env$swap),
      fair_rate = qlr_swap_fair_rate(trade_env$swap)
    )
  )
}

qlr_ir_show_fixing_before_after <- function(
    before_tbl,
    after_tbl,
    title = "Fixing before / after",
    n = NULL
) {
  message("========================================")
  message(title)
  message("========================================")

  cmp_tbl <- before_tbl |>
    dplyr::mutate(row_id = dplyr::row_number()) |>
    dplyr::select(
      row_id,
      pay_date,
      accrual_start,
      accrual_end,
      rate_before = rate,
      amount_before = amount,
      pv_before = pv
    ) |>
    dplyr::left_join(
      after_tbl |>
        dplyr::mutate(row_id = dplyr::row_number()) |>
        dplyr::select(
          row_id,
          rate_after = rate,
          amount_after = amount,
          pv_after = pv
        ),
      by = "row_id"
    )

  if (!is.null(n)) {
    cmp_tbl <- dplyr::slice_head(cmp_tbl, n = n)
  }

  print(cmp_tbl)
  invisible(cmp_tbl)
}
