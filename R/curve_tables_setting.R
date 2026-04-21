# ============================================================
# QuantiveRiemann - curve_tables_settig.R
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