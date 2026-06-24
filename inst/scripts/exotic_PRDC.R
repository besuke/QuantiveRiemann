suppressPackageStartupMessages({
  library(QuantLib)
  library(tibble)
  library(dplyr)
  library(purrr)
})

# ------------------------------------------------------------
# 0. small helpers
# ------------------------------------------------------------

qlr_date <- function(x) {
  if (inherits(x, "Date")) {
    x <- format(x, "%Y-%m-%d")
  }
  DateParser_parseISO(as.character(x))
}

qlr_iso <- function(x) {
  tryCatch(Date_ISO(x), error = function(e) as.character(x))
}

schedule_date_at <- function(schedule_obj, i_one_based) {
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
    ~ schedule_date_at(schedule_obj, .x)
  )
}

# ------------------------------------------------------------
# 1. PathGenerator
# ------------------------------------------------------------

make_path_generator <- function(
    valuation_date,
    coupon_schedule,
    day_counter,
    process
) {
  all_coupon_dates <- schedule_date_vec(coupon_schedule)

  remaining_coupon_dates <- keep(
    all_coupon_dates,
    ~ .x > valuation_date
  )

  time_grid <- c(
    0.0,
    map_dbl(
      remaining_coupon_dates,
      ~ day_counter$yearFraction(valuation_date, .x)
    )
  )

  grid_steps <- diff(time_grid)
  n_steps <- length(grid_steps)

  list(
    valuation_date = valuation_date,
    coupon_schedule = coupon_schedule,
    day_counter = day_counter,
    process = process,
    all_coupon_dates = all_coupon_dates,
    remaining_coupon_dates = remaining_coupon_dates,
    time_grid = time_grid,
    grid_steps = grid_steps,
    n_steps = n_steps,
    next_path = function() {
      e <- rnorm(n_steps, mean = 0, sd = 1)
      spot <- process$x0()

      # Python 原文に合わせて dw = e * dt
      dw <- e * grid_steps

      path <- numeric(n_steps)

      for (i in seq_len(n_steps)) {
        dt_i <- grid_steps[i]
        t_i <- time_grid[i]
        spot <- process$evolve(t_i, spot, dt_i, dw[i])
        path[i] <- spot
      }

      path
    }
  )
}

# ------------------------------------------------------------
# 2. PRDC pricer
# ------------------------------------------------------------

make_mc_pricer_prdc <- function(
    valuation_date,
    coupon_schedule,
    day_counter,
    notional,
    discount_curve_handle,
    payoff_function,
    fx_path_generator,
    n_paths,
    intro_coupon_schedule = NULL,
    intro_coupon_rate = NULL
) {
  all_coupon_dates <- schedule_date_vec(coupon_schedule)

  past_coupon_dates <- keep(
    all_coupon_dates,
    ~ .x < valuation_date
  )

  n_past_coupon_dates <- length(past_coupon_dates) - 1L
  if (n_past_coupon_dates < 0L) n_past_coupon_dates <- 0L

  past_coupon_rates <- rep(0.0, n_past_coupon_dates)

  remaining_coupon_dates <- keep(
    all_coupon_dates,
    ~ .x > valuation_date
  )

  time_grid <- map_dbl(
    remaining_coupon_dates,
    ~ day_counter$yearFraction(valuation_date, .x)
  )

  grid_steps <- c(time_grid[1], diff(time_grid))
  n_steps <- length(grid_steps)

  has_intro_coupon <- FALSE
  remaining_intro_coupon_dates <- list()
  n_remaining_intro_coupon_dates <- 0L

  if (!is.null(intro_coupon_schedule)) {
    intro_coupon_dates <- schedule_date_vec(intro_coupon_schedule)

    remaining_intro_coupon_dates <- keep(
      intro_coupon_dates,
      ~ .x > valuation_date
    )

    n_remaining_intro_coupon_dates <- length(remaining_intro_coupon_dates)
    has_intro_coupon <- n_remaining_intro_coupon_dates > 0L
  }

  append_intro_coupon_rates <- function(simulated_coupon_rates) {
    if (!has_intro_coupon) return(simulated_coupon_rates)

    n_override <- min(
      n_remaining_intro_coupon_dates,
      length(simulated_coupon_rates)
    )

    simulated_coupon_rates[seq_len(n_override)] <- intro_coupon_rate
    simulated_coupon_rates
  }

  simulate_coupon_rates <- function() {
    simulated_coupon_rates <- numeric(n_steps)

    for (i in seq_len(n_paths)) {
      path <- fx_path_generator$next_path()

      for (j in seq_len(n_steps)) {
        simulated_coupon_rates[j] <- simulated_coupon_rates[j] + payoff_function(path[j])
      }
    }

    simulated_coupon_rates <- simulated_coupon_rates / n_paths
    simulated_coupon_rates <- append_intro_coupon_rates(simulated_coupon_rates)

    coupon_rates <- c(past_coupon_rates, simulated_coupon_rates)

    list(
      simulated_coupon_rates = simulated_coupon_rates,
      coupon_rates = coupon_rates
    )
  }

  create_cash_flows <- function(coupon_rates) {
    n_coupon_cash_flows <- length(coupon_rates)

    coupon_cash_flows <- vector("list", n_coupon_cash_flows)

    for (i in seq_len(n_coupon_cash_flows)) {
      coupon_cash_flows[[i]] <- FixedRateCoupon(
        all_coupon_dates[[i + 1L]],
        notional,
        coupon_rates[i],
        day_counter,
        all_coupon_dates[[i]],
        all_coupon_dates[[i + 1L]]
      )
    }

    redemption <- Redemption(
      notional,
      all_coupon_dates[[length(all_coupon_dates)]]
    )

    list(
      coupon_cash_flows = coupon_cash_flows,
      redemption = redemption
    )
  }

  npv <- function() {
    coupon_rate_obj <- simulate_coupon_rates()
    coupon_rates <- coupon_rate_obj$coupon_rates

    cf_obj <- create_cash_flows(coupon_rates)
    coupon_cash_flows <- cf_obj$coupon_cash_flows
    redemption <- cf_obj$redemption

    coupon_amounts <- map_dbl(
      coupon_cash_flows,
      ~ .x$amount()
    )

    coupon_dates <- map_chr(
      coupon_cash_flows,
      ~ qlr_iso(.x$date())
    )

    coupon_pvs <- map_dbl(
      coupon_cash_flows,
      ~ {
        pay_date <- .x$date()
        amt <- .x$amount()
        df <- discount_curve_handle$discount(pay_date)
        amt * df
      }
    )

    redemption_npv <- {
      pay_date <- redemption$date()
      amt <- redemption$amount()
      df <- discount_curve_handle$discount(pay_date)
      amt * df
    }

    coupon_leg_npv <- sum(coupon_pvs)

    amounts_with_redemption <- coupon_amounts
    amounts_with_redemption[length(amounts_with_redemption)] <-
      amounts_with_redemption[length(amounts_with_redemption)] + notional

    pvs_with_redemption <- coupon_pvs
    pvs_with_redemption[length(pvs_with_redemption)] <-
      pvs_with_redemption[length(pvs_with_redemption)] + redemption_npv

    cash_flow_table <- tibble(
      payment_date = coupon_dates,
      coupon_rate = coupon_rates,
      amount = amounts_with_redemption,
      pv = pvs_with_redemption
    )

    list(
      npv = coupon_leg_npv + redemption_npv,
      coupon_leg_npv = coupon_leg_npv,
      redemption_leg_npv = redemption_npv,
      cash_flow_table = cash_flow_table
    )
  }

  list(
    valuation_date = valuation_date,
    coupon_schedule = coupon_schedule,
    day_counter = day_counter,
    notional = notional,
    discount_curve_handle = discount_curve_handle,
    payoff_function = payoff_function,
    fx_path_generator = fx_path_generator,
    n_paths = n_paths,
    intro_coupon_schedule = intro_coupon_schedule,
    intro_coupon_rate = intro_coupon_rate,
    npv = npv
  )
}

# ------------------------------------------------------------
# 3. process factory
# ------------------------------------------------------------

process_factory <- function() {
  today <- Settings_instance()$evaluationDate()

  domestic_curve <- FlatForward(
    today,
    QuoteHandle(SimpleQuote(0.01)),
    Actual360()
  )
  domestic_curve_handle <- YieldTermStructureHandle(domestic_curve)

  foreign_curve <- FlatForward(
    today,
    QuoteHandle(SimpleQuote(0.03)),
    Actual360()
  )
  foreign_curve_handle <- YieldTermStructureHandle(foreign_curve)

  fx_vol_curve <- BlackConstantVol(
    today,
    NullCalendar(),
    QuoteHandle(SimpleQuote(0.10)),
    Actual360()
  )
  fx_vol_curve_handle <- BlackVolTermStructureHandle(fx_vol_curve)

  fx_spot <- QuoteHandle(SimpleQuote(133.2681))

  GarmanKohlagenProcess(
    fx_spot,
    foreign_curve_handle,
    domestic_curve_handle,
    fx_vol_curve_handle
  )
}

# ------------------------------------------------------------
# 4. run example
# ------------------------------------------------------------

today <- Date(11, 4, 2023)
Settings_instance()$setEvaluationDate(today)

discount_curve <- FlatForward(
  today,
  QuoteHandle(SimpleQuote(0.005)),
  Actual360()
)
discount_curve_handle <- YieldTermStructureHandle(discount_curve)

process <- process_factory()

effective_date <- Date(3, September, 2015)
termination_date <- Date(3, September, 2041)

coupon_schedule <- MakeSchedule(
  effective_date,
  termination_date,
  Period(6, Months),
  calendar = TARGET(),
  convention = "ModifiedFollowing",
  backwards = TRUE
)

intro_coupon_termination_date <- Date(3, September, 2016)

intro_coupon_schedule <- MakeSchedule(
  effective_date,
  intro_coupon_termination_date,
  Period(6, Months),
  calendar = TARGET(),
  convention = "ModifiedFollowing",
  backwards = TRUE
)

fx_path_generator <- make_path_generator(
  valuation_date = today,
  coupon_schedule = coupon_schedule,
  day_counter = Actual360(),
  process = process
)

notional <- 300000000
intro_coupon_rate <- 0.022
n_paths <- 10000

prdc_payoff_function <- function(fx_rate) {
  min(max(0.122 * (fx_rate / 120.0) - 0.1, 0.0), 0.022)
}

prdc_pricer <- make_mc_pricer_prdc(
  valuation_date = today,
  coupon_schedule = coupon_schedule,
  day_counter = Actual360(),
  notional = notional,
  discount_curve_handle = discount_curve_handle,
  payoff_function = prdc_payoff_function,
  fx_path_generator = fx_path_generator,
  n_paths = n_paths,
  intro_coupon_schedule = intro_coupon_schedule,
  intro_coupon_rate = intro_coupon_rate
)

prdc_result <- prdc_pricer$npv()

npv_ccy <- prdc_result$npv
cat("PV in CCY:", npv_ccy, "\n")

jpy_eur <- 145.3275
npv_eur <- npv_ccy / jpy_eur
cat("PV in EUR:", npv_eur, "\n\n")

print(prdc_result$cash_flow_table, n = 20)

