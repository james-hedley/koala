# =============================================================================
# KOALA - Waitlist Simulator
# Author: James Hedley
# =============================================================================

library(shiny)
library(bslib)
library(tidyverse)
library(scales)
library(shinycssloaders)
library(koala)
library(dtplyr)
library(later)

donors <- readRDS("data/donors.Rds")
waitlist <- readRDS("data/waitlist.Rds")

# ---- User-adjustable settings ------------------------------------------------
plot_probability_threshold <- 0.95
plot_grid_points <- 2000
maximum_search_years <- 30
default_n_donors <- 10000
simulation_chunk_size <- 500

# ---- Startup instructions popup ---------------------------------------------
show_instructions_popup <- TRUE

# ---- Authorship details -------------------------------------------------------
# Update the institution and contact details here.
author_name <- "James Hedley"
author_institution <- "Centre for Organ Donation Evidence (CODE), University of Sydney"
author_contact <- "james.hedley@sydney.edu.au"

# ---- Plot and results colours ------------------------------------------------
# Change colours here to update both the plot and interpretation text.
quantile_colours <- c(
  "25th percentile" = "#228B22",  # forestgreen
  "Median" = "#EE9A00",          # orange2
  "75th percentile" = "#B22222"  # firebrick
)
offer_curve_colour <- "#4169E1"   # royalblue
threshold_colour <- "#551A8B"     # purple4

# ---- Simulation --------------------------------------------------------------
prepare_simulation <- function(new_patient, waitlist, donors, state_debts,
                               n_donors, donors_per_year,
                               offer_acceptance_prob, seed = 1,
                               chunk_size = simulation_chunk_size) {
  set.seed(seed)
  
  list(
    new_waitlist = bind_rows(waitlist, new_patient),
    donors = donors,
    state_debts = state_debts,
    n_donors = as.integer(n_donors),
    donors_per_year = donors_per_year,
    donors_per_day = donors_per_year / 365.25,
    offer_acceptance_prob = offer_acceptance_prob,
    chunk_size = max(1L, min(as.integer(chunk_size), as.integer(n_donors))),
    next_donor = 1L,
    acceptable_donor_chunks = list()
  )
}

process_simulation_batch <- function(simulation_state) {
  first_donor <- simulation_state$next_donor
  last_donor <- min(
    first_donor + simulation_state$chunk_size - 1L,
    simulation_state$n_donors
  )
  donor_numbers <- seq.int(first_donor, last_donor)
  donors_in_chunk <- length(donor_numbers)
  
  donor_chunk <- tibble(
    donor_id = sample(
      simulation_state$donors$donor_id,
      donors_in_chunk,
      replace = TRUE
    )
  ) |>
    left_join(simulation_state$donors, by = "donor_id") |>
    mutate(
      donor_id = paste0("d", donor_numbers),
      day = rpois(donors_in_chunk, simulation_state$donors_per_day)
    )
  
  matches <- cross_join(donor_chunk, simulation_state$new_waitlist) |>
    lazy_dt() |>
    mutate(
      hla_mismatch = rnorm(
        n(),
        mean = patient_hla_mismatch_mean,
        sd = patient_hla_mismatch_sd
      ),
      rescaled_pra = scales::rescale(
        patient_pra,
        from = c(0, 100),
        to = c(0.1, 99.9)
      ),
      unacceptable_antigens = if_else(
        runif(n()) <= rescaled_pra / 100,
        1,
        0
      )
    ) |>
    as_tibble()
  
  allocation <- run_koala(
    matches = matches,
    state_debts = simulation_state$state_debts
  ) |>
    mutate(
      offered = if_else(
        (rank - 1) * simulation_state$offer_acceptance_prob <= donor_kidneys,
        1,
        0
      )
    )
  
  donor_summary <- allocation |>
    group_by(donor_id) |>
    filter(patient_id == "new" | offered == TRUE) |>
    filter(
      patient_id == "new" |
        rank == max(rank[patient_id != "new"], na.rm = TRUE)
    ) |>
    filter(
      max(bloodgroup_priority[patient_id == "new"]) ==
        max(bloodgroup_priority) &
        max(unacceptable_antigens[patient_id == "new"]) == 0 &
        max(bloodgroup_compatible[patient_id == "new"] == 1)
    ) |>
    mutate(
      waityears_gap = case_when(
        kidney_offer == 1 ~ 0,
        shipping_priority == 0 & shipping_priority[1] == 1 ~
          pmax(shipping_threshold, points[1]) - points,
        TRUE ~ points[1] - points
      )
    ) |>
    ungroup() |>
    filter(patient_id == "new") |>
    mutate(waityears_gap = pmax(0, waityears_gap)) |>
    filter(is.finite(waityears_gap)) |>
    select(donor_id, rank, waityears_gap, donor_kidneys)
  
  simulation_state$acceptable_donor_chunks[[length(
    simulation_state$acceptable_donor_chunks
  ) + 1L]] <- donor_summary
  simulation_state$next_donor <- last_donor + 1L
  
  rm(donor_chunk, matches, allocation, donor_summary)
  gc(FALSE)
  
  simulation_state
}

finalise_simulation <- function(simulation_state) {
  acceptable_donors <- bind_rows(
    simulation_state$acceptable_donor_chunks
  )
  
  if (nrow(acceptable_donors) == 0) {
    return(tibble(years = 0, cum_offer_prob = 0))
  }
  
  gaps <- acceptable_donors$waityears_gap
  
  probability_at <- function(t) {
    lambda <- (simulation_state$donors_per_year /
                 simulation_state$n_donors) *
      sum(pmax(0, t - gaps), na.rm = TRUE)
    1 - exp(-lambda)
  }
  
  time_grid <- seq(
    0,
    maximum_search_years,
    length.out = max(1000, plot_grid_points)
  )
  
  tibble(
    years = time_grid,
    cum_offer_prob = map_dbl(time_grid, probability_at)
  )
}

get_cum_offer_prob <- function(allocation_data, n_donors = 10000,
                               donors_per_year = 479,
                               offer_acceptance_prob = 0.8,
                               grid_points = 2000,
                               maximum_search_years = 100) {
  acceptable_donors <- allocation_data |>
    mutate(
      offered = if_else(
        (rank - 1) * offer_acceptance_prob <= donor_kidneys,
        1,
        0
      )
    ) |>
    group_by(donor_id) |>
    filter(patient_id == "new" | offered == TRUE) |>
    filter(
      patient_id == "new" |
        rank == max(rank[patient_id != "new"], na.rm = TRUE)
    ) |>
    filter(
      max(bloodgroup_priority[patient_id == "new"]) == max(bloodgroup_priority) &
        max(unacceptable_antigens[patient_id == "new"]) == 0 &
        max(bloodgroup_compatible[patient_id == "new"] == 1)
    ) |>
    mutate(
      waityears_gap = case_when(
        kidney_offer == 1 ~ 0,
        shipping_priority == 0 & shipping_priority[1] == 1 ~
          pmax(shipping_threshold, points[1]) - points,
        TRUE ~ points[1] - points
      )
    ) |>
    ungroup() |>
    filter(patient_id == "new") |>
    mutate(waityears_gap = pmax(0, waityears_gap)) |>
    filter(is.finite(waityears_gap)) |>
    select(donor_id, rank, waityears_gap, donor_kidneys)
  
  if (nrow(acceptable_donors) == 0) {
    return(tibble(years = 0, cum_offer_prob = 0))
  }
  
  gaps <- acceptable_donors$waityears_gap
  
  probability_at <- function(t) {
    lambda <- (donors_per_year / n_donors) *
      sum(pmax(0, t - gaps), na.rm = TRUE)
    1 - exp(-lambda)
  }
  
  # This full curve is calculated once per simulation. Changing the displayed
  # threshold only filters and interpolates this saved curve, so no rerun occurs.
  time_grid <- seq(
    0,
    maximum_search_years,
    length.out = max(1000, grid_points)
  )
  
  tibble(
    years = time_grid,
    cum_offer_prob = map_dbl(time_grid, probability_at)
  )
}

truncate_probability_curve <- function(data, probability_threshold) {
  clean_data <- data |>
    filter(is.finite(years), is.finite(cum_offer_prob)) |>
    arrange(years)
  
  if (nrow(clean_data) < 2) return(clean_data)
  
  maximum_probability <- max(clean_data$cum_offer_prob, na.rm = TRUE)
  
  if (maximum_probability < probability_threshold) {
    return(clean_data)
  }
  
  threshold_years <- approx(
    x = clean_data$cum_offer_prob,
    y = clean_data$years,
    xout = probability_threshold,
    ties = "ordered"
  )$y
  
  bind_rows(
    clean_data |> filter(years < threshold_years),
    tibble(
      years = threshold_years,
      cum_offer_prob = probability_threshold
    )
  ) |>
    arrange(years)
}

# ---- Plot helpers ------------------------------------------------------------
estimate_quantile_years <- function(data,
                                    probabilities = c(0.25, 0.50, 0.75)) {
  interpolation_data <- data |>
    filter(is.finite(years), is.finite(cum_offer_prob)) |>
    arrange(cum_offer_prob, years) |>
    distinct(cum_offer_prob, .keep_all = TRUE)
  
  if (nrow(interpolation_data) < 2) {
    return(rep(NA_real_, length(probabilities)))
  }
  
  maximum_probability <- max(interpolation_data$cum_offer_prob, na.rm = TRUE)
  
  map_dbl(probabilities, function(probability) {
    if (!is.finite(probability) || probability > maximum_probability) {
      return(NA_real_)
    }
    
    approx(
      x = interpolation_data$cum_offer_prob,
      y = interpolation_data$years,
      xout = probability,
      ties = "ordered"
    )$y
  })
}

format_waiting_time <- function(years) {
  labels <- rep("not reached in the displayed period", length(years))
  valid <- which(!is.na(years) & is.finite(years) & years >= 0)
  
  if (length(valid) > 0) {
    labels[valid] <- koala::period_fmt(years[valid], unit = "years")
  }
  
  labels
}

get_time_axis_settings <- function(maximum_years) {
  if (!is.finite(maximum_years) || maximum_years <= 0) maximum_years <- 1
  
  if (maximum_years < 3 / 12) {
    multiplier <- 365.25 / 7
    axis_title <- "Waiting time after today (weeks)"
    minor_interval <- 1 / 7
  } else if (maximum_years < 2) {
    multiplier <- 12
    axis_title <- "Waiting time after today (months)"
    minor_interval <- 7 / (365.25 / 12)
  } else {
    multiplier <- 1
    axis_title <- "Waiting time after today (years)"
    minor_interval <- 1 / 12
  }
  
  maximum_axis_value <- maximum_years * multiplier
  
  # Select the smallest sensible major interval that produces no more than
  # approximately eight labelled intervals. The axis limit itself remains the
  # exact threshold time rather than being extended to the next major tick.
  choose_major_interval <- function(axis_maximum, allowed_intervals) {
    interval_counts <- ceiling(axis_maximum / allowed_intervals)
    suitable <- which(interval_counts <= 8)
    
    if (length(suitable) > 0) {
      allowed_intervals[min(suitable)]
    } else {
      allowed_intervals[length(allowed_intervals)]
    }
  }
  
  if (grepl("months", axis_title)) {
    # Month labels are restricted to familiar intervals only.
    major_interval <- choose_major_interval(
      maximum_axis_value,
      c(1, 3, 6, 12)
    )
  } else if (grepl("weeks", axis_title)) {
    major_interval <- choose_major_interval(
      maximum_axis_value,
      c(1, 2, 4, 8, 13, 26, 52)
    )
  } else {
    major_interval <- choose_major_interval(
      maximum_axis_value,
      c(1, 2, 5, 10, 20, 25, 50)
    )
  }
  
  # Major and minor breaks stop at or before the true plot endpoint.
  major_breaks <- seq(
    0,
    floor(maximum_axis_value / major_interval) * major_interval,
    by = major_interval
  )
  
  minor_breaks <- seq(
    0,
    floor(maximum_axis_value / minor_interval) * minor_interval,
    by = minor_interval
  )
  
  list(
    multiplier = multiplier,
    axis_title = axis_title,
    maximum_axis_value = maximum_axis_value,
    major_breaks = major_breaks,
    minor_breaks = minor_breaks
  )
}

format_probability_axis_labels <- function(breaks, axis_maximum) {
  vapply(
    breaks,
    function(value) {
      percentage_value <- value * 100
      is_axis_maximum <- isTRUE(
        all.equal(value, axis_maximum, tolerance = 1e-10)
      )
      is_whole_percentage <- abs(
        percentage_value - round(percentage_value)
      ) < 1e-8
      
      if (is_axis_maximum && !is_whole_percentage) {
        formatC(percentage_value, format = "f", digits = 1)
      } else {
        formatC(percentage_value, format = "f", digits = 0)
      }
    },
    character(1)
  )
}

make_plot <- function(data, probability_threshold) {
  data <- truncate_probability_curve(data, probability_threshold)
  probabilities <- c(0.25, 0.50, 0.75)
  quantile_years <- estimate_quantile_years(data, probabilities)
  axis_settings <- get_time_axis_settings(max(data$years, na.rm = TRUE))
  
  plot_data <- data |>
    mutate(display_time = years * axis_settings$multiplier)
  
  quantile_labels <- tibble(
    prob = probabilities,
    years = quantile_years,
    display_time = quantile_years * axis_settings$multiplier,
    label = format_waiting_time(quantile_years),
    quantile_group = factor(
      probabilities,
      levels = probabilities,
      labels = c("25th percentile", "Median", "75th percentile")
    )
  )
  
  reached_quantiles <- quantile_labels |>
    filter(is.finite(years), is.finite(display_time))
  
  threshold_years <- if (
    max(data$cum_offer_prob, na.rm = TRUE) >= probability_threshold
  ) {
    max(data$years, na.rm = TRUE)
  } else {
    NA_real_
  }
  
  threshold_label <- tibble(
    prob = probability_threshold,
    years = threshold_years,
    display_time = threshold_years * axis_settings$multiplier,
    label = format_waiting_time(threshold_years)
  ) |>
    filter(
      probability_threshold >= 0.80,
      is.finite(years),
      is.finite(display_time)
    )
  
  maximum_probability <- max(plot_data$cum_offer_prob, na.rm = TRUE)
  
  # The y-axis ends at the selected plot threshold exactly. Standard labels are
  # shown at 0%, 25%, 50%, and 75% where applicable. The exact threshold is
  # added as a labelled break only when it is at least five percentage points
  # from an existing standard label, preventing labels such as 75% and 77%
  # from overlapping. The axis still ends at the exact threshold either way.
  y_axis_maximum <- maximum_probability
  standard_y_breaks <- c(0, 0.25, 0.50, 0.75)
  visible_standard_y_breaks <- standard_y_breaks[
    standard_y_breaks < y_axis_maximum
  ]
  
  minimum_label_separation <- 0.05
  threshold_label_has_space <-
    length(visible_standard_y_breaks) == 0 ||
    min(abs(y_axis_maximum - visible_standard_y_breaks)) >=
    minimum_label_separation
  
  y_major_breaks <- visible_standard_y_breaks
  
  if (threshold_label_has_space) {
    y_major_breaks <- sort(unique(c(
      y_major_breaks,
      y_axis_maximum
    )))
  }
  
  y_minor_breaks <- sort(unique(c(
    seq(0, floor(y_axis_maximum / 0.05) * 0.05, by = 0.05),
    y_axis_maximum
  )))
  
  ggplot(plot_data) +
    geom_segment(
      data = reached_quantiles,
      aes(x = 0, xend = display_time, y = prob, yend = prob,
          colour = quantile_group),
      linetype = "dashed",
      linewidth = 1.1
    ) +
    geom_segment(
      data = reached_quantiles,
      aes(x = display_time, xend = display_time, y = prob, yend = 0,
          colour = quantile_group),
      linetype = "dashed",
      linewidth = 1.1
    ) +
    geom_text(
      data = reached_quantiles,
      aes(x = display_time, y = prob, label = label,
          colour = quantile_group),
      hjust = 1.05,
      vjust = -0.55,
      size = 7.5,
      fontface = "bold"
    ) +
    geom_segment(
      data = threshold_label,
      aes(x = 0, xend = display_time, y = prob, yend = prob),
      inherit.aes = FALSE,
      linetype = "dashed",
      linewidth = 1.1,
      colour = threshold_colour
    ) +
    geom_segment(
      data = threshold_label,
      aes(x = display_time, xend = display_time, y = prob, yend = 0),
      inherit.aes = FALSE,
      linetype = "dashed",
      linewidth = 1.1,
      colour = threshold_colour
    ) +
    geom_text(
      data = threshold_label,
      aes(x = display_time, y = prob, label = label),
      inherit.aes = FALSE,
      hjust = 1.05,
      vjust = -0.55,
      size = 7.5,
      fontface = "bold",
      colour = threshold_colour
    ) +
    geom_line(
      aes(x = display_time, y = cum_offer_prob),
      linewidth = 2,
      colour = offer_curve_colour
    ) +
    coord_cartesian(
      clip = "off",
      xlim = c(0, axis_settings$maximum_axis_value),
      ylim = c(0, y_axis_maximum)
    ) +
    scale_x_continuous(
      breaks = axis_settings$major_breaks,
      minor_breaks = axis_settings$minor_breaks,
      labels = scales::label_number(accuracy = 1, trim = TRUE),
      expand = c(0, 0),
      guide = guide_axis(minor.ticks = TRUE)
    ) +
    scale_y_continuous(
      labels = function(values) {
        format_probability_axis_labels(
          values,
          axis_maximum = y_axis_maximum
        )
      },
      breaks = y_major_breaks,
      minor_breaks = y_minor_breaks,
      expand = c(0, 0),
      guide = guide_axis(minor.ticks = TRUE)
    ) +
    scale_colour_manual(values = quantile_colours) +
    labs(
      x = axis_settings$axis_title,
      y = "Probability of receiving an offer (%)"
    ) +
    theme_minimal(base_size = 20) +
    theme(
      axis.line = element_line(colour = "black"),
      axis.ticks = element_line(colour = "black"),
      axis.minor.ticks.x.bottom = element_line(colour = "black"),
      axis.minor.ticks.y.left = element_line(colour = "black"),
      axis.ticks.length = unit(4, "pt"),
      axis.minor.ticks.length = rel(0.4),
      axis.text = element_text(size = 18),
      axis.title = element_text(size = 20, face = "bold"),
      panel.grid.major = element_line(colour = "grey92", linewidth = 0.55),
      panel.grid.minor = element_line(colour = "grey96", linewidth = 0.4),
      plot.margin = margin(l = 5, r = 10, t = 30, b = 5),
      legend.position = "none"
    )
}

get_quantile_summary <- function(data) {
  probabilities <- c(0.25, 0.50, 0.75)
  quantile_years <- estimate_quantile_years(data, probabilities)
  
  tibble(
    prob = probabilities,
    years = quantile_years,
    label = format_waiting_time(quantile_years)
  )
}

# ---- Preset patients ---------------------------------------------------------
preset_patients <- tibble(
  preset_id = c(
    "young_easy", "middle_easy", "older_easy",
    "young_medium", "middle_medium", "older_medium",
    "young_difficult", "middle_difficult", "older_difficult"
  ),
  preset_label = c(
    "Younger, easy to match", 
    "Middle-aged, easy to match",
    "Older, easy to match", 
    "Younger, medium difficulty",
    "Middle-aged, medium difficulty", 
    "Older, medium difficulty",
    "Younger, difficult to match",
    "Middle-aged, difficult to match",
    "Older, difficult to match"
  ),
  # Buttons are ordered left to right by age within each difficulty row.
  patient_age = rep(c(25, 55, 70), times = 3),
  
  # Matching characteristics are constant across each difficulty row.
  patient_bloodgroup = rep(c("AB", "A", "O"), each = 3),
  patient_pra = rep(c(0, 80, 95), each = 3),
  patient_epts = c(10, 40, 75, 15, 45, 80, 20, 50, 85),
  patient_hla_mismatch_mean = rep(c(11, 12, 13), each = 3),
  patient_hla_mismatch_sd = 0.5
)

# ---- Theme and styling -------------------------------------------------------
app_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary = "#2C6E9E",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter")
)

app_css <- "
html, body { height: 100%; }
body { overflow-x: hidden; }
.card, .card-body, .patient-results-layout, .patient-card, .results-card {
  min-width: 0;
}
.card-header { padding: 0.55rem 0.85rem; font-size: 1rem; }
.card-body { padding: 0.75rem 0.85rem; }
.patient-results-layout {
  display: grid;
  grid-template-columns: minmax(610px, 5fr) minmax(720px, 7fr);
  gap: 0.8rem;
  align-items: start;
  width: 100%;
}
.patient-card { grid-column: 1; align-self: start; }
.results-card { grid-column: 2; align-self: start; }
.patient-section-title {
  margin: 0 0 0.45rem 0;
  font-size: 1.05rem;
  font-weight: 600;
}

/* Keep the first patient heading close to the first row of field labels. */
.patient-card .card-body > .patient-section-title:first-child {
  margin-bottom: 0 !important;
}
.patient-card .card-body > .patient-section-title:first-child + .patient-input-grid {
  margin-top: -1.25rem;
}
.patient-input-grid {
  display: grid;
  grid-template-columns: repeat(4, minmax(115px, 1fr));
  column-gap: 0.7rem;
  row-gap: 0.2rem;
  align-items: end;
}
.patient-input-grid .shiny-input-container,
.patient-input-grid .form-group {
  width: 100% !important;
  margin-bottom: 0.25rem;
}
.patient-input-grid label {
  min-height: 2.25rem;
  display: flex;
  align-items: flex-end;
  margin-bottom: 0.2rem;
  font-size: 0.84rem;
  line-height: 1.15;
}
.patient-input-grid input[type='number'],
.patient-input-grid .selectize-control,
.patient-input-grid select { width: 100% !important; }
.patient-input-grid .form-control,
.patient-input-grid .selectize-input {
  min-height: 2.15rem;
  height: 2.15rem;
  padding-top: 0.3rem;
  padding-bottom: 0.3rem;
  font-size: 0.9rem;
}

.waiting-time-input-group {
  min-width: 0;
}
.waiting-time-heading {
  display: flex;
  align-items: center;
  min-height: 2.25rem;
  margin-bottom: -0.35rem;
  font-size: 0.84rem;
  line-height: 1.15;
}
.waiting-time-input-pair {
  display: grid;
  grid-template-columns: repeat(2, minmax(0, 1fr));
  gap: 0.4rem;
  align-items: end;
  min-width: 0;
}
.waiting-time-input-pair .shiny-input-container,
.waiting-time-input-pair .form-group,
.waiting-time-input-pair input[type='number'] {
  width: 100% !important;
  min-width: 0;
}
.waiting-time-input-pair label {
  min-height: auto !important;
  margin-top: 0 !important;
  margin-bottom: 0.15rem;
  font-style: italic;
  font-weight: normal;
  text-transform: lowercase;
}

/* Smaller placeholder text for the partially specified dialysis date fields. */
#dialysis_start_year + .selectize-control .selectize-input,
#dialysis_start_month + .selectize-control .selectize-input,
#dialysis_start_year + .selectize-control .selectize-input input,
#dialysis_start_month + .selectize-control .selectize-input input {
  font-size: 0.72rem !important;
}
#dialysis_start_year + .selectize-control .selectize-input > .item,
#dialysis_start_month + .selectize-control .selectize-input > .item {
  font-size: 0.72rem !important;
  line-height: 1.2;
}
.input-label-with-info {
  display: inline-flex;
  align-items: center;
  gap: 0.3rem;
}
.input-info-button {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  width: 1.15rem;
  height: 1.15rem;
  padding: 0;
  border: 0;
  border-radius: 50%;
  background: transparent;
  color: #2C6E9E;
  font-size: 0.85rem;
  line-height: 1;
  cursor: pointer;
}
.input-info-button:hover,
.input-info-button:focus {
  color: #174d73;
  background: rgba(44, 110, 158, 0.10);
  outline: none;
}
.popover { max-width: 360px; }
.popover-body { font-size: 0.82rem; line-height: 1.35; }
.popover-body a { font-weight: 600; }
.example-patient-section {
  border-top: 1px solid #d9d9d9;
  margin-top: 0.1rem;
  padding-top: 0.4rem;
}
.checkbox-grid {
  display: grid;
  grid-template-columns: repeat(3, minmax(150px, 1fr));
  column-gap: 0.7rem;
  row-gap: 0.1rem;
}
.checkbox-grid .form-group,
.checkbox-grid .shiny-input-container {
  margin-bottom: 0.15rem;
  width: 100% !important;
}
.checkbox-grid .form-check {
  display: flex !important;
  align-items: center !important;
  min-height: 2.15rem;
  margin: 0 !important;
  padding: 0 !important;
}
.checkbox-grid .form-check-input,
.checkbox-grid input[type='checkbox'] {
  position: static !important;
  float: none !important;
  flex: 0 0 auto;
  width: 1rem;
  height: 1rem;
  margin: 0 0.5rem 0 0 !important;
  padding: 0 !important;
  transform: translateY(1px);
}
.checkbox-grid .form-check-label,
.checkbox-grid label {
  display: inline-flex !important;
  align-items: center !important;
  min-height: 1rem;
  margin: 0 !important;
  padding: 0 !important;
  font-size: 0.9rem;
  line-height: 1rem !important;
}
.clear-patient-inputs-cell {
  display: flex;
  align-items: center;
  justify-content: flex-end;
  min-height: 2.15rem;
  margin: 0;
  padding: 0;
}
.clear-patient-inputs-cell .btn {
  padding: 0.25rem 0.65rem;
  font-size: 0.78rem;
}
.example-help { margin: 0 0 0.45rem 0; font-size: 0.77rem; }
.preset-grid {
  display: grid;
  grid-template-columns: repeat(3, minmax(140px, 1fr));
  gap: 0.35rem;
}
.preset-grid .btn {
  min-height: 2.35rem;
  padding: 0.25rem 0.35rem;
  white-space: normal;
  line-height: 1.05;
  font-size: 0.72rem;
}
.seed-section {
  border-top: 1px solid #d9d9d9;
  margin-top: 0.5rem;
  padding-top: 0.55rem;
}
.seed-control-row {
  display: flex;
  align-items: flex-end;
  gap: 0.45rem;
}
.seed-input {
  width: 11rem;
  max-width: calc(100% - 3rem);
}
.seed-input .form-group,
.seed-input .shiny-input-container,
.seed-input input[type='number'] {
  width: 11rem !important;
  max-width: 100%;
  margin-bottom: 0 !important;
}
.seed-refresh-button {
  flex: 0 0 auto;
}
.seed-refresh-button .btn {
  width: 2.4rem;
  height: 2.4rem;
  padding: 0;
  display: inline-flex;
  align-items: center;
  justify-content: center;
}
.calculate-section { margin-top: 0.5rem; }
.calculate-section .btn { min-height: 2.4rem; font-weight: 600; }
.results-card .shiny-plot-output { width: 100% !important; }
.plot-control-row {
  display: flex;
  justify-content: flex-end;
  align-items: center;
  margin-bottom: 0.25rem;
}
.plot-threshold-control {
  width: min(42rem, 100%);
  max-width: 100%;
}
.plot-threshold-control .form-group,
.plot-threshold-control .shiny-input-container {
  display: grid;
  grid-template-columns: max-content minmax(16rem, 20rem);
  column-gap: 0.75rem;
  align-items: center;
  width: 100% !important;
  max-width: 100%;
  margin-bottom: 0.1rem;
}
.plot-threshold-control label {
  margin: 0;
  font-size: 0.94rem;
  line-height: 1.2;
  white-space: nowrap;
}
.plot-threshold-control .irs {
  width: 100%;
}
.results-card hr { margin: 0.5rem 0; }
#interpretation_text p {
  margin-bottom: 0.4rem;
  font-size: 1.05rem;
  line-height: 1.35;
}
.settings-input-grid {
  display: grid;
  grid-template-columns: repeat(4, max-content);
  column-gap: 2rem;
  row-gap: 0.4rem;
}
.compact-number { width: 10rem; max-width: 100%; }
.compact-number .shiny-input-container,
.compact-number .form-group,
.compact-number input[type='number'] {
  width: 10rem !important;
  max-width: 100%;
}
.donor-rate-input {
  width: 13rem;
  max-width: 100%;
}
.donor-rate-input .shiny-input-container,
.donor-rate-input .form-group,
.donor-rate-input input[type='number'] {
  width: 13rem !important;
  max-width: 100%;
}
.donor-rate-input label,
.donor-rate-input .input-label-with-info {
  white-space: nowrap;
}
.compact-slider { width: 18rem; max-width: 100%; }
.compact-slider .shiny-input-container,
.compact-slider .form-group {
  width: 18rem !important;
  max-width: 100%;
}
.debt-inputs {
  display: grid;
  grid-template-columns: repeat(5, 8rem);
  column-gap: 0.75rem;
  row-gap: 0.4rem;
  align-items: end;
}
.debt-input { width: 8rem; }
.debt-input .shiny-input-container,
.debt-input .form-group,
.debt-input input[type='number'] { width: 8rem !important; }
.methods-content h4,
.methods-content h5 {
  margin-top: 0.65rem !important;
  margin-bottom: -0.18rem !important;
  line-height: 1.05;
}
.methods-content h4:first-child {
  margin-top: 0 !important;
}
.methods-content h4 + p,
.methods-content h5 + p,
.methods-content h5 + .shiny-html-output,
.methods-content h5 + ul,
.methods-content h5 + .authorship-details {
  margin-top: -0.08rem !important;
}
.methods-content p,
.methods-content .shiny-html-output,
.methods-content .shiny-html-output p {
  margin-top: 0 !important;
  margin-bottom: 0.55rem !important;
}
.methods-content ul {
  margin-top: 0 !important;
  margin-bottom: 0.55rem !important;
}
.methods-content .methods-alert {
  margin-top: 0.15rem !important;
  margin-bottom: 0 !important;
}
.authorship-details {
  margin-top: 0 !important;
  margin-bottom: 0.55rem !important;
  line-height: 1.35;
}
.estimated-progress-overlay {
  position: fixed;
  inset: 0;
  z-index: 99999;
  display: none;
  align-items: center;
  justify-content: center;
  background: rgba(20, 35, 50, 0.28);
  backdrop-filter: blur(1px);
}
.estimated-progress-overlay.visible { display: flex; }
.estimated-progress-dialog {
  width: min(430px, calc(100vw - 2rem));
  padding: 1.2rem 1.3rem;
  background: white;
  border-radius: 0.45rem;
  box-shadow: 0 0.7rem 2.2rem rgba(0, 0, 0, 0.24);
}
.estimated-progress-title {
  margin-bottom: 0.25rem;
  font-size: 1.05rem;
  font-weight: 600;
}
.estimated-progress-description {
  margin-bottom: 0.8rem;
  font-size: 0.85rem;
  color: #6c757d;
}
.estimated-progress-track {
  width: 100%;
  height: 0.85rem;
  overflow: hidden;
  background: #e9ecef;
  border-radius: 999px;
}
.estimated-progress-bar {
  width: 0%;
  height: 100%;
  background: #2C6E9E;
  border-radius: 999px;
}
.estimated-progress-percentage {
  margin-top: 0.4rem;
  text-align: right;
  font-size: 0.78rem;
  color: #6c757d;
}
.estimated-progress-actions {
  display: flex;
  justify-content: flex-end;
  margin-top: 0.65rem;
}
.estimated-progress-actions .btn { min-width: 6rem; }
.scenario-comparison-section {
  border-top: 1px solid #d9d9d9;
  margin-top: 0.6rem;
  padding-top: 0.55rem;
}
.scenario-comparison-title {
  margin-bottom: 0.35rem;
  font-size: 0.9rem;
  font-weight: 600;
}
.scenario-comparison-buttons {
  display: flex;
  flex-wrap: wrap;
  gap: 0.4rem;
}
.scenario-log-list {
  display: grid;
  gap: 0.75rem;
}
.scenario-log-card .card-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 0.75rem;
}
.scenario-log-controls {
  display: grid;
  grid-template-columns: minmax(180px, 1fr) auto auto auto;
  gap: 0.6rem;
  align-items: end;
  margin-bottom: 0.6rem;
}
.scenario-log-controls .form-group,
.scenario-log-controls .shiny-input-container {
  margin-bottom: 0;
}
.scenario-input-table {
  width: 100%;
  margin-bottom: 0;
  font-size: 0.78rem;
}
.scenario-input-table th,
.scenario-input-table td {
  padding: 0.25rem 0.4rem;
  vertical-align: top;
  border-bottom: 1px solid #edf0f2;
}
.scenario-input-table th {
  width: 18%;
  color: #495057;
  font-weight: 600;
  white-space: nowrap;
}
.scenario-empty-message {
  padding: 1rem;
  color: #6c757d;
  text-align: center;
}

@media (max-width: 1450px) {
  .patient-results-layout {
    grid-template-columns: minmax(520px, 5fr) minmax(620px, 7fr);
  }
  .patient-input-grid {
    grid-template-columns: repeat(4, minmax(100px, 1fr));
  }
}
@media (max-width: 1150px) {
  .patient-results-layout {
    grid-template-columns: minmax(420px, 5fr) minmax(500px, 7fr);
  }
  .patient-input-grid { grid-template-columns: repeat(2, 1fr); }
  .checkbox-grid { grid-template-columns: repeat(2, 1fr); }
}
@media (max-width: 850px) {
  .debt-inputs {
    grid-template-columns: repeat(2, 8rem);
  }
  .patient-results-layout { grid-template-columns: 1fr; }
  .patient-card, .results-card { grid-column: 1; }
  .patient-card { grid-row: 1; }
  .results-card { grid-row: 2; }
  .settings-input-grid { grid-template-columns: 1fr; }
}
@media (max-width: 600px) {
  .debt-inputs {
    grid-template-columns: 8rem;
  }
  .plot-threshold-control .form-group,
  .plot-threshold-control .shiny-input-container {
    grid-template-columns: 1fr;
    row-gap: 0.2rem;
  }
  .scenario-log-controls {
    grid-template-columns: 1fr;
  }
  .patient-input-grid, .checkbox-grid, .preset-grid {
    grid-template-columns: 1fr;
  }
}
"

progress_javascript <- "
  function startActualProgress(message) {
    const overlay = document.getElementById('estimated-progress-overlay');
    const bar = document.getElementById('estimated-progress-bar');
    const percentage = document.getElementById('estimated-progress-percentage');
    const description = document.getElementById('estimated-progress-description');
    if (!overlay || !bar || !percentage || !description) return;

    overlay.classList.add('visible');
    bar.style.transition = 'none';
    bar.style.width = '0%';
    percentage.textContent = '0%';
    const cancelButton = document.getElementById('cancel_simulation');
    if (cancelButton) {
      cancelButton.disabled = false;
      cancelButton.textContent = 'Cancel';
    }
    description.textContent = message && message.detail
      ? message.detail
      : 'Preparing donor simulation...';
  }

  function updateActualProgress(message) {
    const bar = document.getElementById('estimated-progress-bar');
    const percentage = document.getElementById('estimated-progress-percentage');
    const description = document.getElementById('estimated-progress-description');
    if (!bar || !percentage || !description) return;

    const progress = Math.max(0, Math.min(100, Number(message.percent) || 0));
    bar.style.transition = 'width 0.15s linear';
    bar.style.width = progress + '%';
    percentage.textContent = progress + '%';
    description.textContent = message.detail || '';
  }

  function hideEstimatedProgress() {
    const overlay = document.getElementById('estimated-progress-overlay');
    const bar = document.getElementById('estimated-progress-bar');
    if (overlay) overlay.classList.remove('visible');
    if (bar) {
      bar.style.transition = 'none';
      bar.style.width = '0%';
    }
  }

  function initialiseInputPopovers() {
    const popoverElements = document.querySelectorAll(
      '[data-bs-toggle=popover]'
    );

    popoverElements.forEach(function(element) {
      if (!bootstrap.Popover.getInstance(element)) {
        new bootstrap.Popover(element, { container: 'body' });
      }
    });
  }

  document.addEventListener('DOMContentLoaded', initialiseInputPopovers);
  document.addEventListener('shiny:connected', initialiseInputPopovers);
  document.addEventListener('shiny:value', initialiseInputPopovers);

  Shiny.addCustomMessageHandler('start-actual-progress', function(message) {
    startActualProgress(message);
  });

  Shiny.addCustomMessageHandler('update-actual-progress', function(message) {
    updateActualProgress(message);
  });

  Shiny.addCustomMessageHandler('hide-estimated-progress', function(message) {
    hideEstimatedProgress();
  });

  Shiny.addCustomMessageHandler('simulation-notification', function(message) {
    Shiny.notifications.show({
      html: message.text || '',
      type: message.type || 'default',
      duration: message.duration || 4000,
      closeButton: true
    });
  });
  "

# ---- Input information popovers ----------------------------------------------

info_label <- function(label_text, explanation_html) {
  tags$span(
    class = "input-label-with-info",
    tags$span(label_text),
    tags$button(
      type = "button",
      class = "input-info-button",
      `data-bs-toggle` = "popover",
      `data-bs-trigger` = "hover focus click",
      `data-bs-placement` = "top",
      `data-bs-html` = "true",
      `data-bs-sanitize` = "false",
      `data-bs-content` = explanation_html,
      `aria-label` = paste("More information about", label_text),
      icon("circle-info")
    )
  )
}

# ---- Dialysis start-date choices ---------------------------------------------

current_calendar_year <- as.integer(format(Sys.Date(), "%Y"))
dialysis_year_choices <- c(
  "Year not specified" = "",
  setNames(
    as.character(seq(current_calendar_year, current_calendar_year - 80)),
    as.character(seq(current_calendar_year, current_calendar_year - 80))
  )
)
dialysis_month_choices <- c(
  "Month not specified" = "",
  setNames(sprintf("%02d", 1:12), month.name)
)

# ---- UI ----------------------------------------------------------------------
ui <- page_navbar(
  id = "main_navbar",
  selected = "Patient & Results",
  title = "KOALA Waitlist Simulator",
  theme = app_theme,
  fillable = FALSE,
  
  header = tagList(
    tags$head(
      tags$style(HTML(app_css)),
      tags$script(HTML(progress_javascript))
    ),
    div(
      id = "estimated-progress-overlay",
      class = "estimated-progress-overlay",
      div(
        class = "estimated-progress-dialog",
        div(class = "estimated-progress-title", "Simulation progress"),
        div(
          id = "estimated-progress-description",
          class = "estimated-progress-description",
          "Preparing donor simulation..."
        ),
        div(
          class = "estimated-progress-track",
          div(id = "estimated-progress-bar", class = "estimated-progress-bar")
        ),
        div(
          id = "estimated-progress-percentage",
          class = "estimated-progress-percentage",
          "0%"
        ),
        div(
          class = "estimated-progress-actions",
          actionButton(
            "cancel_simulation",
            "Cancel",
            class = "btn-outline-secondary btn-sm",
            onclick = paste0(
              "this.disabled = true; ",
              "this.textContent = 'Cancelling...';"
            )
          )
        )
      )
    )
  ),
  
  
  nav_panel(
    title = "Instructions",
    card(
      card_header("Instructions"),
      card_body(
        style = "max-width: 850px;",
        p("This tool simulates kidney allocation to estimate expected waiting time for a specific patient under the KOALA kidney allocation algorithm."),
        tags$ol(
          tags$li(strong("Patient & Results tab: "), "Enter details for a specific patient."),
          tags$li(strong("Waiting time: "), "You can enter the month and/or year dialysis started, or alternatively enter the number of years and months already waited."),
          tags$li(strong("Settings tab: "), "Change the number of simulated donors and other simulation settings."),
          tags$li(strong("State Debts tab: "), "Adjust state kidney debts if required. These affect which patients are eligible to receive kidneys from interstate."),
          tags$li(strong("Run Simulation: "), "Return to the Patient & Results tab and click 'Run simulation' to estimate the patient's probability of receiving a kidney offer over time."),
          tags$li(
            strong("Scenarios log: "),
            paste0(
              "Each completed simulation is saved in the Scenarios log for the current browser session. ",
              "You can give a scenario a name, reload an earlier result without running the simulation again, ",
              "and select scenarios for comparison. Scenarios selected for comparison appear as buttons below ",
              "the plot description on the Patient & Results tab."
            )
          )
        )
      )
    )
  ),
  nav_panel(
    title = "Patient & Results",
    div(
      class = "patient-results-layout",
      card(
        class = "patient-card",
        card_header("Patient details"),
        card_body(
          div(class = "patient-section-title", "Enter patient details"),
          div(
            class = "patient-input-grid",
            selectInput(
              "dialysis_start_year",
              info_label(
                "Dialysis start year",
                paste0(
                  "Select the year dialysis started. If a year is selected ",
                  "without a month, the app assumes 1 July of that year."
                )
              ),
              choices = dialysis_year_choices,
              selected = "",
              width = "100%"
            ),
            selectInput(
              "dialysis_start_month",
              info_label(
                "Dialysis start month",
                paste0(
                  "Optionally select the month dialysis started. If a month is ",
                  "selected, the app assumes the 15th day of that month. Please ",
                  "select a year first."
                )
              ),
              choices = dialysis_month_choices,
              selected = "",
              width = "100%"
            ),
            div(
              class = "waiting-time-input-group",
              div(
                class = "waiting-time-heading",
                info_label(
                  "Waiting time",
                  paste0(
                    "The current waiting time updates automatically from the ",
                    "selected dialysis start year and optional month. You can ",
                    "also enter or edit the whole years and additional months ",
                    "directly. The app combines both values for the simulation."
                  )
                )
              ),
              div(
                class = "waiting-time-input-pair",
                numericInput(
                  "patient_waityears",
                  "years",
                  value = NA, min = 0, step = 1, width = "100%"
                ),
                numericInput(
                  "patient_waitmonths",
                  "months",
                  value = NA, min = 0, max = 11, step = 1, width = "100%"
                )
              )
            ),
            selectInput(
              "patient_state", "Transplant unit state",
              choices = c("Select..." = "", "NSW", "VIC", "QLD", "SA", "WA"),
              width = "100%"
            ),
            numericInput(
              "patient_age", "Age (years)",
              value = NA, min = 0, max = 80, step = 1, width = "100%"
            ),
            selectInput(
              "patient_bloodgroup", "Blood group",
              choices = c("Select..." = "", "O", "A", "B", "AB"),
              width = "100%"
            ),
            numericInput(
              "patient_pra", "PRA (%)",
              value = NA, min = 0, max = 100, step = 0.1, width = "100%"
            ),
            numericInput(
              "patient_epts",
              info_label(
                "EPTS percentile",
                paste0(
                  "The Australian Estimated Post-Transplant Survival (EPTS) ",
                  "percentile compares expected post-transplant survival with ",
                  "other patients on the Australian kidney waiting list. Lower ",
                  "percentiles indicate more favourable expected survival. It is ",
                  "based on age, dialysis duration and prior solid-organ ",
                  "transplant. Formula and validation provided by Irish et. al., 2023: ",
                  "<a href='https://doi.org/10.1111/nep.14158' ",
                  "target='_blank' rel='noopener noreferrer'>Link to the ",
                  "Australian EPTS validation paper (Irish et. al., 2023)</a>."
                )
              ),
              value = NA, min = 1, max = 100, step = 1, width = "100%"
            ),
            numericInput(
              "patient_hla_mismatch_mean",
              info_label(
                "Average HLA mismatch",
                paste0(
                  "The mean HLA mismatch score when compared against the 1,000 donor reference panel.",
                  " A lower value means a patient is easier to match, a higher value means harder to match.",
                  " Typical values range from 11 to 13. Extreme values range from 10 to 15."
                )
              ),
              value = NA, min = 0, max = 17, step = 0.1, width = "100%"
            ),
            numericInput(
              "patient_hla_mismatch_sd",
              info_label(
                "HLA mismatch SD",
                paste0(
                  "The standard deviation of HLA mismatch scores when compared against the 1,000 donor reference panel.",
                  " A lower value means less variablility, and a higher value means more variability.",
                  " For example, if the mean HLA mismatch score is low (easy to match), then low variability means nearly all donors are a good match for this patient.",
                  " High variability would mean that some donors are a very good match, and others are an ok match, but on average they are a good match. ",
                  " If the mean HLA mismatch score is high (hard to match), then a low variability means nearly all donors are a poor match,",
                  " while high variability means some donors are on ok match, and others are a very poor match, but on average the matches are poor. ",
                  " Typical values range from 0.4 to 0.6. Extreme values range from 0.3 to 0.8."
                )
              ),
              value = NA, min = 0.1, step = 0.1, width = "100%"
            )
          ),
          div(
            class = "checkbox-grid",
            checkboxInput("patient_national_urgent", "National urgent listing"),
            checkboxInput("patient_state_urgent", "State priority listing"),
            checkboxInput("patient_prior_donor", "Prior living kidney donor"),
            checkboxInput(
              "patient_kidney_after_other_organ",
              "Kidney after other organ"
            ),
            checkboxInput("patient_spk", "SPK candidate"),
            div(
              class = "clear-patient-inputs-cell",
              actionButton(
                "clear_patient_inputs",
                "Clear all inputs",
                icon = icon("eraser"),
                class = "btn-outline-secondary btn-sm"
              )
            )
          ),
          div(
            class = "example-patient-section",
            div(class = "patient-section-title", "Or use an example patient"),
            p(
              class = "text-muted example-help",
              "Select an example patient profile, then edit any value above if needed."
            ),
            div(
              class = "preset-grid",
              !!!lapply(seq_len(nrow(preset_patients)), function(i) {
                actionButton(
                  paste0("preset_", preset_patients$preset_id[i]),
                  preset_patients$preset_label[i],
                  class = "btn-outline-primary btn-sm w-100"
                )
              })
            )
          ),
          div(
            class = "seed-section",
            div(
              class = "seed-control-row",
              div(
                class = "seed-input",
                numericInput(
                  "simulation_seed",
                  info_label(
                    "Simulation seed",
                    paste0(
                      "The seed controls the random donor sampling and simulated ",
                      "crossmatches. Using the same seed with the same patient ",
                      "details and model settings produces identical results. ",
                      "Choose a different seed, or press the refresh button to ",
                      "generate a random seed, to obtain a different simulation."
                    )
                  ),
                  value = 1,
                  min = 1,
                  max = 2147483647,
                  step = 1,
                  width = "100%"
                )
              ),
              div(
                class = "seed-refresh-button",
                actionButton(
                  "randomise_seed",
                  label = NULL,
                  icon = icon("arrows-rotate"),
                  class = "btn-outline-primary",
                  title = "Generate a random simulation seed",
                  `aria-label` = "Generate a random simulation seed"
                )
              )
            )
          ),
          div(
            class = "calculate-section",
            actionButton(
              "calculate",
              "Run simulation",
              class = "btn-primary w-100",
              icon = icon("play"),
              onclick = NULL
            )
          )
        )
      ),
      card(
        class = "results-card",
        card_header("Estimated waiting time"),
        card_body(
          div(
            class = "plot-control-row",
            div(
              class = "plot-threshold-control",
              sliderInput(
                "plot_threshold_percent",
                "Show waiting time until probability of an offer reaches:",
                value = 95,
                min = 50,
                max = 99.9,
                step = 0.5,
                post = "%",
                width = "100%"
              )
            )
          ),
          shinycssloaders::withSpinner(
            plotOutput("offer_plot", height = "430px"),
            type = 6,
            color = "#2C6E9E"
          ),
          hr(),
          uiOutput("interpretation_text"),
          uiOutput("scenario_comparison_ui")
        )
      )
    )
  ),
  
  nav_panel(
    title = "Settings",
    card(
      card_header("Simulation settings"),
      card_body(
        p(
          class = "text-muted",
          "These settings control the simulation."
        ),
        div(
          class = "settings-input-grid",
          div(
            class = "compact-number",
            numericInput(
              "n_donors",
              info_label(
                "Simulated donors",
                paste0(
                  "The number of future donors sampled for the simulation. ",
                  "Larger values produce more stable estimates, but take longer to run."
                )
              ),
              value = default_n_donors, min = 1000, max = 100000, step = 100, width = "100%"
            )
          ),
          div(
            class = "compact-number",
            numericInput(
              "simulation_chunk_size",
              info_label(
                "Donor batch size",
                paste0(
                  "The number of donors processed together in each simulation ",
                  "batch. Larger chunks are generally faster because there are ",
                  "fewer batches, but they use more memory. Smaller chunks use ",
                  "less memory but may take longer. The maximum is the current ",
                  "number of simulated donors."
                )
              ),
              value = simulation_chunk_size,
              min = 100,
              max = 1000,
              step = 100,
              width = "100%"
            )
          ),
          div(
            class = "compact-slider",
            sliderInput(
              "offer_acceptance_percent",
              info_label(
                "Offer acceptance probability (%)",
                paste0(
                  "The assumed probability that each higher-ranked patient ",
                  "accepts an offer. The same probability is applied to every ",
                  "higher-ranked patient in the simulation regardless of their characteristics or the characteristics of the donor."
                )
              ),
              value = 80, min = 1, max = 100, step = 1,
              post = "%", width = "100%"
            )
          ),
          div(
            class = "compact-number donor-rate-input",
            numericInput(
              "annual_donors",
              info_label(
                "Donor rate (per year)",
                paste0(
                  "The assumed number of deceased donors available each year. ",
                  "This is used to convert 'number of simulated donors' into 'expected waiting time'. ",
                  " The default value of 479 is based on the 1,000 donor reference panel. ",
                  "  More up-to-date estimates may be obtained from ANZOD: ",
                  "<a href='https://anzorrg.org.au/reports?registry=anzod&type=annual+report' ",
                  "target='_blank' rel='noopener noreferrer'>Link to the ",
                  "Australian EPTS validation paper (Irish et. al., 2023)</a>."
                )
              ),
              value = 479, min = 1, step = 1, width = "100%"
            )
          )
        )
      )
    )
  ),
  
  nav_panel(
    title = "State debts",
    card(
      card_header("Current kidney debts for each state"),
      card_body(
        p(
          class = "text-muted",
          paste(
            "Positive values are owed to the national pool and negative values",
            "are owed by the national pool. Values must sum to zero."
          )
        ),
        div(
          class = "debt-inputs",
          div(class = "debt-input", numericInput("debt_nsw", "NSW", 6)),
          div(class = "debt-input", numericInput("debt_vic", "VIC", 5)),
          div(class = "debt-input", numericInput("debt_qld", "QLD", -1)),
          div(class = "debt-input", numericInput("debt_sa", "SA", -4)),
          div(class = "debt-input", numericInput("debt_wa", "WA", -6))
        ),
        uiOutput("debt_sum_check")
      )
    )
  ),
  
  nav_panel(
    title = "Scenarios log",
    card(
      card_header("Previously run scenarios"),
      card_body(
        p(
          class = "text-muted small",
          paste(
            "Each completed simulation is saved for this browser session.",
            "Rename scenarios, select scenarios for comparison, or load a",
            "previous result without re-running the simulation."
          )
        ),
        uiOutput("scenario_log_ui")
      )
    )
  ),
  
  nav_panel(
    title = "Methods",
    card(
      card_body(
        class = "methods-content",
        style = "max-width: 800px;",
        h4("How this tool works"),
        p(
          "This tool uses data from Organ Match about previous donors and the waiting list to simulate new donors, ",
          "and compare how a specific patient would rank against the full waiting list.",
          "It uses the 1,000 donor reference panel from Organ Match ",
          "and the national kidney-only waiting list at 17th August 2026. ",
          "This data has been de-identified and perturbed prior to uploading to preserve anonymity."
        ),
        h5("Simulated donors"),
        uiOutput("methods_simulated_donors_text"),
        h5("Offer acceptance"),
        uiOutput("methods_acceptance_text"),
        h5("Key assumptions and limitations"),
        tags$ul(
          tags$li("Results are based on simulation, and should be interpreted with caution. "),
          tags$li("Assumes the current waitlist remains the same over time. When a new donor is simulated, ",
                  "waitlisted patients are ranked using KOALA, but those patients are not removed from the waitlist ",
                  "and will be ranked again when the next donor becomes available. ",
                  "This is essentially assuming that as people get removed and added to the waitlist, the general ",
                  "make-up of the waitlist remains constant over time."),
          tags$li("Accounts for increasing priority as waiting time increases. but does not account for ",
                  "increasing age and time on dialysis which will affect EPTS and hence change allocation over time"),
          tags$li("Assumes that state debts stay constant over time. Does not adjust state debts as new kidneys are transplanted."),
          tags$li("Allows for kidneys to be allocated to bloodgroup compatible (but non-identical) recipients if no-one with ",
                  "blood group priority (identical or meets inter-bloodgroup thresholds) has accepted the offer."),
        ),
        h5("Authorship"),
        div(
          class = "authorship-details",
          div(author_name),
          div(author_institution),
          div(author_contact)
        ),
        div(
          class = "alert alert-info methods-alert",
          paste(
            "This tool is intended to support discussion between patients and clinicians. It should not be used as the sole basis for clinical decision-making."
          )
        )
      )
    )
  )
)

# ---- Server ------------------------------------------------------------------
server <- function(input, output, session) {
  
  if (isTRUE(show_instructions_popup)) {
    session$onFlushed(function() {
      showModal(
        modalDialog(
          title = "Welcome to the KOALA Waitlist Simulator",
          p("This tool simulates kidney allocation to estimate expected waiting time for a specific patient under the KOALA kidney allocation algorithm."),
          tags$ol(
            tags$li("Enter details for a specific patient in the Patient & Results tab."),
            tags$li("You can enter the month and/or year of dialysis start, or alternatively just enter the number of years waiting so far."),
            tags$li("You can change the number of donors to be simulated and other settings in the Settings tab."),
            tags$li("You can change the state debts in the State debts tab, since these will affect which patients are eligible to be allocated a kidney from interstate."),
            tags$li(
              paste0(
                "Every completed simulation is saved in the Scenarios log for the current browser session. ",
                "From there, you can name a scenario, reload a previous result without re-running it, and select ",
                "scenarios for comparison. Selected scenarios appear as buttons below the plot in the Patient & Results tab."
              )
            )
          ),
          footer = actionButton("dismiss_welcome", "Proceed", class = "btn-primary"),
          easyClose = FALSE
        )
      )
    }, once = TRUE)
    
    observeEvent(input$dismiss_welcome, {
      removeModal()
    }, once = TRUE)
  }
  
  
  scenarios <- reactiveValues(
    records = list(),
    active_id = NULL
  )
  
  capture_scenario_inputs <- function() {
    list(
      dialysis_start_year = input$dialysis_start_year,
      dialysis_start_month = input$dialysis_start_month,
      patient_waityears = input$patient_waityears,
      patient_waitmonths = input$patient_waitmonths,
      patient_state = input$patient_state,
      patient_age = input$patient_age,
      patient_bloodgroup = input$patient_bloodgroup,
      patient_pra = input$patient_pra,
      patient_epts = input$patient_epts,
      patient_hla_mismatch_mean = input$patient_hla_mismatch_mean,
      patient_hla_mismatch_sd = input$patient_hla_mismatch_sd,
      patient_national_urgent = input$patient_national_urgent,
      patient_state_urgent = input$patient_state_urgent,
      patient_prior_donor = input$patient_prior_donor,
      patient_kidney_after_other_organ = input$patient_kidney_after_other_organ,
      patient_spk = input$patient_spk,
      simulation_seed = input$simulation_seed,
      n_donors = input$n_donors,
      simulation_chunk_size = input$simulation_chunk_size,
      offer_acceptance_percent = input$offer_acceptance_percent,
      annual_donors = input$annual_donors,
      debt_nsw = input$debt_nsw,
      debt_vic = input$debt_vic,
      debt_qld = input$debt_qld,
      debt_sa = input$debt_sa,
      debt_wa = input$debt_wa
    )
  }
  
  find_matching_scenario_id <- function(values) {
    records <- isolate(scenarios$records)
    if (length(records) == 0) return(NULL)
    
    matches <- which(vapply(
      records,
      function(record) identical(record$inputs, values),
      logical(1)
    ))
    
    if (length(matches) == 0) NULL else matches[1]
  }
  
  restore_scenario_inputs <- function(values) {
    updateSelectInput(session, "dialysis_start_year", selected = values$dialysis_start_year)
    updateSelectInput(session, "dialysis_start_month", selected = values$dialysis_start_month)
    updateNumericInput(session, "patient_waityears", value = values$patient_waityears)
    updateNumericInput(session, "patient_waitmonths", value = values$patient_waitmonths)
    updateSelectInput(session, "patient_state", selected = values$patient_state)
    updateNumericInput(session, "patient_age", value = values$patient_age)
    updateSelectInput(session, "patient_bloodgroup", selected = values$patient_bloodgroup)
    updateNumericInput(session, "patient_pra", value = values$patient_pra)
    updateNumericInput(session, "patient_epts", value = values$patient_epts)
    updateNumericInput(session, "patient_hla_mismatch_mean", value = values$patient_hla_mismatch_mean)
    updateNumericInput(session, "patient_hla_mismatch_sd", value = values$patient_hla_mismatch_sd)
    updateCheckboxInput(session, "patient_national_urgent", value = values$patient_national_urgent)
    updateCheckboxInput(session, "patient_state_urgent", value = values$patient_state_urgent)
    updateCheckboxInput(session, "patient_prior_donor", value = values$patient_prior_donor)
    updateCheckboxInput(session, "patient_kidney_after_other_organ", value = values$patient_kidney_after_other_organ)
    updateCheckboxInput(session, "patient_spk", value = values$patient_spk)
    updateNumericInput(session, "simulation_seed", value = values$simulation_seed)
    updateNumericInput(session, "n_donors", value = values$n_donors)
    updateNumericInput(
      session,
      "simulation_chunk_size",
      value = values$simulation_chunk_size,
      max = values$n_donors
    )
    updateSliderInput(session, "offer_acceptance_percent", value = values$offer_acceptance_percent)
    updateNumericInput(session, "annual_donors", value = values$annual_donors)
    updateNumericInput(session, "debt_nsw", value = values$debt_nsw)
    updateNumericInput(session, "debt_vic", value = values$debt_vic)
    updateNumericInput(session, "debt_qld", value = values$debt_qld)
    updateNumericInput(session, "debt_sa", value = values$debt_sa)
    updateNumericInput(session, "debt_wa", value = values$debt_wa)
  }
  
  scenario_summary_rows <- function(values) {
    urgent_flags <- c(
      if (isTRUE(values$patient_national_urgent)) "National urgent",
      if (isTRUE(values$patient_state_urgent)) "State priority",
      if (isTRUE(values$patient_prior_donor)) "Prior living donor",
      if (isTRUE(values$patient_kidney_after_other_organ)) "Kidney after other organ",
      if (isTRUE(values$patient_spk)) "SPK"
    )
    if (length(urgent_flags) == 0) urgent_flags <- "None"
    
    list(
      "Dialysis start" = paste(
        ifelse(nzchar(values$dialysis_start_month),
               month.name[as.integer(values$dialysis_start_month)], "Month unspecified"),
        ifelse(nzchar(values$dialysis_start_year), values$dialysis_start_year, "Year unspecified")
      ),
      "Waiting time" = paste(values$patient_waityears, "years", values$patient_waitmonths, "months"),
      "State / blood group" = paste(values$patient_state, "/", values$patient_bloodgroup),
      "Age / PRA / EPTS" = paste(values$patient_age, "/", values$patient_pra, "/", values$patient_epts),
      "HLA mean / SD" = paste(values$patient_hla_mismatch_mean, "/", values$patient_hla_mismatch_sd),
      "Bonuses" = paste(urgent_flags, collapse = ", "),
      "Seed / simulated donors / batch size" = paste(
        values$simulation_seed,
        "/",
        values$n_donors,
        "/",
        values$simulation_chunk_size
      ),
      "Acceptance / annual donors" = paste0(values$offer_acceptance_percent, "% / ", values$annual_donors),
      "State debts" = paste0("NSW ", values$debt_nsw, ", VIC ", values$debt_vic,
                             ", QLD ", values$debt_qld, ", SA ", values$debt_sa,
                             ", WA ", values$debt_wa)
    )
  }
  
  observeEvent(input$calculate, {
    scenarios$active_id <- NULL
  }, priority = 100)
  
  observeEvent(input$n_donors, {
    req(!is.na(input$n_donors), input$n_donors >= 1)
    
    current_chunk_size <- input$simulation_chunk_size
    if (is.null(current_chunk_size) || is.na(current_chunk_size)) {
      current_chunk_size <- min(simulation_chunk_size, input$n_donors)
    }
    
    updateNumericInput(
      session,
      "simulation_chunk_size",
      value = min(current_chunk_size, input$n_donors),
      min = 1,
      max = input$n_donors,
      step = 1
    )
  }, ignoreInit = FALSE)
  
  observeEvent(input$randomise_seed, {
    new_seed <- sample.int(
      .Machine$integer.max,
      size = 1
    )
    
    updateNumericInput(
      session,
      "simulation_seed",
      value = new_seed
    )
  })
  
  observeEvent(
    list(input$dialysis_start_year, input$dialysis_start_month),
    {
      selected_year <- input$dialysis_start_year
      selected_month <- input$dialysis_start_month
      
      if (!is.null(selected_year) && nzchar(selected_year)) {
        month_was_selected <- !is.null(selected_month) && nzchar(selected_month)
        assumed_month <- if (month_was_selected) as.integer(selected_month) else 7L
        assumed_day <- if (month_was_selected) 15L else 1L
        
        assumed_start_date <- as.Date(sprintf(
          "%04d-%02d-%02d",
          as.integer(selected_year),
          assumed_month,
          assumed_day
        ))
        
        elapsed_days <- max(
          0,
          as.numeric(difftime(Sys.Date(), assumed_start_date, units = "days"))
        )
        completed_total_months <- floor(elapsed_days / (365.25 / 12))
        completed_years <- completed_total_months %/% 12
        completed_months <- completed_total_months %% 12
        
        updateNumericInput(
          session,
          "patient_waityears",
          value = completed_years
        )
        updateNumericInput(
          session,
          "patient_waitmonths",
          value = completed_months
        )
      }
    },
    ignoreInit = TRUE
  )
  
  observeEvent(input$dialysis_start_month, {
    if (
      !is.null(input$dialysis_start_month) &&
      nzchar(input$dialysis_start_month) &&
      (is.null(input$dialysis_start_year) ||
       !nzchar(input$dialysis_start_year))
    ) {
      showNotification(
        "Please select the dialysis start year before selecting a month.",
        type = "message",
        duration = 5
      )
      updateSelectInput(session, "dialysis_start_month", selected = "")
    }
  }, ignoreInit = TRUE)
  observeEvent(input$clear_patient_inputs, {
    updateSelectInput(session, "dialysis_start_year", selected = "")
    updateSelectInput(session, "dialysis_start_month", selected = "")
    updateNumericInput(session, "patient_waityears", value = NA)
    updateNumericInput(session, "patient_waitmonths", value = NA)
    updateSelectInput(session, "patient_state", selected = "")
    updateNumericInput(session, "patient_age", value = NA)
    updateSelectInput(session, "patient_bloodgroup", selected = "")
    updateNumericInput(session, "patient_pra", value = NA)
    updateNumericInput(session, "patient_epts", value = NA)
    updateNumericInput(session, "patient_hla_mismatch_mean", value = NA)
    updateNumericInput(session, "patient_hla_mismatch_sd", value = NA)
    
    updateCheckboxInput(session, "patient_national_urgent", value = FALSE)
    updateCheckboxInput(session, "patient_state_urgent", value = FALSE)
    updateCheckboxInput(session, "patient_prior_donor", value = FALSE)
    updateCheckboxInput(
      session,
      "patient_kidney_after_other_organ",
      value = FALSE
    )
    updateCheckboxInput(session, "patient_spk", value = FALSE)
  })
  
  lapply(seq_len(nrow(preset_patients)), function(i) {
    preset <- preset_patients[i, ]
    
    observeEvent(input[[paste0("preset_", preset$preset_id)]], {
      updateNumericInput(session, "patient_age", value = preset$patient_age)
      updateSelectInput(
        session, "patient_bloodgroup", selected = preset$patient_bloodgroup
      )
      updateNumericInput(session, "patient_pra", value = preset$patient_pra)
      updateNumericInput(session, "patient_epts", value = preset$patient_epts)
      updateNumericInput(
        session, "patient_hla_mismatch_mean",
        value = preset$patient_hla_mismatch_mean
      )
      updateNumericInput(
        session, "patient_hla_mismatch_sd",
        value = preset$patient_hla_mismatch_sd
      )
      updateNumericInput(session, "patient_waityears", value = 0)
      updateNumericInput(session, "patient_waitmonths", value = 0)
      
      if (identical(input$patient_state, "") || is.null(input$patient_state)) {
        updateSelectInput(session, "patient_state", selected = "NSW")
      }
    })
  })
  
  state_debts <- reactive({
    tibble(
      state = c("NSW", "VIC", "QLD", "SA", "WA"),
      debt = c(
        input$debt_nsw, input$debt_vic, input$debt_qld,
        input$debt_sa, input$debt_wa
      )
    )
  })
  
  output$debt_sum_check <- renderUI({
    total <- sum(state_debts()$debt, na.rm = TRUE)
    
    if (total == 0) {
      div(
        class = "alert alert-success mt-2",
        paste0("Sum of debts = ", total, ". The debts are balanced.")
      )
    } else {
      div(
        class = "alert alert-warning mt-2",
        paste0("Sum of debts = ", total, ". State debts should sum to zero.")
      )
    }
  })
  
  simulation_result <- reactiveVal(NULL)
  simulation_state <- reactiveVal(NULL)
  simulation_running <- reactiveVal(FALSE)
  cancellation_requested <- reactiveVal(FALSE)
  
  close_simulation_progress <- function(status = "complete") {
    session$sendCustomMessage(
      "hide-estimated-progress",
      list(status = status)
    )
  }
  
  observeEvent(input$cancel_simulation, {
    if (isTRUE(simulation_running())) {
      cancellation_requested(TRUE)
    }
  }, ignoreInit = TRUE)
  
  run_next_simulation_batch <- function() {
    # This function is called by later::later(), outside a reactive consumer.
    # Reactive values must therefore be read inside isolate().
    if (!isTRUE(isolate(simulation_running()))) return(invisible(NULL))
    
    if (isTRUE(isolate(cancellation_requested()))) {
      simulation_running(FALSE)
      simulation_state(NULL)
      close_simulation_progress("cancelled")
      session$sendCustomMessage(
        "simulation-notification",
        list(
          text = "Simulation cancelled. No partial result was saved.",
          type = "message",
          duration = 4000
        )
      )
      return(invisible(NULL))
    }
    
    state <- isolate(simulation_state())
    if (is.null(state)) return(invisible(NULL))
    
    state <- process_simulation_batch(state)
    simulation_state(state)
    
    completed <- min(state$next_donor - 1L, state$n_donors)
    percent <- floor(completed / state$n_donors * 100)
    session$sendCustomMessage(
      "update-actual-progress",
      list(
        percent = percent,
        detail = paste0(
          format(completed, big.mark = ","),
          " of ",
          format(state$n_donors, big.mark = ","),
          " donors simulated"
        )
      )
    )
    
    if (completed >= state$n_donors) {
      session$sendCustomMessage(
        "update-actual-progress",
        list(
          percent = 100,
          detail = "All donors simulated; finalising results..."
        )
      )
      
      result <- finalise_simulation(state)
      simulation_running(FALSE)
      simulation_state(NULL)
      simulation_result(result)
      close_simulation_progress("complete")
    } else {
      later::later(run_next_simulation_batch, delay = 0.01)
    }
    
    invisible(NULL)
  }
  
  observeEvent(input$calculate, {
    scenarios$active_id <- NULL
    scenario_inputs <- isolate(capture_scenario_inputs())
    matching_id <- find_matching_scenario_id(scenario_inputs)
    
    if (!is.null(matching_id)) {
      close_simulation_progress("reloaded")
      matching_record <- isolate(scenarios$records[[matching_id]])
      scenarios$active_id <- matching_id
      restore_scenario_inputs(matching_record$inputs)
      
      showModal(
        modalDialog(
          title = "Previously run scenario",
          paste0(
            "This exact scenario has already been run as scenario ",
            matching_record$name,
            ". The saved result has been reloaded instead of rerunning the simulation."
          ),
          easyClose = TRUE,
          footer = modalButton("OK")
        )
      )
      return()
    }
    
    validate(
      need(
        !is.na(input$patient_waityears) &&
          input$patient_waityears >= 0 &&
          input$patient_waityears == floor(input$patient_waityears),
        "Waiting time years must be a non-negative whole number."
      ),
      need(
        !is.na(input$patient_waitmonths) &&
          input$patient_waitmonths >= 0 &&
          input$patient_waitmonths <= 11 &&
          input$patient_waitmonths == floor(input$patient_waitmonths),
        "Waiting time months must be a whole number from 0 to 11."
      ),
      need(input$patient_state != "", "Select a transplant unit state."),
      need(!is.na(input$patient_age), "Enter the patient's age."),
      need(input$patient_age >= 0 && input$patient_age <= 80,
           "Age must be between 0 and 80."),
      need(input$patient_bloodgroup != "", "Select a blood group."),
      need(!is.na(input$patient_pra) && input$patient_pra >= 0 &&
             input$patient_pra <= 100, "PRA must be between 0 and 100."),
      need(!is.na(input$patient_epts) && input$patient_epts >= 1 &&
             input$patient_epts <= 100, "EPTS must be between 1 and 100."),
      need(!is.na(input$patient_hla_mismatch_mean) &&
             input$patient_hla_mismatch_mean >= 0,
           "Enter a non-negative average HLA mismatch."),
      need(!is.na(input$patient_hla_mismatch_sd) &&
             input$patient_hla_mismatch_sd >= 0,
           "Enter a non-negative HLA mismatch SD."),
      need(
        !is.na(input$simulation_seed) &&
          input$simulation_seed >= 1 &&
          input$simulation_seed <= .Machine$integer.max &&
          input$simulation_seed == floor(input$simulation_seed),
        "Simulation seed must be a valid positive whole number."
      ),
      need(!is.na(input$n_donors) && input$n_donors >= 100,
           "Simulate at least 100 donors."),
      need(
        !is.na(input$simulation_chunk_size) &&
          input$simulation_chunk_size >= 1 &&
          input$simulation_chunk_size <= input$n_donors &&
          input$simulation_chunk_size == floor(input$simulation_chunk_size),
        paste(
          "Donor batch size must be a whole number between 1 and",
          "the number of simulated donors."
        )
      ),
      need(!is.na(input$annual_donors) && input$annual_donors > 0,
           "Annual donors must be greater than zero."),
      need(!is.na(input$offer_acceptance_percent) &&
             input$offer_acceptance_percent >= 0 &&
             input$offer_acceptance_percent <= 100,
           "Offer acceptance probability must be between 0% and 100%."),
      need(sum(state_debts()$debt, na.rm = TRUE) == 0,
           "State debts must sum to zero.")
    )
    
    new_patient <- tibble(patient_id = "new") |>
      mutate(
        patient_waityears =
          input$patient_waityears + input$patient_waitmonths / 12,
        patient_state = input$patient_state,
        patient_age = input$patient_age,
        patient_bloodgroup = input$patient_bloodgroup,
        patient_pra = input$patient_pra,
        patient_epts = input$patient_epts,
        patient_hla_mismatch_mean = input$patient_hla_mismatch_mean,
        patient_hla_mismatch_sd = input$patient_hla_mismatch_sd,
        patient_national_urgent = as.numeric(input$patient_national_urgent),
        patient_state_urgent = as.numeric(input$patient_state_urgent),
        patient_prior_donor = as.numeric(input$patient_prior_donor),
        patient_kidney_after_other_organ = as.numeric(
          input$patient_kidney_after_other_organ
        ),
        patient_spk = as.numeric(input$patient_spk)
      )
    
    cancellation_requested(FALSE)
    simulation_state(prepare_simulation(
      new_patient = new_patient,
      waitlist = waitlist,
      donors = donors,
      state_debts = state_debts(),
      n_donors = input$n_donors,
      donors_per_year = input$annual_donors,
      offer_acceptance_prob = input$offer_acceptance_percent / 100,
      seed = as.integer(input$simulation_seed),
      chunk_size = as.integer(input$simulation_chunk_size)
    ))
    simulation_running(TRUE)
    
    session$sendCustomMessage(
      "start-actual-progress",
      list(
        detail = paste0(
          "0 of ",
          format(input$n_donors, big.mark = ","),
          " donors simulated"
        )
      )
    )
    
    later::later(run_next_simulation_batch, delay = 0.01)
  }, priority = 100)
  
  observeEvent(simulation_result(), {
    result <- simulation_result()
    req(result)
    
    scenario_id <- length(scenarios$records) + 1L
    record <- list(
      id = scenario_id,
      name = as.character(scenario_id),
      compare = FALSE,
      inputs = isolate(capture_scenario_inputs()),
      result = result,
      run_time = Sys.time()
    )
    scenarios$records <- append(scenarios$records, list(record))
    scenarios$active_id <- scenario_id
    
    local({
      id <- scenario_id
      observeEvent(input[[paste0("scenario_save_name_", id)]], {
        records <- scenarios$records
        if (length(records) >= id) {
          new_name <- trimws(input[[paste0("scenario_name_", id)]])
          records[[id]]$name <- if (nzchar(new_name)) new_name else as.character(id)
          scenarios$records <- records
          showNotification(
            paste0("Scenario ", id, " name saved."),
            type = "message",
            duration = 2
          )
        }
      })
      
      observeEvent(input[[paste0("scenario_compare_", id)]], {
        records <- scenarios$records
        if (length(records) >= id) {
          records[[id]]$compare <- isTRUE(input[[paste0("scenario_compare_", id)]])
          scenarios$records <- records
        }
      }, ignoreInit = TRUE)
      
      observeEvent(input[[paste0("scenario_log_load_", id)]], {
        records <- scenarios$records
        if (length(records) >= id) {
          scenarios$active_id <- id
          restore_scenario_inputs(records[[id]]$inputs)
        }
      })
      
      observeEvent(input[[paste0("scenario_compare_load_", id)]], {
        records <- scenarios$records
        if (length(records) >= id) {
          scenarios$active_id <- id
          restore_scenario_inputs(records[[id]]$inputs)
        }
      })
    })
  }, ignoreInit = TRUE)
  
  displayed_simulation_result <- reactive({
    active_id <- scenarios$active_id
    records <- scenarios$records
    if (!is.null(active_id) && length(records) >= active_id) {
      records[[active_id]]$result
    } else {
      simulation_result()
    }
  })
  
  output$scenario_log_ui <- renderUI({
    records <- scenarios$records
    if (length(records) == 0) {
      return(div(class = "scenario-empty-message", "No scenarios have been run yet."))
    }
    
    div(
      class = "scenario-log-list",
      lapply(records, function(record) {
        summary_rows <- scenario_summary_rows(record$inputs)
        card(
          class = "scenario-log-card",
          card_header(
            span(paste("Scenario", record$id)),
            span(class = "text-muted small", format(record$run_time, "%d %b %Y %H:%M:%S"))
          ),
          card_body(
            div(
              class = "scenario-log-controls",
              textInput(
                paste0("scenario_name_", record$id),
                "Scenario name",
                value = record$name,
                width = "100%"
              ),
              actionButton(
                paste0("scenario_save_name_", record$id),
                "Save name",
                icon = icon("floppy-disk"),
                class = "btn-outline-secondary btn-sm"
              ),
              checkboxInput(
                paste0("scenario_compare_", record$id),
                "Add to comparison",
                value = record$compare
              ),
              actionButton(
                paste0("scenario_log_load_", record$id),
                "Load scenario",
                icon = icon("arrow-rotate-left"),
                class = "btn-primary btn-sm"
              )
            ),
            tags$table(
              class = "scenario-input-table",
              tags$tbody(
                lapply(names(summary_rows), function(label) {
                  tags$tr(tags$th(label), tags$td(summary_rows[[label]]))
                })
              )
            )
          )
        )
      })
    )
  })
  
  output$scenario_comparison_ui <- renderUI({
    records <- scenarios$records
    selected <- Filter(function(record) isTRUE(record$compare), records)
    if (length(selected) == 0) return(NULL)
    
    div(
      class = "scenario-comparison-section",
      div(class = "scenario-comparison-title", "Compare saved scenarios"),
      div(
        class = "scenario-comparison-buttons",
        lapply(selected, function(record) {
          actionButton(
            paste0("scenario_compare_load_", record$id),
            record$name,
            class = if (identical(scenarios$active_id, record$id)) {
              "btn-primary btn-sm"
            } else {
              "btn-outline-primary btn-sm"
            }
          )
        })
      )
    )
  })
  
  output$offer_plot <- renderPlot({
    req(input$calculate > 0)
    result <- displayed_simulation_result()
    req(result)
    validate(need(
      nrow(result) > 1,
      "No acceptable simulated donors were identified for this patient."
    ))
    make_plot(
      result,
      probability_threshold = input$plot_threshold_percent / 100
    )
  })
  
  output$interpretation_text <- renderUI({
    req(input$calculate > 0)
    result <- displayed_simulation_result()
    req(result)
    
    if (nrow(result) <= 1) {
      return(tagList(
        p(strong("No offer-probability curve could be estimated.")),
        p(
          class = "text-muted small",
          "No acceptable simulated donors were identified."
        )
      ))
    }
    
    displayed_result <- truncate_probability_curve(
      result,
      input$plot_threshold_percent / 100
    )
    
    q <- get_quantile_summary(result)
    label_25 <- q$label[q$prob == 0.25]
    label_50 <- q$label[q$prob == 0.50]
    label_75 <- q$label[q$prob == 0.75]
    threshold_value <- input$plot_threshold_percent / 100
    threshold_reached <- max(result$cum_offer_prob, na.rm = TRUE) >= threshold_value
    threshold_label <- format_waiting_time(
      max(displayed_result$years, na.rm = TRUE)
    )
    threshold_percentage_value <- threshold_value * 100
    threshold_percent <- if (
      abs(threshold_percentage_value - round(threshold_percentage_value)) < 1e-8
    ) {
      paste0(formatC(
        threshold_percentage_value,
        format = "f",
        digits = 0
      ), "%")
    } else {
      paste0(formatC(
        threshold_percentage_value,
        format = "f",
        digits = 1
      ), "%")
    }
    
    tagList(
      p(
        HTML(paste0(
          "Estimated ",
          "<strong style='color: ",
          quantile_colours[["25th percentile"]],
          " !important;'>25% chance</strong>",
          " of receiving an offer within ",
          "<strong style='color: ",
          quantile_colours[["25th percentile"]],
          " !important;'>",
          htmltools::htmlEscape(label_25),
          "</strong>, a ",
          "<strong style='color: ",
          quantile_colours[["Median"]],
          " !important;'>50% chance</strong>",
          " within ",
          "<strong style='color: ",
          quantile_colours[["Median"]],
          " !important;'>",
          htmltools::htmlEscape(label_50),
          "</strong>, and a ",
          "<strong style='color: ",
          quantile_colours[["75th percentile"]],
          " !important;'>75% chance</strong>",
          " within ",
          "<strong style='color: ",
          quantile_colours[["75th percentile"]],
          " !important;'>",
          htmltools::htmlEscape(label_75),
          "</strong>."
        ))
      ),
      if (isTRUE(threshold_reached)) {
        p(
          HTML(paste0(
            "The estimated probability reaches <strong style='color: ",
            threshold_colour,
            " !important;'>",
            htmltools::htmlEscape(threshold_percent),
            "</strong> after approximately <strong style='color: ",
            threshold_colour,
            " !important;'>",
            htmltools::htmlEscape(threshold_label),
            "</strong>. The plot is truncated after this point."
          ))
        )
      } else {
        p(
          "The estimated probability did not reach ", strong(threshold_percent),
          " within the maximum search period."
        )
      }
    )
  })
  
  output$methods_simulated_donors_text <- renderUI({
    p(
      HTML(paste0(
        "The model simulates <strong>",
        htmltools::htmlEscape(format(input$n_donors, big.mark = ",", scientific = FALSE)),
        "</strong> donors, and simulates their cross-matches against the current waiting list based on each patient's HLA mismatch mean and SD. ",
        "The simulation assumes an average of <strong>",
        htmltools::htmlEscape(format(input$annual_donors, big.mark = ",", scientific = FALSE)),
        "</strong> donors become available each year. ",
        "The simulations are based on the koala R package, available ",
        tags$a(
          "here.",
          href = "https://github.com/james-hedley/koala",
          target = "_blank"
        )
      ))
    )
  })
  
  output$methods_acceptance_text <- renderUI({
    acceptance_text <- paste0(input$offer_acceptance_percent, "%")
    
    p(
      HTML(paste0(
        "Each higher-ranked patient is assumed to accept an offer with probability ",
        "<strong>",
        htmltools::htmlEscape(acceptance_text),
        "</strong><span style='font-weight: 400;'>.</span>"
      ))
    )
  })
  
  output$methods_n_donors <- renderText({
    format(input$n_donors, big.mark = ",", scientific = FALSE)
  })
  
  output$methods_annual_donors <- renderText({
    format(input$annual_donors, big.mark = ",", scientific = FALSE)
  })
  
  output$methods_acceptance_prob <- renderText({
    paste0(input$offer_acceptance_percent, "%")
  })
}

shinyApp(ui = ui, server = server)
