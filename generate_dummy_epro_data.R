library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(xml2)

set.seed(20260721)

n_participants <- 150
participant_ids <- sprintf("P%04d", seq_len(n_participants))
study_dates <- seq(as.Date("2026-05-01"), as.Date("2027-04-30"), by = "day")
study_months <- format(seq(as.Date("2026-05-01"), as.Date("2027-04-01"), by = "month"), "%Y-%m")

participant_traits <- tibble(
  participant_id = participant_ids,
  programme_start_date = sample(
    seq(as.Date("2026-05-01"), as.Date("2026-05-31"), by = "day"),
    n_participants,
    replace = TRUE
  ),
  baseline_cycle_length = sample(24:35, n_participants, replace = TRUE),
  cycle_variability = sample(1:4, n_participants, replace = TRUE, prob = c(.25, .35, .25, .15)),
  bleeding_length = sample(2:7, n_participants, replace = TRUE),
  days_to_first_onset = sample(0:27, n_participants, replace = TRUE),
  bleeding_active = rbinom(n_participants, 1, 0.84),
  pain_tendency = runif(n_participants, 0.02, 0.22),
  discharge_tendency = runif(n_participants, 0.45, 0.85),
  reporting_tendency = runif(n_participants, 0.72, 0.97),
  reporting_gap_start = sample(45:250, n_participants, replace = TRUE),
  reporting_gap_length = sample(c(0, 0, 0, 7, 14, 21), n_participants, replace = TRUE)
)

# Participant-specific latent cycles vary from one cycle to the next. A small
# minority are substantially prolonged, reflecting the irregular patterns that
# are important to test after contraceptive initiation.
cycle_schedules <- lapply(seq_len(n_participants), function(index) {
  person <- participant_traits[index, ]
  onset <- person$programme_start_date + person$days_to_first_onset
  rows <- list()
  cycle_number <- 1L
  while (onset <= max(study_dates)) {
    prolonged <- sample(c(0L, 7L, 14L, 28L), 1, prob = c(.87, .07, .04, .02))
    length_days <- round(rnorm(1, person$baseline_cycle_length, person$cycle_variability)) + prolonged
    length_days <- max(21L, min(70L, length_days))
    rows[[cycle_number]] <- tibble(
      participant_id = person$participant_id,
      latent_cycle_number = cycle_number,
      latent_onset_date = onset,
      latent_next_onset_date = onset + length_days,
      latent_cycle_length = length_days,
      latent_bleeding_length = max(2L, min(8L, person$bleeding_length + sample(-1:1, 1)))
    )
    onset <- onset + length_days
    cycle_number <- cycle_number + 1L
  }
  bind_rows(rows)
})
names(cycle_schedules) <- participant_ids

all_daily <- crossing(
  participant_id = participant_ids,
  report_date = study_dates
) |>
  left_join(participant_traits, by = "participant_id") |>
  filter(report_date >= programme_start_date) |>
  group_by(participant_id) |>
  group_modify(function(.x, .y) {
    schedule <- cycle_schedules[[.y$participant_id]]
    index <- findInterval(.x$report_date, schedule$latent_onset_date)
    valid <- index > 0
    .x$latent_cycle_number <- ifelse(valid, schedule$latent_cycle_number[pmax(index, 1)], NA_integer_)
    .x$cycle_day <- ifelse(
      valid,
      as.integer(.x$report_date - schedule$latent_onset_date[pmax(index, 1)]) + 1L,
      NA_integer_
    )
    .x$latent_bleeding_length <- ifelse(
      valid,
      schedule$latent_bleeding_length[pmax(index, 1)],
      NA_integer_
    )
    .x
  }) |>
  ungroup() |>
  mutate(
    scheduled_bleeding = bleeding_active == 1 & !is.na(cycle_day) & cycle_day <= latent_bleeding_length,
    mid_cycle_spotting = bleeding_active == 1 & cycle_day %in% 11:17,
    menstrual_bleeding_yesterday = rbinom(
      n(),
      1,
      case_when(
        scheduled_bleeding ~ 0.97,
        mid_cycle_spotting ~ 0.035,
        bleeding_active == 1 ~ 0.012,
        TRUE ~ 0.004
      )
    ),
    bleeding_expected_yesterday = case_when(
      menstrual_bleeding_yesterday == 1 & scheduled_bleeding ~ sample(
        1:3, n(), replace = TRUE, prob = c(0.08, 0.84, 0.08)
      ),
      menstrual_bleeding_yesterday == 1 ~ sample(
        1:3, n(), replace = TRUE, prob = c(0.70, 0.12, 0.18)
      ),
      scheduled_bleeding ~ sample(
        1:3, n(), replace = TRUE, prob = c(0.18, 0.68, 0.14)
      ),
      TRUE ~ sample(1:3, n(), replace = TRUE, prob = c(0.82, 0.07, 0.11))
    )
  )

all_daily$bleeding_amount_yesterday <- NA_integer_
bleeding_rows <- which(all_daily$menstrual_bleeding_yesterday == 1)
for (row in bleeding_rows) {
  early_cycle <- isTRUE(all_daily$cycle_day[row] <= 2)
  all_daily$bleeding_amount_yesterday[row] <- sample(
    1:4,
    1,
    prob = if (early_cycle) c(0.10, 0.25, 0.42, 0.23) else c(0.30, 0.40, 0.23, 0.07)
  )
}

all_daily$bleeding_amount_expected_yesterday <- NA_integer_
expected_bleeding_rows <- which(
  all_daily$menstrual_bleeding_yesterday == 1 &
    all_daily$bleeding_expected_yesterday == 2
)
for (row in expected_bleeding_rows) {
  amount <- all_daily$bleeding_amount_yesterday[row]
  probabilities <- switch(
    as.character(amount),
    `1` = c(0.64, 0.25, 0.04, 0.07),
    `2` = c(0.67, 0.18, 0.08, 0.07),
    `3` = c(0.60, 0.08, 0.25, 0.07),
    `4` = c(0.45, 0.03, 0.46, 0.06)
  )
  all_daily$bleeding_amount_expected_yesterday[row] <- sample(1:4, 1, prob = probabilities)
}

pain_probability <- pmin(
  0.88,
  all_daily$pain_tendency +
    0.42 * all_daily$menstrual_bleeding_yesterday +
    0.10 * (all_daily$cycle_day %in% c(1, 2)) +
    0.05 * (all_daily$cycle_day %in% 13:16)
)
all_daily$menstrual_pain_yesterday <- rbinom(nrow(all_daily), 1, pain_probability)

all_daily$pain_expected_yesterday <- ifelse(
  all_daily$menstrual_pain_yesterday == 1,
  vapply(
    seq_len(nrow(all_daily)),
    function(row) sample(
      1:3,
      1,
      prob = if (all_daily$menstrual_bleeding_yesterday[row] == 1) {
        c(0.18, 0.70, 0.12)
      } else {
        c(0.55, 0.25, 0.20)
      }
    ),
    integer(1)
  ),
  sample(1:3, nrow(all_daily), replace = TRUE, prob = c(0.83, 0.07, 0.10))
)

all_daily$pain_amount_yesterday <- NA_integer_
pain_rows <- which(all_daily$menstrual_pain_yesterday == 1)
for (row in pain_rows) {
  all_daily$pain_amount_yesterday[row] <- sample(
    1:4,
    1,
    prob = if (all_daily$menstrual_bleeding_yesterday[row] == 1) {
      c(0.31, 0.39, 0.22, 0.08)
    } else {
      c(0.50, 0.33, 0.13, 0.04)
    }
  )
}

all_daily$pain_amount_expected_yesterday <- NA_integer_
expected_pain_rows <- which(
  all_daily$menstrual_pain_yesterday == 1 & all_daily$pain_expected_yesterday == 2
)
for (row in expected_pain_rows) {
  severity <- all_daily$pain_amount_yesterday[row]
  probabilities <- switch(
    as.character(severity),
    `1` = c(0.69, 0.22, 0.03, 0.06),
    `2` = c(0.65, 0.17, 0.11, 0.07),
    `3` = c(0.54, 0.07, 0.32, 0.07),
    `4` = c(0.40, 0.02, 0.52, 0.06)
  )
  all_daily$pain_amount_expected_yesterday[row] <- sample(1:4, 1, prob = probabilities)
}

all_daily$pain_duration_yesterday <- NA_integer_
for (row in pain_rows) {
  severity <- all_daily$pain_amount_yesterday[row]
  probabilities <- switch(
    as.character(severity),
    `1` = c(0.58, 0.30, 0.10, 0.02),
    `2` = c(0.26, 0.43, 0.25, 0.06),
    `3` = c(0.10, 0.28, 0.42, 0.20),
    `4` = c(0.04, 0.15, 0.38, 0.43)
  )
  all_daily$pain_duration_yesterday[row] <- sample(1:4, 1, prob = probabilities)
}

svg <- read_xml("outputs/bodymap_prototype.svg")
region_nodes <- xml_find_all(
  svg,
  "//*[@id and (self::*[local-name()='path' or local-name()='polygon' or local-name()='rect' or local-name()='circle' or local-name()='ellipse'] or ./*[local-name()='path' or local-name()='polygon' or local-name()='rect' or local-name()='circle' or local-name()='ellipse'])]"
)
region_ids <- xml_attr(region_nodes, "id")
# Synthetic pain-location weights are deliberately concentrated in regions that
# are plausible for menstrual and contraceptive-related symptoms. They are not
# intended to reproduce a clinical prevalence estimate. Distal limbs remain
# possible, but uncommon, so every SVG response code can still be exercised.
region_weights <- rep(0.6, length(region_ids))
region_weights[str_detect(region_ids, "Foot|Hand|Arm_|Leg_Lower")] <- 0.12
region_weights[str_detect(region_ids, "Leg_Upper|Buttock")] <- 0.35
region_weights[str_detect(region_ids, "Neck|Mouth|Ears|Ear_|Face")] <- 0.12
region_weights[str_detect(region_ids, "Back_Upper|Chest_Upper|Thoracic|Sternum")] <- 1.2
region_weights[str_detect(region_ids, "Pate|Temporal")] <- 2.5
region_weights[str_detect(region_ids, "Breast|Nipple")] <- 3.5
region_weights[str_detect(region_ids, "Flank|Hip")] <- 4.5
region_weights[str_detect(region_ids, "Lumbar|Sacral|Rectal")] <- 8
region_weights[str_detect(region_ids, "Abdomen_Upper|Abdomen_Side")] <- 7
region_weights[str_detect(region_ids, "Abdomen_Lower|Pubic|Vaginovulval")] <- 12
region_weights <- region_weights / sum(region_weights)
names(region_weights) <- region_ids

participant_core_regions <- lapply(
  seq_len(n_participants),
  function(index) sample(region_ids, 4, replace = FALSE, prob = region_weights)
)
names(participant_core_regions) <- participant_ids

all_daily$pain_locations_yesterday <- NA_character_
for (row in pain_rows) {
  participant <- all_daily$participant_id[row]
  severity <- all_daily$pain_amount_yesterday[row]
  number_regions <- sample(1:min(4, severity + 1), 1)
  pool <- unique(c(participant_core_regions[[participant]], region_ids))
  pool_weights <- unname(region_weights[pool])
  pool_weights <- pool_weights * if_else(
    pool %in% participant_core_regions[[participant]], 5, 1
  )

  # Bleeding-day pain is more strongly pelvic/lumbosacral. Head and breast pain
  # are more likely outside active bleeding and in the later cycle phase.
  if (all_daily$menstrual_bleeding_yesterday[row] == 1) {
    pool_weights[str_detect(
      pool,
      "Abdomen|Pubic|Vaginovulval|Rectal|Sacral|Lumbar|Flank|Hip"
    )] <- pool_weights[str_detect(
      pool,
      "Abdomen|Pubic|Vaginovulval|Rectal|Sacral|Lumbar|Flank|Hip"
    )] * 1.8
  }
  if (all_daily$cycle_day[row] %in% 20:35) {
    pool_weights[str_detect(pool, "Breast|Nipple|Pate|Temporal")] <-
      pool_weights[str_detect(pool, "Breast|Nipple|Pate|Temporal")] * 1.8
  }
  selected <- sample(pool, number_regions, replace = FALSE, prob = pool_weights)
  all_daily$pain_locations_yesterday[row] <- paste(selected, collapse = " ")
}

# Mimic heterogeneous adherence, weekends, and occasional multi-day reporting
# gaps rather than dropping each day with one common independent probability.
daily <- all_daily |>
  mutate(
    programme_day = as.integer(report_date - programme_start_date) + 1L,
    in_reporting_gap = programme_day >= reporting_gap_start &
      programme_day < reporting_gap_start + reporting_gap_length,
    weekend = as.POSIXlt(report_date)$wday %in% c(0, 6),
    submission_probability = reporting_tendency * if_else(weekend, .94, 1) *
      if_else(in_reporting_gap, .08, 1),
    submitted = rbinom(n(), 1, pmin(.995, submission_probability))
  ) |>
  filter(submitted == 1) |>
  mutate(
    submitted_at = sprintf(
      "%sT%02d:%02d:00+01:00",
      report_date + 1,
      sample(6:10, n(), replace = TRUE),
      sample(0:59, n(), replace = TRUE)
    )
  ) |>
  select(
    participant_id,
    programme_start_date,
    report_date,
    submitted_at,
    menstrual_bleeding_yesterday,
    bleeding_expected_yesterday,
    bleeding_amount_yesterday,
    bleeding_amount_expected_yesterday,
    menstrual_pain_yesterday,
    pain_expected_yesterday,
    pain_amount_yesterday,
    pain_amount_expected_yesterday,
    pain_duration_yesterday,
    pain_locations_yesterday
  ) |>
  arrange(participant_id, report_date)

daily_month <- daily |>
  mutate(reference_month = format(report_date, "%Y-%m")) |>
  group_by(participant_id, reference_month) |>
  summarise(
    daily_submissions = n(),
    daily_any_bleeding = as.integer(any(menstrual_bleeding_yesterday == 1)),
    daily_any_pain = as.integer(any(menstrual_pain_yesterday == 1)),
    bleeding_days_observed = sum(menstrual_bleeding_yesterday == 1),
    pain_days_observed = sum(menstrual_pain_yesterday == 1),
    .groups = "drop"
  )

monthly <- crossing(
  participant_id = participant_ids,
  reference_month = study_months
) |>
  left_join(daily_month, by = c("participant_id", "reference_month")) |>
  left_join(participant_traits, by = "participant_id") |>
  mutate(
    monthly_submitted = rbinom(n(), 1, 0.94),
    menstrual_bleeding_last_month = if_else(
      daily_any_bleeding == 1,
      rbinom(n(), 1, 0.96),
      rbinom(n(), 1, 0.06)
    ),
    menstrual_pain_last_month = if_else(
      daily_any_pain == 1,
      rbinom(n(), 1, 0.94),
      rbinom(n(), 1, 0.07)
    ),
    clots_last_month = if_else(
      menstrual_bleeding_last_month == 1,
      rbinom(n(), 1, pmin(0.75, 0.14 + coalesce(bleeding_days_observed, 0) * 0.04)),
      NA_integer_
    ),
    vaginal_discharge_last_month = rbinom(n(), 1, discharge_tendency)
  ) |>
  filter(monthly_submitted == 1)

monthly$bleeding_product_material <- NA_character_
monthly$bleeding_product_material_other <- NA_character_
monthly_bleeding_rows <- which(monthly$menstrual_bleeding_last_month == 1)
product_codes <- as.character(1:11)
product_probabilities <- c(0.31, 0.10, 0.18, 0.07, 0.12, 0.04, 0.07, 0.04, 0.01, 0.03, 0.03)
for (row in monthly_bleeding_rows) {
  if (runif(1) < product_probabilities[10]) {
    selected <- "10"
  } else {
    number_selected <- sample(1:2, 1, prob = c(0.78, 0.22))
    selected <- sample(
      product_codes[-10],
      number_selected,
      replace = FALSE,
      prob = product_probabilities[-10]
    )
  }
  monthly$bleeding_product_material[row] <- paste(selected, collapse = " ")
  if ("11" %in% selected) {
    monthly$bleeding_product_material_other[row] <- sample(
      c("Reusable handmade pad", "Locally made absorbent material"),
      1
    )
  }
}

sample_codes <- function(rows, codes, probabilities) {
  output <- rep(NA_integer_, nrow(monthly))
  output[rows] <- sample(codes, length(rows), replace = TRUE, prob = probabilities)
  output
}

bleed_rows <- which(monthly$menstrual_bleeding_last_month == 1)
clot_rows <- which(monthly$clots_last_month == 1)
monthly$bleeding_days_change <- sample_codes(bleed_rows, 1:3, c(0.24, 0.48, 0.28))
monthly$bleeding_spacing_change <- sample_codes(bleed_rows, 1:3, c(0.22, 0.53, 0.25))
monthly$bleeding_amount_change <- sample_codes(bleed_rows, 1:3, c(0.30, 0.44, 0.26))
monthly$bleeding_start_predictability <- sample_codes(bleed_rows, 1:3, c(0.27, 0.50, 0.23))
monthly$bleeding_stop_predictability <- sample_codes(bleed_rows, 1:3, c(0.23, 0.53, 0.24))
monthly$bleeding_color_change <- sample_codes(bleed_rows, 1:4, c(0.52, 0.25, 0.10, 0.13))
monthly$bleeding_smell_change <- sample_codes(bleed_rows, 1:4, c(0.58, 0.20, 0.08, 0.14))
monthly$clot_number_change <- sample_codes(clot_rows, 1:4, c(0.23, 0.42, 0.22, 0.13))
monthly$clot_size_change <- sample_codes(clot_rows, 1:4, c(0.22, 0.46, 0.19, 0.13))

monthly_pain_rows <- which(monthly$menstrual_pain_last_month == 1)
monthly$pain_severity_change <- sample_codes(monthly_pain_rows, 1:3, c(0.27, 0.47, 0.26))
monthly$pain_days_change <- sample_codes(monthly_pain_rows, 1:3, c(0.25, 0.49, 0.26))
monthly$pain_location_change <- sample_codes(monthly_pain_rows, 1:3, c(0.53, 0.28, 0.19))

discharge_rows <- which(monthly$vaginal_discharge_last_month == 1)
monthly$discharge_change <- sample_codes(discharge_rows, 1:4, c(0.57, 0.23, 0.08, 0.12))

burden <- scales::rescale(
  coalesce(monthly$bleeding_days_observed, 0) + coalesce(monthly$pain_days_observed, 0),
  to = c(0, 1)
)
for (field in c(
  "bleeding_management_impact",
  "pain_management_impact",
  "bleeding_daily_activities_impact",
  "pain_daily_activities_impact"
)) {
  monthly[[field]] <- vapply(
    seq_len(nrow(monthly)),
    function(row) sample(
      1:4,
      1,
      prob = c(
        0.24 - 0.10 * burden[row],
        0.48 - 0.12 * burden[row],
        0.16 + 0.18 * burden[row],
        0.12 + 0.04 * burden[row]
      )
    ),
    integer(1)
  )
}

monthly <- monthly |>
  mutate(
    month_start = as.Date(paste0(reference_month, "-01")),
    next_month_start = as.Date(format(month_start + 32, "%Y-%m-01")),
    submitted_at = sprintf(
      "%sT%02d:%02d:00+01:00",
      next_month_start,
      sample(6:11, n(), replace = TRUE),
      sample(0:59, n(), replace = TRUE)
    )
  ) |>
  select(
    participant_id,
    programme_start_date,
    reference_month,
    submitted_at,
    menstrual_bleeding_last_month,
    menstrual_pain_last_month,
    clots_last_month,
    vaginal_discharge_last_month,
    bleeding_product_material,
    bleeding_product_material_other,
    bleeding_days_change,
    bleeding_spacing_change,
    bleeding_amount_change,
    bleeding_start_predictability,
    bleeding_stop_predictability,
    bleeding_color_change,
    bleeding_smell_change,
    clot_number_change,
    clot_size_change,
    pain_severity_change,
    pain_days_change,
    pain_location_change,
    discharge_change,
    bleeding_management_impact,
    pain_management_impact,
    bleeding_daily_activities_impact,
    pain_daily_activities_impact
  ) |>
  arrange(participant_id, reference_month)

write_csv(daily, "outputs/dummy_daily_epro_data.csv", na = "")
write_csv(monthly, "outputs/dummy_monthly_epro_data.csv", na = "")

stopifnot(
  n_distinct(daily$participant_id) == n_participants,
  all(is.na(daily$bleeding_amount_yesterday[daily$menstrual_bleeding_yesterday == 0])),
  all(is.na(daily$pain_amount_yesterday[daily$menstrual_pain_yesterday == 0])),
  all(is.na(monthly$clots_last_month[monthly$menstrual_bleeding_last_month == 0])),
  all(is.na(monthly$pain_severity_change[monthly$menstrual_pain_last_month == 0])),
  all(is.na(monthly$discharge_change[monthly$vaginal_discharge_last_month == 0]))
)
