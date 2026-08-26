op_ensure_fields <- function(data, fields) {
  for (field in setdiff(fields, names(data))) data[[field]] <- NA
  data
}

op_prepare_tracking <- function(enrolment, cimc, config, data_cutoff) {
  protocol_days <- as.integer(config$study$expected_daily_reports)
  monthly_due_day <- as.integer(config$study$monthly_due_day)
  site_labels <- unlist(config$labels$sites)
  if (protocol_days < 1L) stop("study.expected_daily_reports must be at least 1.")

  enrolment <- op_ensure_fields(
    enrolment,
    c("uniqueid", "organization", "site", "enrollment_date", "method_stop_date")
  ) |>
    dplyr::mutate(
      uniqueid = as.character(uniqueid),
      enrollment_date = as.Date(as.character(enrollment_date)),
      site = dplyr::coalesce(
        as.character(site),
        unname(site_labels[as.character(organization)]),
        "Site unavailable"
      ),
      method_stop_date = as.Date(as.character(method_stop_date)),
      planned_end_date = enrollment_date + protocol_days - 1L,
      observed_end_date = pmin(
        planned_end_date, data_cutoff,
        dplyr::coalesce(method_stop_date, data_cutoff), na.rm = TRUE
      )
    ) |>
    dplyr::filter(!is.na(uniqueid), !is.na(enrollment_date)) |>
    dplyr::distinct(uniqueid, .keep_all = TRUE)

  cimc <- op_ensure_fields(cimc, c("uniqueid", "date", "monthly_report_due")) |>
    dplyr::mutate(
      uniqueid = as.character(uniqueid),
      date = as.Date(as.character(date)),
      monthly_report_due = stringr::str_to_lower(as.character(monthly_report_due))
    )

  daily_keys <- cimc |>
    dplyr::filter(!is.na(uniqueid), !is.na(date)) |>
    dplyr::distinct(uniqueid, date) |>
    dplyr::transmute(key = paste(uniqueid, date)) |>
    dplyr::pull(key)
  monthly_keys <- cimc |>
    dplyr::filter(monthly_report_due == "yes", !is.na(uniqueid), !is.na(date)) |>
    dplyr::distinct(uniqueid, date) |>
    dplyr::transmute(key = paste(uniqueid, date)) |>
    dplyr::pull(key)

  if (nrow(enrolment) == 0) {
    return(list(
      participants = enrolment,
      daily_grid = tibble::tibble(), daily_summary = tibble::tibble(),
      monthly_grid = tibble::tibble(), monthly_summary = tibble::tibble()
    ))
  }

  daily_levels <- c(
    "Daily received", "Daily missed", "Monthly + daily received",
    "Monthly due — missing", "Future / not yet due", "Follow-up ended"
  )
  daily_grid <- tidyr::crossing(
    enrolment |>
      dplyr::select(
        uniqueid, organization, site, enrollment_date, planned_end_date,
        observed_end_date, method_stop_date
      ),
    follow_up_day = seq_len(protocol_days)
  ) |>
    dplyr::mutate(
      calendar_date = enrollment_date + follow_up_day - 1L,
      daily_received = paste(uniqueid, calendar_date) %in% daily_keys,
      monthly_received = paste(uniqueid, calendar_date) %in% monthly_keys,
      monthly_due = calendar_date <= observed_end_date &
        as.integer(format(calendar_date, "%d")) == monthly_due_day,
      due = calendar_date <= observed_end_date,
      daily_missing = due & !daily_received,
      monthly_missing = monthly_due & !monthly_received,
      result = dplyr::case_when(
        !is.na(method_stop_date) & calendar_date > method_stop_date ~ "Follow-up ended",
        calendar_date > data_cutoff ~ "Future / not yet due",
        calendar_date > observed_end_date ~ "Follow-up ended",
        monthly_received ~ "Monthly + daily received",
        monthly_missing ~ "Monthly due — missing",
        daily_received ~ "Daily received",
        TRUE ~ "Daily missed"
      ),
      result = factor(result, levels = daily_levels)
    )

  daily_summary <- daily_grid |>
    dplyr::group_by(
      uniqueid, organization, site, enrollment_date, planned_end_date,
      observed_end_date, method_stop_date
    ) |>
    dplyr::summarise(
      observed_days = sum(due),
      daily_received = sum(due & daily_received),
      missed_days = sum(daily_missing),
      gap_episodes = {
        runs <- rle(daily_missing[due])
        sum(runs$values)
      },
      longest_gap = {
        runs <- rle(daily_missing[due])
        if (any(runs$values)) max(runs$lengths[runs$values]) else 0L
      },
      monthly_due = sum(monthly_due),
      monthly_received = sum(monthly_due & monthly_received),
      monthly_missed = sum(monthly_missing),
      daily_completion = dplyr::if_else(
        observed_days > 0, daily_received / observed_days, NA_real_
      ),
      .groups = "drop"
    )

  study_months <- seq(
    as.Date(format(as.Date(config$study$recruitment_start_date), "%Y-%m-01")),
    as.Date(format(data_cutoff, "%Y-%m-01")),
    by = "month"
  )
  monthly_levels <- c(
    "Monthly received", "Monthly missing", "Not yet due",
    "Not enrolled / not due", "Follow-up ended"
  )
  monthly_grid <- tidyr::crossing(
    enrolment |>
      dplyr::select(
        uniqueid, organization, site, enrollment_date, planned_end_date,
        observed_end_date, method_stop_date
      ),
    month = study_months
  ) |>
    dplyr::mutate(
      due_date = month + monthly_due_day - 1L,
      received = paste(uniqueid, due_date) %in% monthly_keys,
      expected = due_date >= enrollment_date & due_date <= observed_end_date,
      missing = expected & !received & due_date <= data_cutoff,
      result = dplyr::case_when(
        received ~ "Monthly received",
        !is.na(method_stop_date) & due_date > method_stop_date ~ "Follow-up ended",
        due_date > data_cutoff ~ "Not yet due",
        missing ~ "Monthly missing",
        TRUE ~ "Not enrolled / not due"
      ),
      result = factor(result, levels = monthly_levels)
    )

  monthly_summary <- monthly_grid |>
    dplyr::group_by(uniqueid, organization, site) |>
    dplyr::summarise(
      monthly_due = sum(expected),
      monthly_received = sum(expected & received),
      monthly_missed = sum(missing),
      monthly_completion = dplyr::if_else(
        monthly_due > 0, monthly_received / monthly_due, NA_real_
      ),
      .groups = "drop"
    )

  list(
    participants = enrolment,
    daily_grid = daily_grid,
    daily_summary = daily_summary,
    monthly_grid = monthly_grid,
    monthly_summary = monthly_summary
  )
}

op_sort_daily <- function(summary, method = "gap_episodes") {
  valid <- c("gap_episodes", "missed_days", "longest_gap", "monthly_missed")
  method <- tolower(method)
  if (!method %in% valid) {
    stop("monitoring.full_tracking_sort must be one of: ", paste(valid, collapse = ", "))
  }
  summary |>
    dplyr::mutate(.sort_value = .data[[method]]) |>
    dplyr::arrange(
      dplyr::desc(.sort_value), dplyr::desc(longest_gap),
      dplyr::desc(missed_days), uniqueid
    )
}

op_cycle_quality_checks <- function(daily_data, config) {
  daily_data <- op_ensure_fields(
    daily_data,
    c(
      "participant_id", "report_date", "menstrual_bleeding_yesterday",
      "bleeding_amount_yesterday"
    )
  ) |>
    dplyr::mutate(
      participant_id = as.character(participant_id),
      report_date = as.Date(as.character(report_date)),
      menstrual_bleeding_yesterday = suppressWarnings(as.numeric(as.character(menstrual_bleeding_yesterday))),
      bleeding_amount_yesterday = suppressWarnings(as.numeric(as.character(bleeding_amount_yesterday)))
    ) |>
    dplyr::filter(!is.na(participant_id), !is.na(report_date)) |>
    dplyr::arrange(participant_id, report_date)

  spotting_starts <- isTRUE(config$cycle_inference$spotting_starts_cycle)
  require_next <- isTRUE(config$cycle_inference$require_next_day_blood)
  minimum_gap <- as.integer(config$cycle_inference$minimum_days_between_onsets)
  long_review <- as.integer(config$cycle_inference$long_cycle_review_days)

  candidates <- daily_data |>
    dplyr::group_by(participant_id) |>
    dplyr::mutate(
      blood_reported = menstrual_bleeding_yesterday == 1,
      qualifying_day = blood_reported &
        (spotting_starts | bleeding_amount_yesterday >= 2),
      next_day_observed = dplyr::lead(report_date) == report_date + 1,
      next_day_blood = next_day_observed & dplyr::lead(blood_reported),
      confirmed_candidate = qualifying_day & (!require_next | next_day_blood)
    ) |>
    dplyr::ungroup()

  select_onsets <- function(data) {
    if (nrow(data) == 0) return(data)
    keep <- logical(nrow(data))
    last_kept <- as.Date(NA)
    for (index in seq_len(nrow(data))) {
      current <- data$report_date[[index]]
      if (is.na(last_kept) || as.integer(current - last_kept) >= minimum_gap) {
        keep[[index]] <- TRUE
        last_kept <- current
      }
    }
    data[keep, , drop = FALSE]
  }

  onsets <- candidates |>
    dplyr::filter(confirmed_candidate) |>
    dplyr::group_by(participant_id) |>
    dplyr::group_modify(~ select_onsets(.x)) |>
    dplyr::ungroup() |>
    dplyr::arrange(participant_id, report_date) |>
    dplyr::group_by(participant_id) |>
    dplyr::mutate(cycle_length = as.integer(dplyr::lead(report_date) - report_date)) |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(cycle_length))

  tibble::tibble(
    Check = c(
      "Cycle inference: qualifying bleeding day lacks required next-day observation",
      "Cycle inference: completed cycle length is zero or negative",
      "Cycle inference: completed cycle shorter than configured onset interval",
      "Cycle inference: completed cycle above review threshold"
    ),
    Issues = c(
      sum(candidates$qualifying_day & require_next & !candidates$next_day_observed, na.rm = TRUE),
      sum(onsets$cycle_length <= 0, na.rm = TRUE),
      sum(onsets$cycle_length < minimum_gap, na.rm = TRUE),
      sum(onsets$cycle_length > long_review, na.rm = TRUE)
    )
  )
}

op_bodymap_quality_check <- function(daily_data, location_field, svg_file) {
  daily_data <- op_ensure_fields(daily_data, location_field)
  reported_codes <- daily_data[[location_field]] |>
    as.character() |>
    dplyr::coalesce("") |>
    stringr::str_split("\\s+") |>
    unlist(use.names = FALSE)
  reported_codes <- unique(reported_codes[reported_codes != ""])

  drawable_ids <- xml2::read_xml(svg_file) |>
    xml2::xml_find_all(
      "//*[local-name()='path' or local-name()='polygon' or local-name()='rect' or local-name()='circle' or local-name()='ellipse'][@id]"
    ) |>
    xml2::xml_attr("id") |>
    unique()

  tibble::tibble(
    Check = "Body map: unmatched detailed response codes",
    Issues = length(setdiff(reported_codes, drawable_ids))
  )
}

op_quality_checks <- function(daily_data, monthly_data, location_field, config = NULL) {
  daily_data <- op_ensure_fields(
    daily_data,
    c(
      "participant_id", "report_date", "programme_start_date",
      "menstrual_bleeding_yesterday", "bleeding_amount_yesterday",
      "bleeding_amount_expected_yesterday", "menstrual_pain_yesterday",
      "pain_amount_yesterday", "pain_duration_yesterday", location_field
    )
  )
  monthly_data <- op_ensure_fields(
    monthly_data,
    c(
      "participant_id", "reference_month", "menstrual_bleeding_last_month",
      "clots_last_month", "bleeding_product_material", "bleeding_days_change",
      "clot_number_change", "clot_size_change", "menstrual_pain_last_month",
      "pain_severity_change", "pain_days_change", "pain_location_change"
    )
  )
  structural <- tibble::tibble(
    Check = c(
      "Daily: duplicate participant-date records",
      "Daily: report before programme start",
      "Daily: bleeding follow-up outside skip condition",
      "Daily: pain follow-up outside skip condition",
      "Monthly: duplicate participant-month records",
      "Monthly: bleeding follow-up outside skip condition",
      "Monthly: clot comparison outside skip condition",
      "Monthly: pain comparison outside skip condition",
      "Monthly: more than two products/materials",
      "Monthly: no product selected with another option"
    ),
    Issues = c(
      sum(duplicated(daily_data[c("participant_id", "report_date")])),
      sum(daily_data$report_date < daily_data$programme_start_date, na.rm = TRUE),
      sum(
        daily_data$menstrual_bleeding_yesterday == 0 &
          (!is.na(daily_data$bleeding_amount_yesterday) |
             !is.na(daily_data$bleeding_amount_expected_yesterday)), na.rm = TRUE
      ),
      sum(
        daily_data$menstrual_pain_yesterday == 0 &
          (!is.na(daily_data$pain_amount_yesterday) |
             !is.na(daily_data$pain_duration_yesterday) |
             (!is.na(daily_data[[location_field]]) &
                stringr::str_trim(as.character(daily_data[[location_field]])) != "")),
        na.rm = TRUE
      ),
      sum(duplicated(monthly_data[c("participant_id", "reference_month")])),
      sum(
        monthly_data$menstrual_bleeding_last_month == 0 &
          (!is.na(monthly_data$clots_last_month) |
             (!is.na(monthly_data$bleeding_product_material) &
                monthly_data$bleeding_product_material != "") |
             !is.na(monthly_data$bleeding_days_change)), na.rm = TRUE
      ),
      sum(
        monthly_data$clots_last_month != 1 &
          (!is.na(monthly_data$clot_number_change) |
             !is.na(monthly_data$clot_size_change)), na.rm = TRUE
      ),
      sum(
        monthly_data$menstrual_pain_last_month == 0 &
          (!is.na(monthly_data$pain_severity_change) |
             !is.na(monthly_data$pain_days_change) |
             !is.na(monthly_data$pain_location_change)), na.rm = TRUE
      ),
      sum(stringr::str_count(
        dplyr::coalesce(as.character(monthly_data$bleeding_product_material), ""), "\\S+"
      ) > 2),
      sum(
        stringr::str_detect(
          dplyr::coalesce(as.character(monthly_data$bleeding_product_material), ""),
          "(^| )10( |$)"
        ) & stringr::str_count(
          dplyr::coalesce(as.character(monthly_data$bleeding_product_material), ""), "\\S+"
        ) > 1
      )
    )
  )

  if (is.null(config)) return(structural)
  dplyr::bind_rows(
    structural,
    op_cycle_quality_checks(daily_data, config),
    op_bodymap_quality_check(daily_data, location_field, config$files$bodymap_svg)
  )
}
