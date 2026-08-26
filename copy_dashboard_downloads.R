library(yaml)

config <- read_yaml("dashboard_config.yml")
mode <- tolower(config$data_source$mode)
mode_settings <- config$data_source[[mode]]
publish_action_lists <- isTRUE(mode_settings$publish_action_lists_in_rendered_site)

action_files <- c(
  "reminder_targets_3_consecutive_missed_days.csv",
  "eligible_not_enrolled_targets.csv",
  "enrolled_no_daily_submission_targets.csv",
  "monthly_report_due_targets.csv",
  "protocol_deviation_violation_review.csv",
  "participant_progress_summary.csv",
  "participant_followup_gap_summary.csv"
)
site_codes <- names(unlist(config$labels$sites))
site_action_files <- unlist(lapply(
  action_files,
  function(path) vapply(
    site_codes,
    function(site_code) sub("\\.csv$", paste0("_", site_code, ".csv"), path),
    character(1)
  )
))
action_files <- unique(c(action_files, site_action_files))
development_files <- c(
  "dummy_recruitment_form_data.csv",
  "dummy_enrolment_sociodemographics_data.csv",
  "dummy_CIMC_pro_data.csv"
)

dir.create("docs", showWarnings = FALSE)

copy_checked <- function(files) {
  missing <- files[!file.exists(files)]
  if (length(missing) > 0) {
    stop("Dashboard download files were not generated: ", paste(missing, collapse = ", "))
  }
  copied <- file.copy(files, file.path("docs", files), overwrite = TRUE)
  if (!all(copied)) stop("One or more dashboard download files could not be copied into docs.")
}

if (mode == "development") {
  copy_checked(development_files)
} else {
  published_development <- file.path("docs", development_files)
  file.remove(published_development[file.exists(published_development)])
}

if (publish_action_lists) {
  copy_checked(action_files)
} else {
  # Remove known stale copies from an earlier development render. The files
  # remain available in the protected project output directory.
  published_actions <- file.path("docs", action_files)
  file.remove(published_actions[file.exists(published_actions)])
}
