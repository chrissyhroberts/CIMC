required_packages <- c("dplyr", "httr2", "jsonlite", "readr", "stringr", "tibble", "yaml")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Install required packages before preparing dashboard data: ",
       paste(missing_packages, collapse = ", "))
}

suppressPackageStartupMessages({
  library(dplyr)
  library(httr2)
  library(jsonlite)
  library(readr)
  library(stringr)
  library(tibble)
  library(yaml)
})

config_path <- "dashboard_config.yml"
if (!file.exists(config_path)) stop("Cannot find ", config_path)
config <- read_yaml(config_path)
mode <- tolower(config$data_source$mode)
if (!mode %in% c("development", "production")) {
  stop("data_source.mode must be either 'development' or 'production'.")
}

runtime_files <- unlist(config$files[c(
  "recruitment", "enrolment", "cimc", "daily_data", "monthly_data",
  "refresh_metadata"
)])
dir.create(dirname(runtime_files[[1]]), recursive = TRUE, showWarnings = FALSE)

write_refresh_metadata <- function(mode, sources, destination, effective_data_cutoff) {
  metadata <- tibble(
    mode = mode,
    prepared_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    effective_data_cutoff = as.character(effective_data_cutoff),
    dataset = names(sources),
    source = unname(sources),
    rows = vapply(
      unname(runtime_files[names(sources)]),
      function(path) suppressWarnings(nrow(read_csv(path, show_col_types = FALSE))),
      integer(1)
    )
  )
  write_csv(metadata, destination, na = "")
}

stage_development_data <- function() {
  development <- config$data_source$development
  if (isTRUE(development$regenerate_dummy_data)) {
    source("generate_monitoring_dummy_data.R", local = new.env(parent = globalenv()))
  }

  source_files <- unlist(development$files)
  missing <- names(source_files)[!file.exists(source_files)]
  if (length(missing) > 0) {
    stop("Development source files are missing for: ", paste(missing, collapse = ", "),
         ". Run generate_monitoring_dummy_data.R or enable regenerate_dummy_data.")
  }

  copied <- file.copy(
    unname(source_files),
    unname(runtime_files[names(source_files)]),
    overwrite = TRUE
  )
  if (!all(copied)) stop("One or more development datasets could not be staged.")

  write_refresh_metadata(
    "development",
    setNames(paste0("local dummy file: ", source_files), names(source_files)),
    runtime_files[["refresh_metadata"]],
    as.Date(config$study$data_cutoff)
  )
}

collapse_kobo_value <- function(value) {
  if (is.null(value) || length(value) == 0) return(NA_character_)
  if (is.atomic(value)) return(paste(value, collapse = " "))
  toJSON(value, auto_unbox = TRUE, null = "null")
}

normalise_kobo_records <- function(records) {
  if (length(records) == 0) return(tibble())
  data <- fromJSON(
    toJSON(records, auto_unbox = TRUE, null = "null"),
    flatten = TRUE,
    simplifyDataFrame = TRUE
  ) |>
    as_tibble()

  list_columns <- names(data)[vapply(data, is.list, logical(1))]
  for (column in list_columns) {
    data[[column]] <- vapply(data[[column]], collapse_kobo_value, character(1))
  }

  # Kobo group fields are commonly returned as group_name/question_name.
  # Use the leaf question name when it is unique; retain the full path when two
  # groups contain the same leaf name so ambiguity is never silently introduced.
  original_names <- names(data)
  leaf_names <- str_replace(original_names, "^.*/", "")
  unique_leaf <- !duplicated(leaf_names) & !duplicated(leaf_names, fromLast = TRUE)
  names(data)[unique_leaf] <- leaf_names[unique_leaf]
  data
}

fetch_kobo_asset <- function(asset_uid, dataset_name, token, production) {
  base_url <- str_remove(production$base_url, "/+$")
  next_url <- paste0(base_url, "/api/v2/assets/", asset_uid, "/data/")
  first_page <- TRUE
  records <- list()

  repeat {
    request <- request(next_url) |>
      req_headers(Authorization = paste("Token", token)) |>
      req_timeout(production$request_timeout_seconds) |>
      req_retry(max_tries = production$request_max_tries, retry_on_failure = TRUE)
    if (first_page) {
      request <- request |>
        req_url_query(limit = production$page_size)
    }

    response <- req_perform(request)
    page <- resp_body_json(response, simplifyVector = FALSE)
    if (is.null(page$results)) {
      stop("Kobo returned an unexpected response for ", dataset_name,
           "; the paginated 'results' field was absent.")
    }
    records <- c(records, page$results)
    first_page <- FALSE
    if (is.null(page$`next`) || identical(page$`next`, "")) break
    next_url <- page$`next`
    if (str_starts(next_url, "/")) {
      next_url <- paste0(base_url, next_url)
    }
  }

  if (length(records) == 0 && isTRUE(production$fail_if_a_form_has_no_submissions)) {
    stop("Kobo asset ", dataset_name, " returned no submissions.")
  }

  if (isTRUE(production$save_timestamped_raw_snapshots)) {
    snapshot_directory <- production$snapshot_directory
    dir.create(snapshot_directory, recursive = TRUE, showWarnings = FALSE)
    timestamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
    snapshot_path <- file.path(
      snapshot_directory,
      paste0(dataset_name, "_", asset_uid, "_", timestamp, ".json")
    )
    writeLines(
      toJSON(records, auto_unbox = TRUE, pretty = TRUE, null = "null"),
      snapshot_path,
      useBytes = TRUE
    )
  }

  normalise_kobo_records(records)
}

require_fields <- function(data, fields, dataset_name) {
  missing <- setdiff(fields, names(data))
  if (length(missing) > 0) {
    stop(dataset_name, " is missing required fields: ", paste(missing, collapse = ", "),
         ". Check the Kobo asset UID and deployed form version.")
  }
}

add_optional_fields <- function(data, fields) {
  for (field in setdiff(fields, names(data))) data[[field]] <- NA
  data
}

derive_epro_files <- function(enrolment, cimc) {
  require_fields(
    enrolment,
    c("uniqueid", "contraception_start_date"),
    "Enrolment submissions"
  )
  require_fields(
    cimc,
    c("uniqueid", "date", "Q101", "Q105", "monthly_report_due", "Q201", "Q211", "Q214"),
    "CIMC submissions"
  )

  daily_optional <- c(
    "_submission_time", "Q102", "Q103", "Q104", "Q106", "Q107", "Q108",
    "Q109", "Q110"
  )
  monthly_optional <- c(
    "Q202", "Q203", "Q204", "Q205", "Q206", "bleeding_product_material_other",
    "Q207", "Q208", "Q210", "Q212", "Q213", "Q215", "Q216", "Q217",
    "Q218", "Q219", "Q220", "Q221", "QA", "QB"
  )
  cimc <- add_optional_fields(cimc, c(daily_optional, monthly_optional))

  programme_dates <- enrolment |>
    transmute(
      uniqueid = as.character(uniqueid),
      programme_start_date = as.Date(contraception_start_date)
    ) |>
    distinct(uniqueid, .keep_all = TRUE)

  cimc <- cimc |>
    mutate(uniqueid = as.character(uniqueid), date = as.Date(date)) |>
    left_join(programme_dates, by = "uniqueid")
  if (any(is.na(cimc$programme_start_date))) {
    stop("Some CIMC submissions could not be linked to an enrolment programme start date.")
  }

  daily <- cimc |>
    transmute(
      participant_id = uniqueid,
      programme_start_date,
      report_date = date,
      submitted_at = `_submission_time`,
      menstrual_bleeding_yesterday = Q101,
      bleeding_expected_yesterday = Q102,
      bleeding_amount_yesterday = Q103,
      bleeding_amount_expected_yesterday = Q104,
      menstrual_pain_yesterday = Q105,
      pain_expected_yesterday = Q106,
      pain_amount_yesterday = Q107,
      pain_amount_expected_yesterday = Q108,
      pain_duration_yesterday = Q109,
      pain_locations_yesterday = Q110
    )

  monthly <- cimc |>
    filter(monthly_report_due == "yes") |>
    transmute(
      participant_id = uniqueid,
      programme_start_date,
      reference_month = format(date - 1, "%Y-%m"),
      menstrual_bleeding_last_month = Q201,
      clots_last_month = Q211,
      menstrual_pain_last_month = Q214,
      bleeding_product_material = Q206,
      bleeding_product_material_other,
      bleeding_days_change = Q202,
      bleeding_spacing_change = Q210,
      bleeding_amount_change = Q203,
      bleeding_start_predictability = Q207,
      bleeding_stop_predictability = Q208,
      bleeding_color_change = Q204,
      bleeding_smell_change = Q205,
      clot_number_change = Q212,
      clot_size_change = Q213,
      pain_severity_change = Q215,
      pain_days_change = Q216,
      pain_location_change = Q217,
      pain_management_impact = Q218,
      bleeding_management_impact = Q219,
      pain_daily_activities_impact = Q220,
      bleeding_daily_activities_impact = Q221,
      still_using_method = QA,
      method_stop_date = QB
    )

  list(daily_data = daily, monthly_data = monthly)
}

stage_production_data <- function() {
  production <- config$data_source$production
  token_variable <- production$api_token_environment_variable
  token <- Sys.getenv(token_variable, unset = "")
  if (identical(token, "")) {
    stop("Production mode requires the Kobo API token in environment variable ",
         token_variable, ". The token must not be stored in dashboard_config.yml.")
  }

  asset_uids <- unlist(production$asset_uids)
  placeholders <- names(asset_uids)[str_detect(asset_uids, "^REPLACE_WITH_") | asset_uids == ""]
  if (length(placeholders) > 0) {
    stop("Replace the production Kobo asset UID placeholders for: ",
         paste(placeholders, collapse = ", "))
  }

  downloaded <- lapply(names(asset_uids), function(name) {
    fetch_kobo_asset(asset_uids[[name]], name, token, production)
  })
  names(downloaded) <- names(asset_uids)

  require_fields(
    downloaded$recruitment,
    c("organization", "recruitment_date", "eligibility_status", "uniqueid"),
    "Recruitment submissions"
  )
  require_fields(
    downloaded$enrolment,
    c("organization", "uniqueid", "enrollment_date", "contraception_start_date"),
    "Enrolment submissions"
  )

  derived <- derive_epro_files(downloaded$enrolment, downloaded$cimc)
  prepared <- c(downloaded, derived)
  for (name in names(prepared)) {
    write_csv(prepared[[name]], runtime_files[[name]], na = "")
  }

  source_labels <- c(
    recruitment = paste0(production$base_url, "/api/v2/assets/", asset_uids[["recruitment"]], "/data/"),
    enrolment = paste0(production$base_url, "/api/v2/assets/", asset_uids[["enrolment"]], "/data/"),
    cimc = paste0(production$base_url, "/api/v2/assets/", asset_uids[["cimc"]], "/data/"),
    daily_data = "derived from current CIMC and enrolment submissions",
    monthly_data = "derived from current CIMC and enrolment submissions"
  )
  cutoff_strategy <- production$data_cutoff_strategy
  effective_data_cutoff <- if (identical(cutoff_strategy, "retrieval_date")) {
    as.Date(format(Sys.time(), tz = config$study$time_zone))
  } else if (identical(cutoff_strategy, "latest_cimc_submission")) {
    max(as.Date(downloaded$cimc$date), na.rm = TRUE)
  } else if (identical(cutoff_strategy, "configured")) {
    as.Date(config$study$data_cutoff)
  } else {
    stop(paste(
      "production.data_cutoff_strategy must be 'retrieval_date',",
      "'latest_cimc_submission', or 'configured'."
    ))
  }
  write_refresh_metadata(
    "production",
    source_labels,
    runtime_files[["refresh_metadata"]],
    effective_data_cutoff
  )
}

if (mode == "development") {
  stage_development_data()
} else {
  stage_production_data()
}

message("Prepared dashboard data in ", mode, " mode.")
