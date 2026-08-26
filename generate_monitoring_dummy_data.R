library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)
library(xml2)
library(yaml)

config <- read_yaml("dashboard_config.yml")
set.seed(config$dummy_data$random_seed)

data_cutoff <- as.Date(config$study$data_cutoff)
study_days <- as.integer(config$study$expected_daily_reports)
n_screened <- as.integer(config$dummy_data$screened_participants)
monthly_recall_disagreement_probability <-
  config$dummy_data$monthly_recall_disagreement_probability
eligibility_window_days <- as.integer(config$study$contraceptive_start_eligibility_days)
monthly_due_day <- as.integer(config$study$monthly_due_day)

sample_date <- function(start, end, n, weights = NULL) {
  dates <- seq(as.Date(start), as.Date(end), by = "day")
  sample(dates, n, replace = TRUE, prob = weights)
}

add_missing_columns <- function(data, fields) {
  for (field in setdiff(fields, names(data))) data[[field]] <- NA
  data |> select(any_of(c("_id", "_uuid", "_submission_time", fields)))
}

recruitment_fields <- c(
  "organization", "organization_label", "organization_code", "payment",
  "recruiter_initials", "recruitment_date", "recruitment_place",
  "prior_participation", "ohsu_intro", "whru_newsletter", "email_newsletter",
  "full_name_newsletter", "screening_consent", "age_18_49",
  "currently_pregnant", "recently_pregnant", "pregnant_last_6_months",
  "pregnant_last_6_weeks", "currently_breastfeeding", "periods_returned",
  "current_contraception", "date_74_days_ago", "started_last_74_days",
  "injections", "ocp", "implant", "ring", "method_eligible",
  "iud_implant", "same_method_before_insertion",
  "non_contraceptive_hormones", "personal_smartphone",
  "personal_smartphone_90_days", "language_ability", "eligibility_status",
  "uniqueid", "comments_ohsu", "first_name", "last_name", "phone",
  "message_permission", "email", "email_security", "days",
  "enrollment_date", "enrollment_times", "reminder",
  "receiving_newsletter", "notes", "submitMessage"
)

enrolment_fields <- c(
  "organization", "uniqueid", "enrollment_date", "consent_completed",
  "consent_date", "age", "age_confirmation", "education_level",
  "partner_status_a", "partner_status_a_other", "partner_status_b",
  "partner_status_c", "currently_using_contraception",
  "contraception_start_date", "days_since_contraception_start",
  "date_74_days_ago", "start_date_confirmation", "contraceptive_method",
  "method_label", "method_confirmation", "eligibility_status",
  "daily_reminder_method", "no_reminder_confirm", "phone", "email",
  "daily_reminder_time", "missed_days_reminder_method", "submitMessage"
)

cimc_fields <- c(
  "uniqueid", "method", "total_submissions", "date", "id_date",
  "already_submitted", "monthly_report_due", "date_label", "date_label2",
  "last_month", "last_month_label", "id_first_submission", "Q01_enrollment",
  "Q02_enrollment", "Q03_enrollment", "Q01", "CHK", "Q02", "Q03",
  "Q101", "Q102", "Q103", "Q104", "Q105", "Q106", "Q107", "Q108",
  "Q109", "Q110", "Q110_nr", "QA", "QB", "QC", "QD", "QD_other",
  "QE", "Q201", "Q202", "Q203", "Q204", "Q205", "Q206",
  "bleeding_product_material_other", "Q207", "Q208", "Q209", "Q210",
  "Q211", "Q212", "Q213", "Q214", "Q215", "Q216", "Q217", "Q218",
  "Q219", "Q220", "Q221", "Q222", "Q222_other", "label_date_today",
  "date_previous", "id_previous", "previous_day", "label_date_previous",
  "date_previous2", "id_previous2", "previous_day2", "label_date_previous2",
  "date_previous3", "id_previous3", "previous_day3", "label_date_previous3",
  "date_previous4", "id_previous4", "previous_day4", "label_date_previous4",
  "date_previous5", "id_previous5", "previous_day5", "label_date_previous5",
  "date_previous6", "id_previous6", "previous_day6", "label_date_previous6",
  "last_missed_day", "last_missed_day_label", "seven_days"
)

organizations <- c("wmru", "profamilia", "ohsu")
organization_labels <- c(
  wmru = "Wits MRU", profamilia = "Profamilia", ohsu = "OHSU"
)
organization_codes <- c(wmru = "01", profamilia = "02", ohsu = "03")

recruitment_dates <- sample_date(
  "2026-05-15", "2026-08-20", n_screened,
  weights = seq(0.65, 1.35, length.out = 98)
)

screening_outcome <- sample(
  c(
    "eligible", "prior_participation", "declined_screening", "age",
    "pregnancy", "contraception", "method_timing", "method_ineligible",
    "same_method", "other_hormones", "smartphone", "language"
  ),
  n_screened,
  replace = TRUE,
  prob = c(.68, .025, .025, .035, .035, .035, .045, .025, .02, .015, .035, .025)
)

recruitment <- tibble(
  `_id` = seq_len(n_screened),
  `_uuid` = sprintf("uuid-recruit-%04d", seq_len(n_screened)),
  organization = sample(organizations, n_screened, TRUE, c(.35, .34, .31)),
  recruitment_date = recruitment_dates,
  screening_outcome = screening_outcome
) |>
  mutate(
    organization_label = unname(organization_labels[organization]),
    organization_code = unname(organization_codes[organization]),
    payment = case_when(
      organization == "wmru" ~ "R1200",
      organization == "profamilia" ~ "7,700 RD pesos",
      TRUE ~ "$250"
    ),
    recruiter_initials = if_else(
      organization == "ohsu", NA_character_,
      sample(c("AM", "BN", "CK", "DS", "EP"), n(), TRUE)
    ),
    recruitment_place = if_else(
      organization == "ohsu", NA_character_,
      sample(c("clinic", "community"), n(), TRUE, c(.76, .24))
    ),
    prior_participation = if_else(screening_outcome == "prior_participation", "yes", "no"),
    ohsu_intro = if_else(organization == "ohsu", "yes", NA_character_),
    screening_consent = if_else(screening_outcome == "declined_screening", "no", "yes"),
    age_18_49 = if_else(screening_outcome == "age", "no", "yes"),
    currently_pregnant = if_else(screening_outcome == "pregnancy", "yes", "no"),
    recently_pregnant = if_else(
      currently_pregnant == "no", sample(c("yes", "no"), n(), TRUE, c(.12, .88)), NA_character_
    ),
    pregnant_last_6_months = if_else(
      recently_pregnant == "yes", sample(c("yes", "no"), n(), TRUE, c(.58, .42)), NA_character_
    ),
    pregnant_last_6_weeks = if_else(
      pregnant_last_6_months == "yes", sample(c("yes", "no"), n(), TRUE, c(.22, .78)), NA_character_
    ),
    currently_breastfeeding = if_else(
      pregnant_last_6_months == "yes" & pregnant_last_6_weeks == "no",
      sample(c("yes", "no"), n(), TRUE, c(.28, .72)), NA_character_
    ),
    periods_returned = if_else(
      currently_breastfeeding == "yes", sample(c("yes", "no"), n(), TRUE, c(.82, .18)), NA_character_
    ),
    current_contraception = if_else(screening_outcome == "contraception", "no", "yes"),
    date_74_days_ago = recruitment_date - eligibility_window_days,
    started_last_74_days = if_else(screening_outcome == "method_timing", "no", "yes"),
    injections = "Contraceptive injections",
    ocp = "Oral contraceptive pills",
    implant = "Contraceptive implant(s) in your arm",
    ring = "Hormonal rings",
    method_eligible = if_else(screening_outcome == "method_ineligible", "no", "yes"),
    iud_implant = sample(c("yes", "no"), n(), TRUE, c(.45, .55)),
    same_method_before_insertion = if_else(
      iud_implant == "yes",
      if_else(screening_outcome == "same_method", "yes", "no"),
      NA_character_
    ),
    non_contraceptive_hormones = if_else(screening_outcome == "other_hormones", "yes", "no"),
    personal_smartphone = if_else(screening_outcome == "smartphone", "no", "yes"),
    personal_smartphone_90_days = if_else(personal_smartphone == "yes", "yes", NA_character_),
    language_ability = if_else(
      personal_smartphone_90_days == "yes",
      if_else(screening_outcome == "language", "no", "yes"),
      NA_character_
    ),
    eligibility_status = if_else(screening_outcome == "eligible", "eligible", "not_eligible"),
    uniqueid = if_else(
      eligibility_status == "eligible",
      sprintf("%s-CIMC-%04d", organization_code, row_number()),
      NA_character_
    ),
    comments_ohsu = if_else(organization == "ohsu", "Dummy screening record", NA_character_),
    first_name = if_else(organization == "ohsu" & eligibility_status == "eligible", paste0("Test", `_id`), NA_character_),
    last_name = if_else(organization == "ohsu" & eligibility_status == "eligible", "Participant", NA_character_),
    phone = if_else(
      organization == "ohsu" & eligibility_status == "eligible",
      sprintf("+1503555%04d", `_id`), NA_character_
    ),
    message_permission = if_else(
      organization == "ohsu" & eligibility_status == "eligible",
      sample(c("detailed_message_ok", "name_number_ok", "no_messages"), n(), TRUE, c(.55, .35, .10)),
      NA_character_
    ),
    email = if_else(
      organization == "ohsu" & eligibility_status == "eligible",
      sprintf("test.participant.%03d@example.org", `_id`), NA_character_
    ),
    email_security = if_else(
      organization == "ohsu" & eligibility_status == "eligible",
      sample(c("non_secure_ok", "prefers_secure"), n(), TRUE, c(.7, .3)),
      NA_character_
    ),
    `_submission_time` = paste0(recruitment_date, "T10:00:00Z")
  )

eligible_indices <- which(recruitment$eligibility_status == "eligible")
planned_enrolment <- recruitment$recruitment_date[eligible_indices] +
  sample(2:14, length(eligible_indices), TRUE)
will_enrol <- runif(length(eligible_indices)) < .82 & planned_enrolment <= data_cutoff
enrolled_indices <- eligible_indices[will_enrol]
actual_enrolment_dates <- planned_enrolment[will_enrol]

recruitment$enrollment_date <- as.Date(NA)
recruitment$enrollment_date[eligible_indices] <- planned_enrolment
recruitment$days <- if_else(
  recruitment$organization == "ohsu" & recruitment$eligibility_status == "eligible",
  sample(c("monday wednesday", "tuesday thursday", "friday"), nrow(recruitment), TRUE),
  NA_character_
)
recruitment$enrollment_times <- if_else(
  recruitment$organization == "ohsu" & recruitment$eligibility_status == "eligible",
  sample(c("09:00-11:00", "13:00-15:00", "16:00-18:00"), nrow(recruitment), TRUE),
  NA_character_
)
recruitment$reminder <- if_else(
  recruitment$organization == "ohsu" & recruitment$eligibility_status == "eligible",
  sample(c("email_reminder", "phone_reminder"), nrow(recruitment), TRUE),
  NA_character_
)
recruitment$receiving_newsletter <- if_else(
  recruitment$organization == "ohsu" & recruitment$eligibility_status == "eligible",
  sample(c("yes", "no"), nrow(recruitment), TRUE, c(.28, .72)),
  NA_character_
)
recruitment$notes <- if_else(
  recruitment$organization == "ohsu" & recruitment$eligibility_status == "eligible",
  "Dummy scheduling note", NA_character_
)
recruitment$submitMessage <- if_else(
  recruitment$eligibility_status == "eligible",
  "Eligible; continue to enrollment form", "Screening complete"
)

recruitment <- add_missing_columns(recruitment, recruitment_fields)

education_by_site <- list(
  wmru = c("primary", "secondary", "tertiary_university", "post_matric_qualification", "no_response"),
  profamilia = c("primary", "secondary", "tertiary", "university", "no_response"),
  ohsu = c("less_than_high_school_ged", "high_school_graduate_ged", "some_college_associates_degree", "bachelors_degree", "advanced_degree", "no_response")
)

methods_by_site <- list(
  wmru = c("injectable_dmpa", "injectable_net_en", "pills", "implants", "hormonal_iud", "nonhormonal_iud", "hormonal_rings", "hormonal_patches"),
  profamilia = c("injectables", "pills", "implants", "hormonal_iud", "nonhormonal_iud", "hormonal_rings", "hormonal_patches"),
  ohsu = c("injectables", "pills", "implants", "hormonal_iud", "nonhormonal_iud", "hormonal_rings", "hormonal_patches")
)

method_label_lookup <- c(
  injectable_dmpa = "the injectable", injectable_net_en = "the injectable",
  injectables = "the injectable", pills = "the pill", implants = "the implant",
  hormonal_iud = "the IUD", nonhormonal_iud = "the IUD",
  hormonal_rings = "the ring", hormonal_patches = "the patch"
)

enrolment <- recruitment[enrolled_indices, ] |>
  transmute(
    `_id` = seq_along(enrolled_indices),
    `_uuid` = sprintf("uuid-enrol-%04d", seq_along(enrolled_indices)),
    `_submission_time` = paste0(actual_enrolment_dates, "T14:00:00Z"),
    organization,
    uniqueid,
    enrollment_date = actual_enrolment_dates,
    consent_completed = "yes",
    consent_date = actual_enrolment_dates,
    age = sample(18:49, n(), TRUE, prob = dnorm(18:49, 29, 7)),
    age_confirmation = NA_character_
  ) |>
  rowwise() |>
  mutate(
    education_level = sample(education_by_site[[organization]], 1, prob = c(rep(1, length(education_by_site[[organization]]) - 1), .08)),
    contraceptive_method = sample(methods_by_site[[organization]], 1)
  ) |>
  ungroup() |>
  mutate(
    partner_status_a = if_else(
      organization == "wmru",
      sample(c("regular_partner_not_living_together", "regular_partner_living_together", "spouse_living_together", "casual_partners", "no_partner", "no_response"), n(), TRUE, c(.18, .20, .28, .08, .20, .06)),
      NA_character_
    ),
    partner_status_a_other = NA_character_,
    partner_status_b = if_else(
      organization == "profamilia",
      sample(c("never_in_union", "currently_married", "living_with_man", "divorced_separated_widowed", "no_response"), n(), TRUE, c(.25, .32, .25, .12, .06)),
      NA_character_
    ),
    partner_status_c = if_else(
      organization == "ohsu",
      sample(c("unpartnered_not_sexually_active", "unpartnered_sexually_active", "partnered_not_living_together", "partnered_living_together", "no_response"), n(), TRUE, c(.16, .18, .22, .38, .06)),
      NA_character_
    ),
    currently_using_contraception = "yes",
    days_since_contraception_start = sample(
      0:eligibility_window_days,
      n(),
      TRUE,
      prob = rev(seq(1, 2, length.out = eligibility_window_days + 1L))
    ),
    contraception_start_date = enrollment_date - days_since_contraception_start,
    date_74_days_ago = enrollment_date - eligibility_window_days,
    start_date_confirmation = NA_character_,
    method_label = unname(method_label_lookup[contraceptive_method]),
    method_confirmation = NA_character_,
    eligibility_status = "eligible",
    daily_reminder_method = case_when(
      organization == "ohsu" ~ sample(c("calendar", "whatsapp", "text_message_sms", "email", "no_reminder", "calendar email"), n(), TRUE, c(.20, .18, .24, .16, .08, .14)),
      TRUE ~ sample(c("calendar", "whatsapp", "no_reminder"), n(), TRUE, c(.48, .42, .10))
    ),
    no_reminder_confirm = if_else(str_detect(daily_reminder_method, "no_reminder"), "yes", NA_character_),
    phone = if_else(
      str_detect(daily_reminder_method, "whatsapp|text_message_sms"),
      sprintf("+1999555%04d", row_number()), NA_character_
    ),
    email = if_else(
      str_detect(daily_reminder_method, "email"),
      sprintf("cimc.%04d@example.org", row_number()), NA_character_
    ),
    daily_reminder_time = sprintf("%02d:%02d:00", sample(6:10, n(), TRUE), sample(c(0, 15, 30, 45), n(), TRUE)),
    missed_days_reminder_method = if_else(
      organization == "ohsu",
      sample(c("whatsapp", "text_message_sms", "email"), n(), TRUE, c(.35, .40, .25)),
      NA_character_
    ),
    submitMessage = "Eligible; participant survey links created"
  )

enrolment <- add_missing_columns(enrolment, enrolment_fields)

region_nodes <- xml_find_all(
  read_xml("bodymap_prototype.svg"),
  "//*[@id and (self::*[local-name()='path' or local-name()='polygon' or local-name()='rect' or local-name()='circle' or local-name()='ellipse'] or ./*[local-name()='path' or local-name()='polygon' or local-name()='rect' or local-name()='circle' or local-name()='ellipse'])]"
)
body_regions <- unique(xml_attr(region_nodes, "id"))
body_regions <- body_regions[!is.na(body_regions) & body_regions != ""]

region_weights <- rep(.4, length(body_regions))
region_weights[str_detect(body_regions, "Foot|Hand|Arm_|Leg_Lower")] <- .08
region_weights[str_detect(body_regions, "Leg_Upper|Buttock")] <- .22
region_weights[str_detect(body_regions, "Pate|Temporal")] <- 2.2
region_weights[str_detect(body_regions, "Breast|Nipple")] <- 3.1
region_weights[str_detect(body_regions, "Flank|Hip")] <- 4.2
region_weights[str_detect(body_regions, "Lumbar|Sacral|Rectal")] <- 7.5
region_weights[str_detect(body_regions, "Abdomen_Upper|Abdomen_Side")] <- 6.5
region_weights[str_detect(body_regions, "Abdomen_Lower|Pubic|Vaginovulval")] <- 11
region_weights <- region_weights / sum(region_weights)
names(region_weights) <- body_regions

participant_traits <- enrolment |>
  transmute(
    uniqueid, organization, enrollment_date, contraception_start_date,
    method = method_label,
    baseline_cycle_length = sample(24:36, n(), TRUE),
    bleeding_length = sample(2:7, n(), TRUE),
    phase_offset = sample(0:27, n(), TRUE),
    pain_tendency = runif(n(), .03, .22),
    reporting_probability = pmin(.98, pmax(.35, rbeta(n(), 8, 2))),
    usual_bleeding = rbinom(n(), 1, .90),
    usual_clots = rbinom(n(), 1, .33),
    usual_pain = rbinom(n(), 1, .72),
    planned_end_date = enrollment_date + study_days - 1L
  ) |>
  mutate(
    stop_method = rbinom(n(), 1, .09),
    stop_date = as.Date(if_else(
      stop_method == 1,
      as.numeric(enrollment_date + sample(25:78, n(), TRUE)),
      NA_real_
    ), origin = "1970-01-01"),
    expected_end_date = pmin(planned_end_date, data_cutoff, coalesce(stop_date, data_cutoff)),
    core_regions = map(uniqueid, ~ sample(body_regions, 4, FALSE, region_weights))
  )

due_days <- participant_traits |>
  select(-core_regions) |>
  pmap_dfr(function(uniqueid, organization, enrollment_date, contraception_start_date,
                    method, baseline_cycle_length, bleeding_length, phase_offset,
                    pain_tendency, reporting_probability, usual_bleeding,
                    usual_clots, usual_pain, planned_end_date, stop_method,
                    stop_date, expected_end_date) {
    tibble(
      uniqueid = uniqueid,
      date = seq(enrollment_date, expected_end_date, by = "day"),
      organization = organization,
      enrollment_date = enrollment_date,
      contraception_start_date = contraception_start_date,
      method = method,
      baseline_cycle_length = baseline_cycle_length,
      bleeding_length = bleeding_length,
      phase_offset = phase_offset,
      pain_tendency = pain_tendency,
      reporting_probability = reporting_probability,
      usual_bleeding = usual_bleeding,
      usual_clots = usual_clots,
      usual_pain = usual_pain,
      planned_end_date = planned_end_date,
      stop_date = stop_date
    )
  })

active_ids <- participant_traits |>
  filter(planned_end_date >= data_cutoff, is.na(stop_date) | stop_date > data_cutoff) |>
  pull(uniqueid)
forced_gap_ids <- sample(active_ids, min(20, length(active_ids)))
zero_submission_ids <- sample(setdiff(active_ids, forced_gap_ids), min(4, length(setdiff(active_ids, forced_gap_ids))))

due_days <- due_days |>
  mutate(
    monthly_due = as.integer(format(date, "%d")) == monthly_due_day,
    submitted = runif(n()) < reporting_probability,
    submitted = if_else(monthly_due & runif(n()) < .92, TRUE, submitted),
    submitted = if_else(
      uniqueid %in% forced_gap_ids & date >= data_cutoff - 2 & date <= data_cutoff,
      FALSE, submitted
    ),
    submitted = if_else(uniqueid %in% zero_submission_ids, FALSE, submitted)
  )

reports <- due_days |>
  filter(submitted) |>
  arrange(uniqueid, date) |>
  group_by(uniqueid) |>
  mutate(
    total_submissions = row_number() - 1L,
    first_submission = row_number() == 1L
  ) |>
  ungroup() |>
  mutate(
    cycle_day = (as.integer(date - contraception_start_date) + phase_offset) %% baseline_cycle_length + 1L,
    scheduled_bleeding = usual_bleeding == 1 & cycle_day <= bleeding_length,
    Q101 = rbinom(n(), 1, case_when(
      scheduled_bleeding ~ .96,
      cycle_day %in% 11:17 ~ .035,
      TRUE ~ .012
    )),
    Q102 = case_when(
      Q101 == 1 & scheduled_bleeding ~ sample(1:3, n(), TRUE, c(.08, .84, .08)),
      Q101 == 1 ~ sample(1:3, n(), TRUE, c(.70, .12, .18)),
      scheduled_bleeding ~ sample(1:3, n(), TRUE, c(.18, .68, .14)),
      TRUE ~ sample(1:3, n(), TRUE, c(.82, .07, .11))
    ),
    Q103 = if_else(
      Q101 == 1,
      sample(1:4, n(), TRUE, c(.20, .38, .30, .12)),
      NA_integer_
    ),
    Q104 = if_else(
      Q101 == 1 & Q102 == 2,
      sample(1:4, n(), TRUE, c(.62, .16, .16, .06)),
      NA_integer_
    ),
    pain_probability = pmin(.88, pain_tendency + .42 * Q101 + .08 * (cycle_day %in% c(1, 2))),
    Q105 = rbinom(n(), 1, pain_probability),
    Q106 = case_when(
      Q105 == 1 & Q101 == 1 ~ sample(1:3, n(), TRUE, c(.18, .70, .12)),
      Q105 == 1 ~ sample(1:3, n(), TRUE, c(.55, .25, .20)),
      TRUE ~ sample(1:3, n(), TRUE, c(.83, .07, .10))
    ),
    Q107 = if_else(Q105 == 1, sample(1:4, n(), TRUE, c(.42, .36, .16, .06)), NA_integer_),
    Q108 = if_else(Q105 == 1 & Q106 == 2, sample(1:4, n(), TRUE, c(.61, .17, .15, .07)), NA_integer_),
    Q109 = if_else(Q105 == 1, sample(1:4, n(), TRUE, c(.36, .34, .21, .09)), NA_integer_)
  )

core_region_lookup <- setNames(participant_traits$core_regions, participant_traits$uniqueid)
reports$Q110 <- NA_character_
pain_rows <- which(reports$Q105 == 1)
for (row in pain_rows) {
  uid <- reports$uniqueid[[row]]
  severity <- reports$Q107[[row]]
  n_regions <- sample(seq_len(min(4L, severity + 1L)), 1)
  pool_weights <- region_weights
  pool_weights[names(pool_weights) %in% core_region_lookup[[uid]]] <-
    pool_weights[names(pool_weights) %in% core_region_lookup[[uid]]] * 5
  if (reports$Q101[[row]] == 1) {
    pelvic <- str_detect(names(pool_weights), "Abdomen|Pubic|Vaginovulval|Rectal|Sacral|Lumbar|Flank|Hip")
    pool_weights[pelvic] <- pool_weights[pelvic] * 1.8
  }
  reports$Q110[[row]] <- paste(
    sample(names(pool_weights), n_regions, FALSE, pool_weights),
    collapse = " "
  )
}

reports <- reports |>
  mutate(
    Q110_nr = if_else(Q105 == 1 & (is.na(Q110) | Q110 == ""), "1", NA_character_),
    Q01 = if_else(first_submission, usual_bleeding, NA_integer_),
    CHK = if_else(first_submission & usual_bleeding == 0, "2", NA_character_),
    Q02 = if_else(first_submission, usual_clots, NA_integer_),
    Q03 = if_else(first_submission, usual_pain, NA_integer_),
    monthly_report_due = if_else(monthly_due, "yes", "no"),
    id_date = paste0(uniqueid, date),
    already_submitted = 0L,
    id_first_submission = if_else(first_submission, uniqueid, ""),
    Q01_enrollment = usual_bleeding,
    Q02_enrollment = usual_clots,
    Q03_enrollment = usual_pain,
    last_month = if_else(monthly_due, format(date - 1, "%m"), NA_character_),
    last_month_label = if_else(monthly_due, format(date - 1, "%B"), NA_character_),
    date_label = format(date, "%a, %b %e"),
    date_label2 = paste("on", format(date, "%a, %b %e")),
    QA = if_else(monthly_due, if_else(is.na(stop_date) | stop_date > date, 1L, 0L), NA_integer_),
    QB = as.Date(if_else(monthly_due & QA == 0, as.numeric(stop_date), NA_real_), origin = "1970-01-01"),
    QC = if_else(monthly_due & QA == 0, rbinom(n(), 1, .35), NA_integer_),
    QD = if_else(monthly_due & QA == 0 & QC == 1, sample(c("1", "3", "4", "5", "other"), n(), TRUE), NA_character_),
    QD_other = if_else(QD == "other", "Other dummy method", NA_character_),
    QE = if_else(monthly_due & QA == 0, "Changed or discontinued method", NA_character_)
  )

monthly_history <- reports |>
  mutate(reference_month = format(date, "%Y-%m")) |>
  group_by(uniqueid, reference_month) |>
  summarise(
    any_bleeding = as.integer(any(Q101 == 1)),
    any_pain = as.integer(any(Q105 == 1)),
    .groups = "drop"
  )

reports <- reports |>
  mutate(previous_month = format(date - 1, "%Y-%m")) |>
  left_join(
    monthly_history,
    by = c("uniqueid", "previous_month" = "reference_month")
  ) |>
  mutate(
    Q201 = if_else(
      monthly_due,
      if_else(
        runif(n()) < monthly_recall_disagreement_probability,
        1L - coalesce(any_bleeding, 0L),
        coalesce(any_bleeding, 0L)
      ),
      NA_integer_
    ),
    Q202 = if_else(monthly_due & Q201 == 1 & Q01_enrollment == 1, sample(1:3, n(), TRUE, c(.25, .48, .27)), NA_integer_),
    Q203 = if_else(monthly_due & Q201 == 1 & Q01_enrollment == 1, sample(1:3, n(), TRUE, c(.30, .44, .26)), NA_integer_),
    Q204 = if_else(monthly_due & Q201 == 1 & Q01_enrollment == 1, sample(1:4, n(), TRUE, c(.52, .25, .10, .13)), NA_integer_),
    Q205 = if_else(monthly_due & Q201 == 1 & Q01_enrollment == 1, sample(1:4, n(), TRUE, c(.58, .20, .08, .14)), NA_integer_),
    Q207 = if_else(monthly_due & Q201 == 1, sample(1:3, n(), TRUE, c(.27, .50, .23)), NA_integer_),
    Q208 = if_else(monthly_due & Q201 == 1, sample(1:3, n(), TRUE, c(.23, .53, .24)), NA_integer_),
    Q209 = if_else(monthly_due & Q201 == 1, as.character(sample(1:5, n(), TRUE, c(.03, .44, .27, .16, .10))), NA_character_),
    Q210 = if_else(monthly_due & Q201 == 1, sample(1:3, n(), TRUE, c(.22, .53, .25)), NA_integer_),
    Q211 = if_else(monthly_due & Q201 == 1, rbinom(n(), 1, .28), NA_integer_),
    Q212 = if_else(monthly_due & Q211 == 1 & Q02_enrollment == 1, sample(1:4, n(), TRUE, c(.25, .43, .22, .10)), NA_integer_),
    Q213 = if_else(monthly_due & Q211 == 1 & Q02_enrollment == 1, sample(1:4, n(), TRUE, c(.24, .47, .19, .10)), NA_integer_),
    Q214 = if_else(
      monthly_due,
      if_else(
        runif(n()) < monthly_recall_disagreement_probability,
        1L - coalesce(any_pain, 0L),
        coalesce(any_pain, 0L)
      ),
      NA_integer_
    ),
    Q215 = if_else(monthly_due & Q214 == 1 & Q03_enrollment == 1, sample(1:3, n(), TRUE, c(.28, .47, .25)), NA_integer_),
    Q216 = if_else(monthly_due & Q214 == 1 & Q03_enrollment == 1, sample(1:3, n(), TRUE, c(.26, .46, .28)), NA_integer_),
    Q217 = if_else(monthly_due & Q214 == 1 & Q03_enrollment == 1, as.character(sample(1:4, n(), TRUE, c(.37, .31, .22, .10))), NA_character_),
    Q218 = if_else(monthly_due, sample(1:3, n(), TRUE, c(.25, .51, .24)), NA_integer_),
    Q219 = if_else(monthly_due, sample(1:3, n(), TRUE, c(.24, .53, .23)), NA_integer_),
    Q220 = if_else(monthly_due, sample(1:3, n(), TRUE, c(.22, .56, .22)), NA_integer_),
    Q221 = if_else(monthly_due, sample(1:3, n(), TRUE, c(.21, .57, .22)), NA_integer_),
    Q222 = if_else(monthly_due, rbinom(n(), 1, .12), NA_integer_),
    Q222_other = if_else(monthly_due & Q222 == 1, "Other dummy menstrual-cycle change", NA_character_)
  )

reports$Q206 <- NA_character_
product_rows <- which(reports$monthly_due & reports$Q201 == 1)
for (row in product_rows) {
  selected <- sample(as.character(1:11), sample(1:2, 1), FALSE,
                     c(.35, .10, .20, .08, .10, .04, .05, .04, .01, .02, .01))
  reports$Q206[[row]] <- paste(selected, collapse = " ")
}
reports$bleeding_product_material_other <- if_else(
  str_detect(coalesce(reports$Q206, ""), "(^| )11( |$)"),
  "Other dummy product", NA_character_
)

reports <- reports |>
  group_by(uniqueid) |>
  arrange(date, .by_group = TRUE) |>
  ungroup() |>
  mutate(seven_days = data_cutoff - 7)

# Populate the calculated previous-day fields used by the form's completion display.
for (lag_number in 1:6) {
  date_name <- if (lag_number == 1) "date_previous" else paste0("date_previous", lag_number)
  id_name <- if (lag_number == 1) "id_previous" else paste0("id_previous", lag_number)
  previous_name <- if (lag_number == 1) "previous_day" else paste0("previous_day", lag_number)
  label_name <- if (lag_number == 1) "label_date_previous" else paste0("label_date_previous", lag_number)
  reports[[date_name]] <- reports$date - lag_number
  reports[[id_name]] <- paste0(reports$uniqueid, reports[[date_name]])
  submitted_keys <- paste0(reports$uniqueid, reports$date)
  reports[[previous_name]] <- as.integer(reports[[id_name]] %in% submitted_keys)
  reports[[label_name]] <- paste(
    format(reports[[date_name]], "%a, %b %e"),
    if_else(reports[[previous_name]] == 1, "Received", "Not received")
  )
}

reports <- reports |>
  rowwise() |>
  mutate(
    last_missed_day = {
      candidate_dates <- c_across(all_of(c("date_previous", paste0("date_previous", 2:6))))
      received <- c_across(all_of(c("previous_day", paste0("previous_day", 2:6))))
      missing_dates <- candidate_dates[received == 0]
      if (length(missing_dates) == 0) as.Date(NA) else max(as.Date(missing_dates, origin = "1970-01-01"))
    },
    last_missed_day_label = if_else(is.na(last_missed_day), NA_character_, format(last_missed_day, "%a, %b %e")),
    label_date_today = paste(format(date, "%a, %b %e"), "Received")
  ) |>
  ungroup() |>
  mutate(
    `_id` = row_number(),
    `_uuid` = sprintf("uuid-cimc-%06d", row_number()),
    `_submission_time` = paste0(date + 1, "T", sprintf("%02d", sample(6:10, n(), TRUE)), ":", sprintf("%02d", sample(0:59, n(), TRUE)), ":00Z")
  )

reports_full <- reports
reports <- add_missing_columns(reports_full, cimc_fields)

daily_analysis <- reports_full |>
  left_join(
    enrolment |> select(uniqueid, programme_start_date = contraception_start_date),
    by = "uniqueid"
  ) |>
  transmute(
    participant_id = uniqueid,
    programme_start_date,
    report_date = as.Date(date),
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

monthly_analysis <- reports_full |>
  filter(monthly_report_due == "yes") |>
  left_join(
    enrolment |> select(uniqueid, programme_start_date = contraception_start_date),
    by = "uniqueid"
  ) |>
  transmute(
    participant_id = uniqueid,
    programme_start_date,
    reference_month = previous_month,
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

write_csv(recruitment, "dummy_recruitment_form_data.csv", na = "")
write_csv(enrolment, "dummy_enrolment_sociodemographics_data.csv", na = "")
write_csv(reports, "dummy_CIMC_pro_data.csv", na = "")
write_csv(daily_analysis, "dummy_daily_epro_data.csv", na = "")
write_csv(monthly_analysis, "dummy_monthly_epro_data.csv", na = "")

message("Created form-faithful linked dummy datasets through ", data_cutoff)
