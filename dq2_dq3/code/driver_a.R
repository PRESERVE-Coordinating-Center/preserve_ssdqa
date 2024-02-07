# Top-level code for execution of data request

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(rlang))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(readr))

# Need to do this for assignInNamespace to work
suppressPackageStartupMessages(library(dbplyr))

# Required for execution using Rscript
suppressPackageStartupMessages(library(methods))

#' Set up the execution environment
#'
#' The .load() function sources the R files needed to execute the query
#' and sets up the execution environment.  In particular, all of the base
#' framework files, as well as files inthe code_dir with names matching
#' `cohort_*.R` or `analyze_*.R` will be sourced.
#'
#' This function is usually run automatically when the `run.R` file is sourced
#' to execute the request.  It may also be executed manually during an
#' interactive session to re-source changed code or to re-establish a connection
#' to the database.
#'
#' **N.B.** You will almost never have to edit this function.
#'
#' @param here The name of the top-level directory for the request.  The default
#'   is `config('base_dir')` if the config function has been set up, or the
#'   global variable `base_dir` if not.
#'
#' @return The value of `here`.
#' @md
.load <- function(here = ifelse(typeof(get('config')) == 'closure',
                                config('base_dir'), base_dir)) {
    source(file.path(here, 'code', 'config.R'))
    source(file.path(here, 'code', 'req_info.R'))
    source(config('site_info'))
    source(file.path(here, config('subdirs')$code_dir, 'setup.R'))
    source(file.path(here, config('subdirs')$code_dir, 'codesets.R'))
    for (fn in list.files(file.path(here, config('subdirs')$code_dir),
                        'util_.+\\.R', full.names = TRUE))
      source(fn)
    for (fn in list.files(file.path(here, config('subdirs')$code_dir),
                          'cohort_.+\\.R', full.names = TRUE))
      source(fn)
    for (fn in list.files(file.path(here, config('subdirs')$code_dir),
                        'analyze_.+\\.R', full.names = TRUE))
      source(fn)
    source(file.path(here, config('subdirs')$code_dir, 'cohorts.R'))

    .env_setup()

    for (def in c('retain_intermediates', 'results_schema')) {
      if (is.na(config(def)))
        config(def, config(paste0('default_', def)))
    }

    here
}

#' Execute the request
#'
#' This function presumes the environment has been set up, and executes the
#' steps of the request.
#'
#' In addition to performing queries and analyses, the execution path in this
#' function should include periodic progress messages to the user, and logging
#' of intermediate totals and timing data through [append_sum()].
#'
#' This function is also typically executed automatically, but is separated from
#' the setup done in [.load()] to facilitate direct invocation during
#' development and debugging.
#'
#' @param base_dir The name of the top-level directory for the request.  The default
#'   is `config('base_dir')`, which should always be valid after execution of
#'   [.load()].
#'
#' @return The return value is dependent on the content of the request, but is
#'   typically a structure pointing to some or all of the retrieved data or
#'   analysis results.  The value is not used by the framework itself.
#' @md
.run  <- function(base_dir = config('base_dir')) {

  # Compute narrow attrition cohort ----
  # Note: No sites using the append strategy for nephrology facility
  cdm_tbl("encounter") %>%
    filter(
      str_detect(toupper(raw_facility_type), "163WN0300X") |
        str_detect(toupper(raw_facility_type), "207RN0300X") |
        str_detect(toupper(raw_facility_type), "2080P0210X") |
        str_detect(toupper(raw_facility_type), "246ZN0300X")
    ) %>%
    tally() %>% pull() == 0

  # Get attrition cohort prior to implementation of nephrology specialty
  # requirement (step 8) - this was a component of the cohort extracted from sites)
  attr_cht_step7 <- cdm_tbl("broad_ckd_cht_info") %>%
    filter(attr_cht == 1) %>%
    distinct(patid, site, ce_date) %>%
    compute_new()

  # Nephrology specialty codesets
  neph_faci_vs <-
    c("HOSPITAL_OUTPATIENT_PEDIATRIC_NEPHROLOGY_CLINIC")
  neph_prov_vs <-
    c("163WN0300X", "207RN0300X", "2080P0210X", "246ZN0300X")

  # Get nephrology encounters for broad ckd cohort
  nephrology_encounters <-
    get_spec_encounters(
      faci_type_vs = neph_faci_vs,
      prov_spec_primary_vs = neph_prov_vs,
      min_date = as.Date("2009-01-01"),
      max_date = as.Date("2021-12-31")
    )
  
  nephrology_encounters %>% output_tbl("neph_encounters")

  # All kidney transplants procedures for broad ckd cohort
  kidney_transplant_px <- get_px(px_codeset = load_codeset("px_kidney_transplant")) %>%
    compute_new()

  kidney_transplant_px %>% output_tbl("kidney_transplant_px")

  kidney_transplant_px_before_ced <- kidney_transplant_px %>%
    inner_join(attr_cht_step7, by = "patid") %>%
    filter(px_date < ce_date)

  kidney_transplant_px_before_ced %>% output_tbl("kidney_transplant_px_before_ced")

  # Any dialysis procedures for broad ckd cohort
  kidney_dialysis_any_px <-
    get_px(px_codeset = load_codeset("px_kidney_dialysis")) %>%
    compute_new()

  kidney_dialysis_any_px %>% output_tbl("kidney_dialysis_any_px")

  # Chronic dialysis procedures for broad ckd cohort
  chronic_dialysis_any_px <-
    get_preserve_chronic_dialysis_px(px_codeset = load_codeset("px_chronic_dialysis")) %>%
    copy_to_new(df = .,
                name = "chronic_dialysis_temp")

  chronic_dialysis_any_px %>% output_tbl("chronic_dialysis_any_px")

  # Patients with 3 consecutive months with chronic dialysis for broad ckd cohort
  chronic_dialysis_consec_px <-
    chronic_dialysis_any_px %>%
    filter(consec_3 == TRUE) %>%
    group_by(patid, site) %>%
    summarize(chronic_dialysis_init_date = min(px_date_first_month,
                                               na.rm = TRUE)) %>%
    ungroup()

  chronic_dialysis_consec_px %>% output_tbl("chronic_dialysis_consec_px")

  # Patients for whom first day of first month for 3 consecutive months with chronic dialysis
  # procedures is before CED, for broad ckd cohort
  chronic_dialysis_init_before_ced <- chronic_dialysis_consec_px %>%
    inner_join(attr_cht_step7, by = "patid") %>%
    filter(chronic_dialysis_init_date < ce_date)

  chronic_dialysis_init_before_ced %>% output_tbl("chronic_dialysis_init_before_ced")

  # Compute final attrition cohort
  attrition_cohort <- attr_cht_step7 %>%
    # step 8 - nephrology visit requirement
    inner_join(distinct(results_tbl("neph_encounters"), patid),
               by = "patid") %>%
    # step 9 - exclude chronic dialysis before CED
    anti_join(distinct(results_tbl("chronic_dialysis_init_before_ced"), patid),
              by = "patid") %>%
    # step 10 - exclude kidney transplant before CED
    anti_join(distinct(results_tbl("kidney_transplant_px_before_ced"), patid),
              by = "patid")

  attrition_cohort %>% output_tbl("attrition_cohort")

  # Compute cohort membership table
  attrition_cohort_flag <- attrition_cohort %>%
    distinct(patid) %>%
    mutate(attr_cht = 1L)

  neph_encounter_flag <- results_tbl("neph_encounters") %>%
    distinct(patid) %>%
    mutate(neph_encounter = 1L)

  cohort_membership <- cdm_tbl("broad_ckd_cht_info") %>%
    rename(broad_ckd_cohort = attr_cht,
           broad_high_scr = high_scr) %>%
    left_join(attrition_cohort_flag) %>%
    mutate(attr_cht = if_else(is.na(attr_cht), 0L, attr_cht)) %>%
    left_join(neph_encounter_flag) %>%
    mutate(neph_encounter = if_else(is.na(neph_encounter), 0L, neph_encounter)) %>%
    select(
      patid,
      site,
      ce_date,
      ce_year,
      ce_age_years,
      attr_cht,
      neph_encounter,
      broad_ckd_cohort,
      broad_high_scr,
      ckd_dx
    ) %>%
    compute_new()

  cohort_membership %>% output_tbl("cohort_membership")

  # Compute initial approach to composite renal outcome  ----
  cro_time_to_event <- get_cro_time_to_event(
    cohort = results_tbl("attrition_cohort"),
    chronic_dialysis = results_tbl("chronic_dialysis_consec_px") %>% rename(chronic_dialysis_date = chronic_dialysis_init_date),
    kidney_transplant = results_tbl("kidney_transplant_px") %>% rename(kidney_transplant_date = px_date),
    egfr = results_tbl(in_schema('preserve_cro','egfr_bounded_preserve_153'), results_tag=FALSE) %>% rename(egfr_date = comb_lab_date) %>% select(-ce_date),
    min_date = as.Date("2009-01-01"),
    max_date = as.Date("2021-12-31")
  )

  cro_time_to_event %>% output_tbl("cro_time_to_event")
  
  # ABPM
  abpm_any_px <-
    get_px(px_codeset = load_codeset("px_abpm")) %>%
    compute_new()
  
  abpm_any_px %>% output_tbl("abpm_any_px")
  
  # Get oncology encounters for attrition cohort
  heme_onc_faci_vs <-
    c(
      'HOSPITAL_OUTPATIENT_PEDIATRIC_HEMATOLOGY_ONCOLOGY_CLINIC',
      'HOSPITAL_OUTPATIENT_ONCOLOGY_CLINIC'
    )
  heme_onc_prov_vs <- c(
    '207RH0000X',
    '207ZH0000X',
    '207RH0003X',
    '207RX0202X',
    '2086X0206X',
    '2080P0207X',
    '2085R0001X',
    '246QH0000X',
    '261QX0203X',
    '364SX0200X',
    '364SX0204X',
    '163WP0218X',
    '163WX0200X'
  )
  heme_onc_encounters <-
    get_spec_encounters(
      cohort = results_tbl("attrition_cohort"),
      faci_type_vs = heme_onc_faci_vs,
      prov_spec_primary_vs = heme_onc_prov_vs,
      min_date = as.Date("2009-01-01"),
      max_date = as.Date("2021-12-31")
    )
  heme_onc_encounters %>% output_tbl("heme_onc_encounters")
  
}

#' Set up and execute a data request
#'
#' This function encapsulates a "production" run of the data request.  It sets
#' up the environment, executes the request, and cleans up the environment.
#'
#' Typically, the `run.R` file calls run_request() when in a production mode.
#'
#' @param base_dir Path to the top of the data request files.  This is
#'   typically specified in `run.R`.
#'
#' @return The result of [.run()].
#' @md
run_request <- function(base_dir) {
    base_dir <- .load(base_dir)
    on.exit(.env_cleanup())
    .run(base_dir)
}
