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

  message('Starting execution with framework version ',
          config('framework_version'))
  
  
  ## ip_fu_active
  ### For each year of cohort entry, calculate the cumulative sum of patients in the cohort by site
  cohort_entries <- get_cohort_entries(cohort = results_tbl('attrition_cohort')) 
  output_tbl(cohort_entries, 'cohort_entries')
  
  ### For each year and site, calculate the number and percent of patients with at least one encounter
  with_visit <- get_with_visit(cohort = results_tbl('attrition_cohort'))
  output_tbl(with_visit, 'with_visits')
  
  
  ## visits_temporal
  ### need someone to check my categorization of visit_type
  
  ### Of the patients with at least one encounter, find the percent that had outpatient, inpatient, or ED visits by year and by site
  visit_types <- get_visit_types(cohort = results_tbl("attrition_cohort"))
  output_tbl(visit_types, 'visit_types')
  
  
  ### Median number of encounters per person by year and by site (after cohort entry)
  visits_per_person <- get_visits_per_person(cohort = results_tbl("attrition_cohort"))
  output_tbl(visits_per_person, 'visits_per_person')
  
  
  ## visits_nofacts
  ### Total number of encounters by year and by site for patients in the cohort
  visits <- get_visits(cohort = results_tbl("attrition_cohort"))
  output_tbl(visits, 'visits')
  
  ### Number of encounters not associated with a diagnosis, procedure, condition, vital, or lab
  nofacts <- get_nofacts(cohort = results_tbl("attrition_cohort"), 
                             table1 = cdm_tbl('diagnosis'),
                             table2 = cdm_tbl('procedures'),
                             table3 = cdm_tbl('condition'),
                             table4 = cdm_tbl('vital'),
                             table5 = cdm_tbl('lab_result_cm'))
  output_tbl(nofacts, 'nofacts')
  
  
  ## ip_fu_facts
  ### Median number of patient facts per year, stratified by year, site, and PCORnet CDM table
  procedures_per_person <-
    get_facts_per_person(
      cohort = results_tbl("attrition_cohort"),
      domain = cdm_tbl('procedures') %>%
        mutate(combined_date = coalesce(px_date, admit_date)),
      domain_date = 'combined_date'
    )
  output_tbl(procedures_per_person, 'procedures_per_person')
  
  conditions_per_person <-
    get_facts_per_person(
      cohort = results_tbl("attrition_cohort"),
      domain = cdm_tbl('condition') %>%
        mutate(combined_date = coalesce(onset_date, report_date)),
      domain_date = 'combined_date'
    )
  output_tbl(conditions_per_person, 'conditions_per_person')
  
  diagnoses_per_person <-
    get_facts_per_person(
      cohort = results_tbl("attrition_cohort"),
      domain = cdm_tbl('diagnosis') %>%
        mutate(combined_date = coalesce(admit_date, dx_date)),
      domain_date = 'combined_date'
    )
  output_tbl(diagnoses_per_person, 'diagnoses_per_person')
  
  vitals_per_person <-
    get_facts_per_person(
      cohort = results_tbl("attrition_cohort"),
      domain = cdm_tbl('vital'),
      domain_date = 'measure_date'
    )
  output_tbl(vitals_per_person, 'vitals_per_person')
  
  labs_per_person <-
    get_facts_per_person(
      cohort = results_tbl("attrition_cohort"),
      domain = cdm_tbl('lab_result_cm') %>%
        mutate(combined_date = coalesce(
          specimen_date, result_date, lab_order_date
        )),
      domain_date = 'combined_date'
    )
  output_tbl(labs_per_person, 'labs_per_person')
  
  dispensing_per_person <-
    get_facts_per_person(
      cohort = results_tbl("attrition_cohort"),
      domain = cdm_tbl('dispensing'),
      domain_date = 'dispense_date'
    ) %>%
    rename(median_dispensed_per_person = median_per_person)
  
  prescribing_per_person <-
    get_facts_per_person(
      cohort = results_tbl("attrition_cohort"),
      domain = cdm_tbl('prescribing') %>%
        mutate(combined_date = coalesce(rx_order_date, rx_start_date)),
      domain_date = 'combined_date'
    ) %>%
    rename(median_prescribed_per_person = median_per_person)
  
  
  administered_per_person <-
    get_facts_per_person(
      cohort = results_tbl("attrition_cohort"),
      domain = cdm_tbl('med_admin')  %>%
        mutate(combined_date = coalesce(medadmin_start_date, medadmin_stop_date)),
      domain_date = 'combined_date'
    ) %>%
    rename(median_administered_per_person = median_per_person)
  
  meds_per_person <- prescribing_per_person %>%
    left_join(administered_per_person, by = c('site', 'year')) %>%
    left_join(dispensing_per_person, by = c('site', 'year'))
  output_tbl(meds_per_person, 'meds_per_person')
  

  ## provider_facility
  ### Percent of encounters that are nephrology provider specialty, nephrology facility specialty, both, or neither
  nephrology_encounters <-
    get_nephrology_encounters(cohort = results_tbl("attrition_cohort"))
  output_tbl(nephrology_encounters, 'nephrology_encounters')
  
  ### Total number of visits per year by site
  visits_per_year <- get_visits_per_year(cohort = results_tbl("attrition_cohort"))
  output_tbl(visits_per_year, 'visits_per_year')
  
  ### Of all visits, find the percent associated with a nephrology provider by year and by site
  neph_providers <- get_neph_providers(cohort = results_tbl("attrition_cohort"))
  output_tbl(neph_providers, 'neph_providers')
  
  ### Of all visits, find the percent associated with a nephrology clinic by year and by site
  neph_clinic <- get_neph_clinics(cohort = results_tbl("attrition_cohort"))
  output_tbl(neph_clinic, 'neph_clinic')
  
  ## Facility
  ### Number of encounters by facilityid of nephrology clinic
  clinics <- get_clinics(cohort = results_tbl("attrition_cohort"))
  output_tbl(clinics, 'clinics')
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
