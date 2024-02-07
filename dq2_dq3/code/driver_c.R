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

  init_sum(cohort = 'Start', persons = 0)
  rslt <- list()

  # Attrition cohort
  message('Finding BP completeness info')
  rslt$bp_complete <- complete_bp(cohort = results_tbl('attrition_cohort')) %>%
    output_tbl(name = 'bp_completeness',
               indexes = list('patid'))
  
  rslt$cohort_bp_corr <- rslt$bp_complete %>%
    group_by(site) %>%
    summarise(corr = cor(systolic, diastolic)) %>%
    ungroup() %>%
    collect() %>%
    mutate(corr = round(corr, 2)) %>%
    output_tbl(name = 'cohort_bp_corr')
  
  rslt$cohort_bp_sample <- rslt$bp_complete %>% group_by(site) %>%
    slice_sample(n = 5000) %>%
    output_tbl(name = 'cohort_bp_sample',
               indexes = list('patid'))
  
  rslt$cohort_bp_sample2 <- rslt$bp_complete %>%
    group_by(site) %>%
    slice_sample(n = 50000) %>%
    output_tbl(name = 'cohort_bp_sample2',
               indexes = list('patid'))
  
  # Output BP tables for the cohort of patients excluded at step 8 of the attrition
  ex_step_8 <- results_tbl("cohort_membership") %>% 
    filter(attr_cht == 0,
           broad_ckd_cohort == 1) %>% 
    distinct(patid, site, ce_date) %>% 
    compute_new()
  
  message('Finding BP completeness info')
  rslt$ex_step_8_bp_complete <- complete_bp(cohort = ex_step_8) %>%
    compute_new(indexes = list('patid'))
  
  rslt$ex_step_8_bp_corr <- rslt$ex_step_8_bp_complete %>%
    group_by(site) %>%
    summarise(corr = cor(systolic, diastolic)) %>%
    ungroup() %>%
    collect() %>%
    mutate(corr = round(corr, 2)) %>%
    output_tbl(name = 'ex_step_8_bp_corr')
  
  rslt$ex_step_8_bp_sample <- rslt$ex_step_8_bp_complete %>% group_by(site) %>%
    slice_sample(n = 5000) %>%
    output_tbl(name = 'ex_step_8_bp_sample',
               indexes = list('patid'))
  
  rslt$ex_step_8_bp_sample2 <- rslt$ex_step_8_bp_complete %>%
    group_by(site) %>%
    slice_sample(n = 50000) %>%
    output_tbl(name = 'ex_step_8_bp_sample2',
               indexes = list('patid'))
  
  message('Finding antihypertensive medication exposures')
  # Exclude loop diuretics from htn medications
  rslt$cohort_htn_rx <- find_drugs(admin_tbl=cdm_tbl('med_admin'),
                                   presc_tbl=cdm_tbl('prescribing'),
                                   fact_codes=load_codeset('rx_ace_inhibitor', col_types='icccccc')%>%
                                     dplyr::union(load_codeset('rx_arb', col_types='icccccc'))%>%
                                     dplyr::union(load_codeset('rx_bb', col_types='icccccc'))%>%
                                     dplyr::union(load_codeset('rx_ccb', col_types='icccccc'))%>%
                                     dplyr::union(load_codeset('rx_thiazide', col_types='icccccc'))) %>%
    compute_new(indexes=list('patid'))
  rslt$cohort_htn_rx_summary <- gen_min_summary(fact_tbl=rslt$cohort_htn_rx,
                                                date_field="drug_date",
                                                cohort_tbl=results_tbl('attrition_cohort')) %>%
    output_tbl(name='med_ah_rx_first',
               indexes = list('patid'))

  rslt$htn_rx_prop <- summarize_rx_any(rslt$cohort_htn_rx_summary) %>%
    output_tbl('med_ah_rx_prop')

  rslt$htn_rx_prop_postced <-
    summarize_rx_any(rslt$cohort_htn_rx_summary %>%
                       mutate(ever = case_when(ever==1 & min_date >= ce_date ~ 1L,
                                               TRUE ~ 0L))) %>%
    output_tbl('med_ah_rx_postce_prop')

  rslt$ah_rx_time <- find_rx_time(drug_tbl=rslt$cohort_htn_rx,
                                  cohort_tbl=results_tbl('attrition_cohort')) %>%
    output_tbl(name='med_ah_rx_time')

  message('Finding address histories')
  rslt$address_history_zip <-
    count_address_individual(
      address_tbl = cdm_tbl('lds_address_history'),
      obs_gen_tbl = cdm_tbl('obs_gen') %>%
        mutate(
          obsgen_code = if_else(site == "site_a" && # Remap site a CBG as CT per communication from site a
                                  obsgen_code == '49084-7', '42026-5', obsgen_code)
        ),
      cohort_tbl = results_tbl('attrition_cohort')
    ) %>% 
    compute_new(indexes = list('patid'))
  rslt$cohort_fu <- compute_followup(ced_tbl=results_tbl('attrition_cohort'),
                                     encounter_tbl=cdm_tbl('encounter')) %>%
    compute_new(indexes=list('patid'))

  rslt$address_history_duration <-
    summarise_addr_spans(rslt$address_history_zip, rslt$cohort_fu) %>%
    output_tbl(name = 'address_history_cohort')

  rslt$address_summary_site <- summarise_addresses(rslt$address_history_zip) %>%
    output_tbl(name = 'address_history_site')

  message('Done.')

  invisible(rslt)

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
