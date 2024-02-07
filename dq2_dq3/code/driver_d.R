
# Top-level code for execution of data request

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(rlang))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(tidyr))

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

  rslt <- list()

  message('Computing urine labs with raw_results evaluated')
  rslt$up_qual <- make_qualitative_lab_table(cohort = results_tbl('attrition_cohort')) %>%
                  output_tbl('qualitative_urine_protein_cts')

  message('Computing quantitative urine labs')
  rslt$up_quan <- make_up_quant_table(cohort = results_tbl('attrition_cohort')) %>%
                  output_tbl('quantitative_urine_protein',
                             indexes=list('patid'))

  message('Computing drugs')
  # loop diuretics excluded from htn meds
  rslt$rx_htn <- find_drugs_d(cohort = results_tbl('attrition_cohort'),
                               fact_codes=list(load_codeset('rx_ace_inhibitor','icccccc'),
                                               load_codeset('rx_arb','icccccc'),
                                               load_codeset('rx_bb','icccccc'),
                                               load_codeset('rx_ccb','icccccc'),
                                               load_codeset('rx_thiazide','icccccc'))) %>%
                  output_tbl('rx_htn',
                             indexes=list('patid','encounterid','drug_code'))

  rslt$lab_hts_all <- make_lab_temporal_tbl(cohort=results_tbl('attrition_cohort'),
                                            compare_tbl=cdm_tbl('vital') %>%
                                              filter(!is.na(ht)) %>%
                                              select(patid,site,measure_date),
                                            compare_tbl_date_col='measure_date') %>%
                      output_tbl('lab_hts_all',
                                 indexes=list('patid'))

  rslt$lab_ht_min <- compute_lab_ht_min(lab_ht_tbl = rslt$lab_hts_all) %>%
                     output_tbl('lab_ht_min',
                                indexes=list('patid'))

  egfr_cohort <- results_tbl(in_schema('preserve_cro','egfr_bounded_preserve_153'), results_tag=FALSE) %>% 
    inner_join(select(results_tbl('attrition_cohort'), patid), by = "patid") %>% 
    compute_new()
  
  gen_persist <- function(tval) {
    message('Computing consistency of low eGFR (', tval, ')')
    rslt[[paste0('low_egfr_frac_', tval)]] <-
      egfr_below_threshold(lower_limit = tval) %>%
      output_tbl(name = paste0('egfr_low_frac_', tval))
  }
  lapply(seq(10L, 90L, by = 10L), gen_persist)

  message('Computing days separation between serum creatinine and height
          used to calculate eGFR')
  rslt$egfr_days_sep_cts <-
    categorize_efgr_days_sep(egfr_table = egfr_cohort) %>%
    output_tbl(name = 'egfr_days_sep_cts')

  message('Computing utilization of urine protein tests')
  rslt$uprot_test_frac <- make_uprot_util_sum(cohort=results_tbl('attrition_cohort')) %>%
    output_tbl(name = 'uprot_test_cts')

  message('Compute patients with a hypertension diagnosis')
  rslt$hypertension_dx_patients <-
    get_qualifying_patients(pt_tbl = results_tbl('attrition_cohort')) %>%
    output_tbl(name = 'htn_dx_cohort')
  message('Compute patients with a high systolic or diastolic bp')
  rslt$high_bp_patients <-
    high_sys_or_dias_within_period() %>% compute_new()
  message('Compute overlap between hypertension dx and drug')
  rslt$htn_dx_intersection_summary <-
    make_intersection_summary(cohort = results_tbl("attrition_cohort"),
                              ht_dx_tbl = rslt$hypertension_dx_patients,
                              ht_trmnt_tbl = rslt$rx_htn) %>%
    output_tbl(name = 'htn_dx_intersection_summary')
  message('Compute overlap between high bp and drug')
  rslt$htn_meas_intersection_summary <-
    make_intersection_summary(cohort = results_tbl("attrition_cohort"),
                              ht_dx_tbl = rslt$high_bp_patients,
                              ht_trmnt_tbl = rslt$rx_htn) %>%
    output_tbl(name = 'htn_meas_intersection_summary')

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
