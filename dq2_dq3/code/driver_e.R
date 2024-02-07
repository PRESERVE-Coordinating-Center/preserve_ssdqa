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
  
  suppressPackageStartupMessages(library(purrr))
  
  message('Computing specialty table')
  visits <- compute_all_specialties()
  
  message('Computing visits per patient')
  visit_specialty_cts <- compute_anyspec_summary(visits,
                                                 attr_cohort = results_tbl('attrition_cohort')) %>%
    output_tbl(.,
               name = 'visit_specialty_cts',
               db = TRUE,
               file = FALSE)
  
  message('Computing number of nephrology visits')
  visits_person_ct <-
    compute_num_neph(cohort = results_tbl('attrition_cohort'))
  output_tbl(visits_person_ct,
             'visits_spec_prop',
             db = TRUE,
             file = FALSE)
  
  message('Computing visits with urine protein')
  visits_urine_protein <-
    compute_urine_protein(cohort = results_tbl('attrition_cohort'))
  output_tbl(visits_urine_protein,
             'visits_urine_protein',
             db = TRUE,
             file = FALSE)
  
  message('Computing visits with drugs')
  visits_rx <-
    compute_drugs_rx(cohort = results_tbl('attrition_cohort'))
  output_tbl(visits_rx,
             'visits_rx',
             db = TRUE,
             file = FALSE)
  
  message('Computing visits with blood pressures')
  visits_bp <-
    compute_bp_sys(cohort = results_tbl('attrition_cohort'))
  output_tbl(visits_bp,
             'visits_bp',
             db = TRUE,
             file = FALSE)
  
  message('Computing condition adverse events')
  
  computeconds_attr_cohort <- function(codeset) {
    output <- find_conditions(codeset = codeset,
                              cohort = results_tbl('attrition_cohort'))
    output
  }
  
  conditions_list <- list(
    load_codeset('dx_cough', 'icccc'),
    load_codeset('dx_depression', 'icccc'),
    load_codeset('dx_hypotension', 'icccc'),
    load_codeset('dx_dizziness', 'icccc'),
    load_codeset('dx_fatigue', 'icccc'),
    load_codeset('dx_pericarditis', 'icccccc'),
    load_codeset('dx_asthma', 'icccccc')
  )
  
  ae_conditions <- purrr::map(.x = conditions_list,
                              .f = computeconds_attr_cohort)
  
  ae_conditions_reduce <- purrr::reduce(.x = ae_conditions,
                                        .f = dplyr::union)
  
  output_tbl(ae_conditions_reduce,
             'ae_conditions',
             db = TRUE,
             file = FALSE)
  
  egfr_cohort <- results_tbl(in_schema('preserve_cro','egfr_bounded_preserve_153'), results_tag=FALSE) %>% 
    inner_join(select(results_tbl('attrition_cohort'), patid), by = "patid") %>% 
    compute_new()
  
  message('Computing egfr values')
  egfr_medians <-
    compute_median_egfr(egfr_tbl = egfr_cohort)
  output_tbl(egfr_medians,
             'egfr_medians',
             db = TRUE, file = FALSE)
  
  message('Computing egfr values with washout')
  egfr_washout <-
    compute_median_egfr_washout(egfr_tbl = egfr_cohort)
  output_tbl(egfr_washout,
             'egfr_washout',
             db = TRUE, file = FALSE)

  
  egfr_site_medians <-
    compute_site_median_egfr(egfr_tbl = egfr_cohort)
  output_tbl(egfr_site_medians,
             'egfr_site_medians')
  
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
