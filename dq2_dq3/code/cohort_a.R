#'
#' This file contains functions to identify cohorts in a study.  Its
#' contents, as well as the contents of any R files whose name begins
#' with "cohort_", will be automatically sourced when the request is
#' executed.
#'
#' For simpler requests, it will often be possible to do all cohort
#' manipulation in functions within this file.  For more complex
#' requests, it may be more readable to separate different cohorts or
#' operations on cohorts into logical groups in different files.

#' Get specialty encounters for cohort of interest using the PCORnet CDM
#'
#' @param cohort Cohort of interest
#' @param faci_type_vs PCORnet value set vector for facility_type
#' @param prov_spec_primary_vs PCORnet value set vector for provider_specialty_primary
#' @param encounter_table PCORnet encounter table
#' @param provider_table PCORnet provider table
#' @param min_date Earliest date of encounter
#' @param max_date Latest date of encounter

get_spec_encounters <- function(cohort = cdm_tbl("demographic"),
                                faci_type_vs,
                                prov_spec_primary_vs,
                                encounter_table = cdm_tbl("encounter"),
                                provider_table = cdm_tbl("provider"),
                                min_date,
                                max_date) {
  
  # distinct providerids with provider_specialty_primary of interest
  providerids <- provider_table %>%
    filter(toupper(provider_specialty_primary) %in% prov_spec_primary_vs) %>%
    distinct(providerid) %>%
    compute_new()
  
  # subset encounters to cohort and date range
  subset_encounters <- encounter_table %>%
    inner_join(select(cohort, patid), by = "patid") %>%
    filter(admit_date >= min_date,
           admit_date <= max_date)

  # encounters with providerids with provider_specialty_primary of interest
  prov_spec_encounters <- subset_encounters %>%
    inner_join(providerids, by = "providerid") %>%
    compute_new()

  # encounters where facility_type is specialty of interest
  fac_spec_encounters <- subset_encounters %>%
    filter(facility_type %in% faci_type_vs) %>%
    compute_new()

  # combine provider- and facility- based approach to identification
  spec_encounters <- prov_spec_encounters %>%
    dplyr::union(fac_spec_encounters) %>%
    distinct() %>%
    compute_new()
}

#' Get procedures in codeset
#'
#' @param cohort Cohort of interest
#' @param procedure_table PCORnet procedures table
#' @param px_codeset Procedure codeset - must contain fields concept_code and pcornet_vocabulary_id
#'
#' @return Procedures in codeset
#'
get_px <- function(cohort = cdm_tbl("demographic"),
                   procedure_table = cdm_tbl("procedures"),
                   px_codeset) {
  procedure_table %>%
    inner_join(select(px_codeset, concept_code, pcornet_vocabulary_id),
               by = c("px" = "concept_code",
                      "px_type" = "pcornet_vocabulary_id"))
}

#' Get chronic dialysis as defined by CRO WG for PRESERVE
#' 
#' Flag for 3 chronic dialysis codesets from codesets in 3 consecutive calendar months
#'
#' @param cohort Cohort of interest
#' @param procedure_table PCORnet procedures table
#' @param px_codeset Procedure codeset - must contain fields concept_code and pcornet_vocabulary_id
#'
#' @return Table of patids, px_date_first_month (first day of month of procedure),
#' consec_3 (flag for whether month is followed by 2 consecutive calendar months)
#' 
get_preserve_chronic_dialysis_px <-
  function(cohort = cdm_tbl("demographic"),
           procedure_table = cdm_tbl("procedures"),
           px_codeset) {
    chronic_dialyis_px <- procedure_table %>%
      inner_join(select(px_codeset, concept_code, pcornet_vocabulary_id),
                 by = c("px" = "concept_code",
                        "px_type" = "pcornet_vocabulary_id")) %>%
      compute_new()
    
    chronic_dialyis_px %>%
      collect_new() %>% 
      mutate(
        px_month = lubridate::month(px_date),
        px_year = lubridate::year(px_date),
        # get first day of month for px_date
        px_date_first_month = as.Date(paste0(px_year, "-", px_month, "-", 01))
      ) %>%
      distinct(patid, site, px_date_first_month) %>%
      group_by(patid, site) %>%
      arrange(px_date_first_month) %>%
      mutate(
        lag_px_date_first_month = lag(px_date_first_month),
        lead_px_date_first_month = lead(px_date_first_month),
        # if the distance between the current procedure month start and the
        # previous procedure month start is <= 31 days and the distance between
        # the current procedure month start and the next procedure month start
        # is <= 31 days, than the current procedure is in the middle
        # of 3 procedures in consecutive months
        consec_3_middle = px_date_first_month - lag(px_date_first_month) <= 31
        & lead(px_date_first_month) - px_date_first_month <= 31,
        # the prior procedure to the consec_3_middle = TRUE procedure would be
        # the first month of 3 consecutive months
        consec_3 = lead(consec_3_middle)
      ) %>%
      ungroup() %>%
      mutate(consec_3 = if_else(is.na(consec_3), FALSE, consec_3)) %>%
      select(patid, site, px_date_first_month, consec_3) %>%
      arrange(site, patid, px_date_first_month)
  }

get_cro_time_to_event <- function(cohort,
                                  chronic_dialysis,
                                  kidney_transplant,
                                  egfr,
                                  encounter_table = cdm_tbl("encounter"),
                                  ip_enc_vs = c("AV", "ED", "EI", "IP"),
                                  min_date,
                                  max_date) {
  ip_follow_up <- encounter_table %>%
    filter(enc_type %in% ip_enc_vs) %>%
    inner_join(select(cohort, patid, ce_date), by = "patid") %>%
    filter(admit_date >= min_date,
           admit_date <= max_date) %>%
    group_by(patid, ce_date) %>%
    summarize(min_ip_enc = min(admit_date),
              max_ip_enc = max(admit_date)) %>%
    ungroup() %>%
    compute_new()
  
  egfr_below_15_ro <- egfr %>%
    inner_join(ip_follow_up, by = "patid") %>%
    filter(egfr_date <= max_ip_enc,
           egfr_date >= ce_date) %>%
    inner_join(select(cohort, patid), by = "patid") %>%
    filter(egfr < 15) %>%
    group_by(patid) %>%
    summarize(min_egfr_below_15_date = min(egfr_date)) %>%
    ungroup() %>%
    mutate(egfr_below_15 = TRUE) %>%
    compute_new()
  
  chronic_dialysis_ro <- chronic_dialysis %>%
    inner_join(ip_follow_up, by = "patid") %>%
    filter(chronic_dialysis_date <= max_ip_enc,
           chronic_dialysis_date >= ce_date) %>%
    inner_join(select(cohort, patid), by = "patid") %>%
    group_by(patid) %>%
    summarize(min_chronic_dialysis_date = min(chronic_dialysis_date)) %>%
    ungroup() %>%
    mutate(chronic_dialysis = TRUE) %>%
    compute_new()
  
  kidney_transplant_ro <- kidney_transplant %>%
    inner_join(ip_follow_up, by = "patid") %>%
    filter(kidney_transplant_date <= max_ip_enc,
           kidney_transplant_date >= ce_date) %>%
    inner_join(select(cohort, patid), by = "patid") %>%
    group_by(patid) %>%
    summarize(min_kidney_transplant_date = min(kidney_transplant_date)) %>%
    ungroup() %>%
    mutate(kidney_transplant = TRUE) %>%
    compute_new()
  
  cro_time_to_event <- cohort %>%
    left_join(egfr_below_15_ro, by = "patid") %>%
    left_join(chronic_dialysis_ro, by = "patid") %>%
    left_join(kidney_transplant_ro, by = "patid") %>%
    left_join(ip_follow_up, by = c("patid", "ce_date")) %>%
    mutate(
      egfr_below_15 = if_else(is.na(egfr_below_15), FALSE, egfr_below_15),
      chronic_dialysis = if_else(is.na(chronic_dialysis), FALSE, chronic_dialysis),
      kidney_transplant = if_else(is.na(kidney_transplant), FALSE, kidney_transplant),
      cro_date = pmin(
        min_egfr_below_15_date,
        min_chronic_dialysis_date,
        min_kidney_transplant_date,
        na.rm = TRUE
      ),
      cro = if_else(is.na(cro_date), 0, 1),
      cro_check = if_else((egfr_below_15 |
                            chronic_dialysis | kidney_transplant), 0, 1),
      end_date = pmin(cro_date,
                      max_ip_enc,
                      na.rm = TRUE),
      ob_time_days = as.numeric(end_date - ce_date),
      ob_time_years = round(ob_time_days / 365.25, digits = 3)
    ) %>%
    compute_new()
  
}
