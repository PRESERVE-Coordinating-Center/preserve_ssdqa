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
#'

#' Function to reformat a medication codeset to be used in the PCORnet data model
#' @param code_tbl table of codes with at least a `concept_code` and `pcornet_vocabulary_id` column
#' @return all of the distinct concept_code and pcornet_vocabulary_id in the code_tbl
process_pcornet_codeset <- function(code_tbl) {
  code_tbl %>% select(concept_code, pcornet_vocabulary_id) %>%
    distinct()
}

#' Function to find exposures to medications (both admins and prescriptions) in a codeset
#' @param admin_tbl table following the PCORnet med_admin format
#' @param presc_tbl table following the PCORnet prescribing format
#' @param fact_codes codeset
#' @return table with both med_admins and prescriptions, with a column drug_type indicating whether originated from med_admin or prescribing table
find_drugs <- function(admin_tbl,
                       presc_tbl,
                      fact_codes) {
  codes_formatted <- process_pcornet_codeset(fact_codes)
  # medadmins
  rx_admin <- admin_tbl %>%
    inner_join(codes_formatted, by = c('medadmin_code'='concept_code', 'medadmin_type'='pcornet_vocabulary_id')) %>%
    mutate(drug_type = 'med_admin')
  # prescriptions
  rx_presc <- presc_tbl %>%
    inner_join(codes_formatted, by = c('rxnorm_cui'='concept_code')) %>%
    mutate(drug_type='prescription')

  all_rx <- rx_admin %>%
    dplyr::union_all(rx_presc) %>%
    mutate(drug_date = coalesce(medadmin_start_date, rx_start_date, rx_order_date))
}

#' Function to find the first date of an event and flag whether ever happened
#' @param fact_tbl table to find min date in
#' @param date_field quoted column name of date field to find first of
#' @param cohort_tbl table with distinct patids for cohort
#' @return table with patid, site, ever (0/1 for whether or not event ever happened), min_date (date of first occurrence)
gen_min_summary <- function(fact_tbl,
                      date_field,
                      cohort_tbl) {
  # find first if event ever happened
  min_dates <- fact_tbl %>%
    group_by(patid, site) %>%
    summarise(min_date = min(!!sym(date_field), na.rm=TRUE),
              tot_ct = n()) %>%
    mutate(ever=1L) %>%
    ungroup()

  # bring in the pats in cohort without
  cohort_tbl %>% select(patid, ce_date, site)%>%
    left_join(min_dates, by = c('patid', 'site')) %>%
    mutate(tot_ct=case_when(ever==1L~as.integer(tot_ct),
                            TRUE~0L),
           ever=case_when(ever==1L~1L,
                          TRUE ~ 0L)) %>%
    mutate(time_to_rx=as.integer(min_date-ce_date),
           time_to_rx_years=time_to_rx/365.25)

}

#' Function to compute proportion of patients with exposure to a medication over time
#' @param drug_tbl table with drug exposures at least the column `drug_date`, `site`, `patid`
#' @param cohort_tbl table with cohort entry dates `ce_date` and `patid` for cohort
#' @return table with the cols:
#'              `site`
#'              `drug_year`
#'              `n_pats`: number of distinct patients with at least one exposure to med in the `drug_year`
#'              `site_total_pats`: total number of patients at site to have at least one exposure at any time
#'              `prop_pats`: `n_pats`/`site_total_pats`
find_rx_time <- function(drug_tbl,
                         cohort_tbl) {
  ced_tbl <- cohort_tbl %>%
    select(patid, ce_date) %>%
    distinct()
  # drugs of interest on/after CED
  drugs_all <- drug_tbl %>%
    inner_join(ced_tbl, by = 'patid') %>%
    filter(drug_date >= ce_date) %>%
    mutate(drug_year=year(drug_date))
  # total site counts ever
  site_tot_pats <- drugs_all %>%
    group_by(site) %>%
    summarise(site_tot_pats=n_distinct(patid)) %>%
    ungroup()
  # compute proportions per year
  drugs_all %>%
    group_by(drug_year, site) %>%
    summarise(n_pats=n_distinct(patid))%>%
    ungroup() %>%
      inner_join(site_tot_pats, by = 'site') %>%
    collect()%>%
    mutate(prop_pats = round(n_pats/site_tot_pats,2))
}

#' Compute proportions of patients ever on antihypertensive
#'
#' @param ah_tbl Antihypertensive meds, as returned by [find_rx_time()]
#'
#' @return A per-site summary of proportion of patients ever or never
#'   on antihypertensive
summarize_rx_any <- function(ah_tbl) {
  ah_tbl %>%
    group_by(site, ever) %>%
    summarise(tot_pats=n_distinct(patid)) %>%
    ungroup() %>%
    group_by(site) %>%
    mutate(prop_pats=round(tot_pats/sum(tot_pats, na.rm = TRUE),2))
}

#' Find BP measurements and annotate with completeness
#'
#' @param cohort The cohort of patients to examine; must contain patid]
#' @param bp_tbl The tbl containing vital sign measurements
#' @param dob_tbl Tbl from which to get dob for age computation
#'
#' @return Tbl of BP measurements with information about completeness of each
complete_bp <- function(cohort = cdm_tbl('attrition_cohort'),
                     bp_tbl = cdm_tbl('vital'),
                     dob_tbl = cdm_tbl('demographic') %>% select(patid, birth_date)) {
  bp_tbl %>%
  inner_join(select(cohort, patid, site, ce_date), by = c("patid", "site")) %>%
    filter(!is.na(systolic) | !is.na(diastolic)) %>%
  inner_join(dob_tbl, by = "patid") %>%
  mutate(measure_age = floor((measure_date - birth_date)/365.23)) %>%
  mutate(bp_completeness=case_when(!is.na(systolic)&!is.na(diastolic)~'both',
                                   !is.na(systolic)~'systolic_only',
                                   !is.na(diastolic)~'diastolic_only',
                                   TRUE ~ 'neither')) %>%
  filter(bp_completeness!='neither') %>%
  select(-birth_date)
}

#' Function to count number of available address elements per patient
#' @param address_tbl table with the cols `address_zip5`, `address_zip9`, assuming format of the PCORnet `lds_address_history` table
#' @return table with summary address information at the patient level, with the columns:
#'           patid
#'           site
#'           num_zip5: # of distinct zip5 available for patient
#'           num_zip9: # of distinct zip9 available for patient
#'           num_cbg: # of distinct census block group available for patient
#'           num_ct: # of distinct census tract available for patient
#'           min_address_date: first address_period_start for patient
#'           max_address_date: last address for patient, first favoring address_period_end, then address_period_start if address_period_end not available
count_address_individual <- function(address_tbl,
                                     obs_gen_tbl = cdm_tbl('obs_gen'),
                          cohort_tbl) {
  cohort_distinct <- cohort_tbl %>%
    select(patid, site) %>%
    distinct()

  any_zip5 <- address_tbl %>%
    filter(!is.na(address_zip5)) %>%
    group_by(patid) %>%
    summarise(num_zip5=as.integer(n_distinct(address_zip5))) %>%
    ungroup()
  any_zip9 <- address_tbl %>%
    filter(!is.na(address_zip9)) %>%
    group_by(patid) %>%
    summarise(num_zip9=as.integer(n_distinct(address_zip9))) %>%
    ungroup()
  any_cbg <- obs_gen_tbl %>%
    filter(obsgen_type=='LC' & obsgen_code=='49084-7') %>%
    group_by(patid)%>%
    summarise(num_cbg=as.integer(n_distinct(obsgen_result_text)))%>%
    ungroup()
  any_ct <- obs_gen_tbl %>%
    filter(obsgen_type=='LC' & obsgen_code=='42026-5') %>%
    group_by(patid) %>%
    summarise(num_ct=as.integer(n_distinct(obsgen_result_text)))%>%
    ungroup()
  count_address <- address_tbl %>%
    filter(!(is.na(address_zip5) && is.na(address_zip9))) %>% 
    select(addressid, patid, address_zip5, address_zip9) %>%
    distinct() %>%
    group_by(patid) %>%
    summarise(num_address=as.integer(n_distinct(addressid))) %>%
    ungroup()
  address_timing <- address_tbl %>%
    mutate(final_address_date = coalesce(address_period_end, address_period_start)) %>%
    group_by(patid) %>%
    summarise(min_address_date = min(address_period_start),
              max_address_date = max(final_address_date))%>%
    ungroup()

  cohort_distinct %>%
    left_join(any_zip5, by = 'patid') %>%
    left_join(any_zip9, by = 'patid') %>%
    left_join(count_address, by = 'patid') %>%
    left_join(address_timing, by = 'patid') %>%
    left_join(any_cbg, by = 'patid') %>%
    left_join(any_ct, by = 'patid') %>%
    # where() selector doesn't work with across() in dbplyr()
    mutate(across(starts_with('num_'), ~ coalesce(.x, 0L)))
}

compute_followup <- function(ced_tbl,
                             encounter_tbl) {
  encounter_tbl %>%
    group_by(patid) %>%
    summarise(last_fu_date = max(admit_date, na.rm=TRUE)) %>%
    ungroup() %>%
    right_join(ced_tbl, by = 'patid')
}

#' Function to compute counts and proportions of available address data
#'     given individual-level availability of addresses
#' @param address_individual table with the cols:
#'                               patid, site, num_zip5, num_zip9, num_cbg
#' @return table with cols:
#'                    site, numpat_zip5, numpat_zip9, numpat_cbg, numpat_multzip, numpat_multcbg, numpat_multadd, site_total
#'                    prop_numpat_zip5, prop_numpat_zip9, prop_numpat_cbg, prop_numpat_multzip, prop_numpat_multadd
#'                       where the numpat columns are # patients at site with available
#'                             and prop columns are proportion of pats at site with available
#'
summarise_addresses <- function(address_individual) {

  address_individual %>%
    mutate(has_zip5 = if_else(num_zip5 >= 1L, 1L, 0L),
           has_zip9 = if_else(num_zip9 >= 1L, 1L, 0L),
           has_cbg = if_else(num_cbg >= 1L, 1L, 0L),
           has_ct = if_else(num_ct >= 1L, 1L, 0L),
           has_multzip = if_else(num_address > 1L, 1L, 0L),
           has_multzip5 = if_else(num_zip5 > 1L, 1L, 0L),
           has_multzip9 = if_else(num_zip9 > 1L, 1L, 0L),
           has_multcbg = if_else(num_cbg > 1L, 1L, 0L),
           has_multct = if_else(num_ct > 1L, 1L, 0L),
           has_multadd = if_else(num_ct > 1L | num_cbg > 1L | num_address > 1L, 1L, 0L)) %>%
    group_by(site) %>%
    summarise(numpat_zip5 = sum(has_zip5),
              numpat_zip9 = sum(has_zip9),
              numpat_cbg = sum(has_cbg),
              numpat_ct = sum(has_ct),
              numpat_multzip = sum(has_multzip),
              numpat_multzip5 = sum(has_multzip5),
              numpat_multzip9 = sum(has_multzip9),
              numpat_multcbg = sum(has_multcbg),
              numpat_multct = sum(has_multct),
              numpat_multadd = sum(has_multadd),
              site_total=n()) %>%
    mutate(across(starts_with("numpat"), ~round(1.0 * .x/site_total,2), .names="prop_{.col}"))%>%
    ungroup()
}


#' Duration information about address history
#'
#' @param addr_hx Tbl containing address histories
#' @param fu_hx Tbl containing followup histories
#'
#' @return Tbl with post-CED address history duration
summarise_addr_spans <- function(addr_hx, fu_hx) {
  addr_hx %>%
    left_join(fu_hx, by = c('patid', 'site')) %>%
    mutate(address_span = as.integer(max_address_date - min_address_date),
           fu_span = as.integer(last_fu_date - ce_date),
           address_date_diff_yrs = (fu_span - address_span) / 365.25)
}

