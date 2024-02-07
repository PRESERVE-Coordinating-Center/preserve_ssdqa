library(stringr)
library(ggplot2)
library(tidyr)

#' ASSUMES TABLE HAS SITE COL
#'finds how many facts each patient has in an arbitrary table in the pcornet cdm
#'@params [tbls_dates_list] a list having structure tbl_name=c(date_column, uid_column)
find_facts_by_year_pt_level <-
  function(cohort,
           tbls_dates_list=
             list(
               'condition'=c('report_date','conditionid'),
               'diagnosis'=c('dx_date','diagnosisid'),
               'dispensing'=c('dispense_date','dispensingid'),
               'lab_result_cm'=c('lab_order_date','lab_result_cm_id'),
               'med_admin'=c('medadmin_start_date','medadminid'),
               'prescribing'=c('rx_start_date','prescribingid'),
               'procedures'=c('px_date','proceduresid'),
               'vital'=c('measure_date','vitalid'))){
    rv <- NA
    for (i in 1:length(tbls_dates_list)){
      message(i)
      tbl_name <- names(tbls_dates_list)[i]
      foo <- cdm_tbl(tbl_name) %>%
        inner_join(select(cohort, patid), by = "patid") %>% 
        rename(date_col=tbls_dates_list[[i]][1],
               uid_col=tbls_dates_list[[i]][2]) %>%
        mutate(yr = year(date_col)) %>%
        group_by(patid, site, yr) %>%
        summarise(ct_facts = n_distinct(uid_col)) %>%
        ungroup() %>%
        mutate(tbl_name_col=tbl_name)
      if (!any(is.na(rv))){
        rv <- union(rv, foo)
      } else {
        rv <- foo
      }
    }
    return(rv)
  }

#'For patients with at least one egfr below a threshold, summarize the proportion of low values
egfr_below_threshold <- function(lower_limit=30){
  egfrs <- results_tbl(in_schema('preserve_cro','egfr_bounded_preserve_153'), results_tag=FALSE) %>% 
    inner_join(select(results_tbl('attrition_cohort'), patid), by = "patid") %>%
    mutate(low_egfr=ifelse(egfr<lower_limit,1,0),
           all_egfr=1,
           test_date = comb_lab_date)

  first_low <- egfrs %>% filter(low_egfr == 1) %>%
    group_by(patid) %>% summarize(first_low = min(test_date, na.rm = TRUE))

  post_low <- egfrs %>% left_join(first_low, by = 'patid') %>%
    filter(test_date >= first_low)

  post_low %>%
    group_by(patid, site) %>%
    summarise(low_egfr=sum(low_egfr),
              all_egfr=sum(all_egfr)) %>%
    ungroup() %>%
    filter(low_egfr>0) %>%
    mutate(proportion_egfr_measures_low = low_egfr/all_egfr) %>%
    group_by(site) %>%
    summarise(percentile_25 = quantile(proportion_egfr_measures_low,0.25),
              med = median(proportion_egfr_measures_low, na.rm = TRUE),
              percentile_75 = quantile(proportion_egfr_measures_low,0.75))
}

#' Not parameterized because its very specific
#' make the dataframe with the relevant variables for the egfr temporal dq checks
make_egfr_dq_df <- function(egfr_tbl=results_tbl('egfr_bounded'),
                            cohort=cdm_tbl('attrition_cohort')){
  interim_rv <-
    egfr_tbl %>%
    inner_join(select(cohort, patid, ce_date, site), by = c("patid", "ce_date", "site")) %>%
    mutate(
      mo_yr_date=as.Date(paste0(month(comb_lab_date),'-01-',year(comb_lab_date)))) %>%
    select(site,
           mo_yr_date,
           patid,
           comb_lab_date,
           ce_date) %>%
    group_by(mo_yr_date,site, patid, ce_date) %>%
    summarise(all_egfr_measures=n_distinct(paste0(patid, comb_lab_date))) %>%
    ungroup() %>%
    mutate(pts_after_ce=ifelse(ce_date<mo_yr_date,1,0)) %>%
    group_by(mo_yr_date, site) %>%
    summarise(all_egfr_measures=sum(all_egfr_measures),
              pts_after_ce_in_month=sum(pts_after_ce),
              pts_in_month=n_distinct(patid)) %>%
    ungroup() %>%
    mutate(egfr_per_patient = all_egfr_measures/pts_in_month) %>% collect()

  #all patients in cohort for covered months
  pts_after_ce <-
    expand.grid(interim_rv %>% select(mo_yr_date) %>% distinct() %>% pull(),
                cohort %>% select(patid) %>% pull()) %>%
    inner_join(cohort %>% collect(),
               by=c('Var2'='patid')) %>%
    mutate(pts = ifelse(ce_date<Var1,1,0)) %>%
    group_by(Var1, site) %>%
    summarise(pts_after_ce_total=sum(pts)) %>%
    ungroup() %>%
    rename(mo_yr_date=Var1)

  rv <- interim_rv %>%
    inner_join(pts_after_ce,by=c('mo_yr_date','site'))

  return(rv)
}

#make urine protein table
make_up_quant_table <- function(lab_tbl=cdm_tbl('lab_result_cm'),
                                cohort=cdm_tbl('attrition_cohort'),
                                demog_tbl=cdm_tbl('demographic'),
                                quant_lab_codeset=load_codeset('lab_urine_protein_quant','icccc')){

   rv <- lab_tbl %>%
    inner_join(quant_lab_codeset,
               by=c('lab_loinc'='concept_code')) %>%
    inner_join(demog_tbl %>% select(patid,birth_date),
               by='patid') %>%
     inner_join(select(cohort, patid, ce_date),by='patid') %>%
    mutate(measure_date=coalesce(specimen_date,result_date,lab_order_date),
           age=(measure_date-birth_date)/365.25,
           na_vals = ifelse(is.na(result_num),1,0),
           unit_nonstd = ifelse(!grepl('mg/dl',lower(result_unit)) | is.na(result_unit),1,0),
           numerically_invalid=ifelse(result_num<=0 & !is.na(result_num),1,0))# %>%
   # output_tbl('quantitative_urine_protein')
  return(rv)
}

#' Days before/after a lab measure and a drug prescription
up_meas_hypertension_meds <- function(drug_tbl, up_tbl){
  rv <- up_tbl %>%
    select(-encounterid,-site) %>% 
    inner_join(drug_tbl, by='patid') %>%
    filter(!is.na(drug_start_date),
           !is.na(drug_end_date),
           !is.na(measure_date)) %>%
    mutate(
      before=measure_date >= drug_start_date-180 & measure_date < drug_start_date,
      after=measure_date <= drug_end_date+180 & measure_date > drug_end_date,
      period=ifelse(before, 'before','after')) %>%
    filter(before | after) %>%
    select(encounterid, patid, site, measure_date,period, result_num) %>%
    distinct()
  return(rv)
}


#'looks for pairs between a specified lab measurement and an arbitrary comparison
#'table, providing a summary of matches
#'the comparison table must have patid and a date column
make_lab_temporal_tbl <- function(cohort,
                                  compare_tbl,
                                  compare_tbl_date_col,
                                  lab_codeset=load_codeset('lab_serum_creatinine','icccc')) {


  pts <- cdm_tbl('demographic') %>% select(patid,
                                           birth_date) %>% inner_join(cohort,by='patid')

  lab_compare_pairs <-
    cdm_tbl('lab_result_cm') %>%
    inner_join(lab_codeset,
               by=c('lab_loinc'='concept_code')) %>%
    inner_join(pts,by=c('patid', 'site')) %>%
    mutate(comb_lab_date = coalesce(specimen_date, result_date, lab_order_date),
           age_at_measurement=round((comb_lab_date-birth_date)/365.25)) %>%
    distinct(patid,site,comb_lab_date) %>%
    compute_new(temporary=TRUE,
                indexes=list('patid'))

  hts <-
    compare_tbl %>%
    inner_join(pts,by=c('patid', 'site')) %>%
    distinct(patid,measure_date) %>% compute_new(temporary=TRUE,
                                              indexes=list('patid'))

  lab_hts <-
    lab_compare_pairs %>%
    #filter(result_unit==lab_units) %>%
    left_join(hts,by='patid') %>%
    mutate(days_diff = ifelse(is.na(abs(comb_lab_date-!!sym(compare_tbl_date_col))),
                              -1,
                              abs(comb_lab_date-!!sym(compare_tbl_date_col)))) %>%
    distinct()

}

#' find min days diff

compute_lab_ht_min <- function(lab_ht_tbl) {


  nearest <-
    lab_ht_tbl %>%
    group_by(site,
             patid,
             days_diff) %>%
    summarise(min_days_diff=min(days_diff, na.rm = TRUE))

}




#' joins a codeset and describes frequency of qualitative lab
#' results within that codeset
#'
make_qualitative_lab_table <-
  function(cohort = cdm_tbl('attrition_cohort'),
           codeset_tbl =
             load_codeset('lab_urine_protein_qual', 'icccc')) {
    df <- cdm_tbl('lab_result_cm') %>%
      inner_join(cohort) %>%
      inner_join(codeset_tbl,
                 by = c('lab_loinc' = 'concept_code')) %>%
      select(site, lab_result_cm_id, raw_result, result_qual, result_num) %>%
      mutate(raw_test = ifelse(!is.na(raw_result), NA, lab_result_cm_id)) %>%
      mutate(raw_test_workaround = ifelse(is.na(raw_test), 1L, 0L)) %>%
      group_by(site, result_qual, result_num) %>%
      summarise(
        measurements = n_distinct(lab_result_cm_id),
        measurements_raw_result_is_na = n_distinct(raw_test),
        need_to_subtract_one = sum(raw_test_workaround)
      ) %>%
      ungroup() %>%
      mutate(
        measurements_raw_result_is_na =
          ifelse(
            need_to_subtract_one > 0,
            measurements_raw_result_is_na - 1,
            measurements_raw_result_is_na
          )
      ) %>%
      select(-need_to_subtract_one)
    
  }

#' Summarise utilization of urine protein testing
make_uprot_util_sum <- function(cohort = cdm_tbl('attrition_cohort'),
                                lab_tbl = cdm_tbl('lab_result_cm'),
                                qual_tests = load_codeset('lab_urine_protein_qual','icccc'),
                                quant_tests = load_codeset('lab_urine_protein_quant','icccc')) {

  all_pts <- cohort %>% group_by(site) %>%
    summarize(all_pts = n_distinct(patid)) %>% compute_new()
  qual_pts <- lab_tbl %>% semi_join(cohort, by = 'patid') %>%
    inner_join(qual_tests, by = c('lab_loinc' = 'concept_code')) %>%
    group_by(site) %>% summarize(qual_pts = n_distinct(patid)) %>%
    compute_new()
  quant_pts <- lab_tbl %>% semi_join(cohort, by = 'patid') %>%
    inner_join(quant_tests, by = c('lab_loinc' = 'concept_code')) %>%
    group_by(site) %>% summarize(quant_pts = n_distinct(patid)) %>%
    compute_new()

  all_pts %>% left_join(qual_pts, by = 'site') %>%
    left_join(quant_pts, by = 'site') %>%
    mutate(across(qual_pts:quant_pts,
           ~ coalesce(.x, 0L))) %>%
    mutate(qual_prop = round(1.0 * qual_pts / all_pts, 2),
           quant_prop = round(1.0 * quant_pts / all_pts, 2))
  }

#' Function to find exposures to medications (both admins and prescriptions) in a codeset
#' @param admin_tbl table following the PCORnet med_admin format
#' @param presc_tbl table following the PCORnet prescribing format
#' @param fact_codes codeset written as a list
#' @return table with both med_admins and prescriptions, with a column drug_type indicating whether originated from med_admin or prescribing table
find_drugs_d <- function(fact_codes,
                          cohort = cdm_tbl('attrition_cohort'),
                          admin_tbl = cdm_tbl('med_admin'),
                          presc_tbl = cdm_tbl('prescribing')) {
  fact_codes_reduce <-
    reduce(.x = fact_codes,
           .f = union)
  
  # medadmins
  rx_admin <- admin_tbl %>% inner_join(cohort) %>%
    inner_join(
      fact_codes_reduce,
      by = c('medadmin_code' = 'concept_code', 'medadmin_type' =
               'pcornet_vocabulary_id')
    ) %>%
    mutate(drug_end_date = if_else(
      is.na(medadmin_stop_date),
      medadmin_start_date,
      medadmin_stop_date
    )) %>%
    select(
      encounterid,
      ce_date,
      drug_code = medadmin_code,
      drug_name = concept_name,
      patid,
      site,
      category,
      drug_start_date = medadmin_start_date,
      drug_end_date
    ) %>%
    mutate(drug_type = 'med_admin') %>%
    compute_new(temporary = TRUE,
                indexes = list('patid', 'encounterid'))
  # prescriptions
  rx_presc <- presc_tbl %>% inner_join(cohort) %>%
    inner_join(fact_codes_reduce, by = c('rxnorm_cui' = 'concept_code')) %>%
    filter(rx_basis == '01') %>%
    mutate(drug_end_date = if_else(is.na(rx_end_date), rx_start_date, rx_end_date)) %>%
    select(
      encounterid,
      ce_date,
      drug_code = rxnorm_cui,
      drug_name = concept_name,
      patid,
      site,
      category,
      drug_start_date = rx_start_date,
      drug_end_date
    ) %>%
    mutate(drug_type = 'prescription') %>%
    compute_new(temporary = TRUE,
                indexes = list('patid', 'encounterid'))
  
  all_rx <- dplyr::union(rx_admin,
                         rx_presc)
}


#' Return all patients who have at least two instances of high diastolic or systolic values in a window
#' @param [diastolic_high] - Provides a threshold above which a diastolic value is considered high
#' @param [systolic_high] - Provides a threshold above which a systolic value is considered high
#' @param [period_length] - defines the length of the window for which a pair can be found for an anchor value
high_sys_or_dias_within_period <- function(diastolic_high=90,
                                           systolic_high=140, #Boundaries for bp correspond with stage 2 high blood pressure
                                           period_length=90){
  all_high_meas <- cdm_tbl('vital') %>%
    filter(diastolic>diastolic_high | systolic>systolic_high) %>%
    mutate(period_end=measure_date+as.integer(period_length)) %>%
    select(patid, measure_date, period_end, diastolic, systolic)

  person_date_tbl <- all_high_meas %>%
    inner_join(all_high_meas %>%
                 rename(measure_date_2=measure_date) %>%
                 select(patid, measure_date_2) %>%
                 distinct(),
               by='patid') %>%
    filter(between(measure_date_2,measure_date+as.integer(1),period_end)) %>%
    select(patid, measure_date) %>% distinct()

  return(person_date_tbl)
}


#' Join a codeset to a cdm table and join to a patient/cohort table by PATID
get_qualifying_patients <- function(pt_tbl=cdm_tbl('demographic'),
                                    compare_tbl=cdm_tbl('diagnosis') %>%
                                      mutate(foo = str_replace_all(raw_dx,'\\|.*',''),
                                             bar = str_replace_all(raw_dx,'.*\\|','')),
                                    compare_tbl_codeset=load_codeset('dx_hypertension'),
                                    compare_join_by=c('foo'='concept_name','bar'='concept_code')) {
  rv <- compare_tbl %>%
    inner_join(compare_tbl_codeset,by=compare_join_by) %>%
    inner_join(pt_tbl,by='patid')

  return(rv)
}


#'Make summary table for overlap in
make_intersection_summary <- function(cohort, ht_dx_tbl, ht_trmnt_tbl) {
  rv <- cohort %>%
    select(patid, site) %>%
    left_join(ht_dx_tbl %>%
                select(patid) %>%
                distinct() %>%
                mutate(htn_evidence = 1),
              by = 'patid') %>%
    left_join(ht_trmnt_tbl %>%
                select(patid) %>%
                distinct() %>%
                mutate(hypertension_trmnt = 1),
              by = 'patid') %>%
    mutate(
      htn_evidence = ifelse(is.na(htn_evidence), 0, htn_evidence),
      hypertension_trmnt = ifelse(is.na(hypertension_trmnt), 0, hypertension_trmnt),
      ht_dx_ht_trmnt = ifelse(htn_evidence + hypertension_trmnt ==
                                2, 1, 0),
      total_pts = 1
    ) %>%
    group_by(site) %>%
    summarise(
      htn_evidence = sum(htn_evidence),
      hypertension_trmnt = sum(hypertension_trmnt),
      ht_dx_and_trmnt = sum(ht_dx_ht_trmnt),
      total_pts = sum(total_pts)
    ) %>%
    ungroup() %>%
    mutate(
      percent_ht_dx_only = (htn_evidence - ht_dx_and_trmnt) / total_pts,
      percent_ht_trmt_only = (hypertension_trmnt - ht_dx_and_trmnt) /
        total_pts,
      percent_ht_trmt_and_dx = ht_dx_and_trmnt / total_pts
    ) %>%
    select(
      site,
      total_pts,
      percent_ht_dx_only,
      percent_ht_trmt_only,
      percent_ht_trmt_and_dx
    )
  return(rv)
}

categorize_efgr_days_sep <- function(egfr_table) {
  egfr_total_counts <- egfr_table %>%
    distinct() %>%
    group_by(site) %>%
    summarize(total_meas = as.numeric(n())) %>%
    ungroup() %>%
    compute_new()
  
  egfr_days_sep_cts <- egfr_table %>%
    distinct() %>%
    mutate(
      days_sep_cat = case_when(
        days_sep == 0 ~ "0",
        days_sep >= 1 &
          days_sep < 7 ~ ">=1 & < 7",
        days_sep >= 7 &
          days_sep < 30 ~ ">=7 & < 30",
        days_sep >= 30 &
          days_sep < 90 ~ ">=30 & < 90",
        days_sep >= 90 &
          days_sep < 135 ~ ">=90 & < 135",
        days_sep >= 135 &
          days_sep <= 180 ~ ">=135 & <= 180",
        TRUE ~ "not categorized"
      )
    ) %>%
    group_by(site, days_sep_cat) %>%
    summarize(n_meas = as.numeric(n())) %>%
    ungroup() %>%
    compute_new() %>%
    inner_join(egfr_total_counts, by = "site") %>%
    mutate(prop_meas = n_meas / total_meas)
  
  return(egfr_days_sep_cts)
}
