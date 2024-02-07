


#' computes total number of nephrology
#' and non-nephrology visits
#'
#' @param cohort the cohort of interest
#'
#' @return a database tbl with the following columns:
#'
#' `site` | `patid` | `sc` | `ce_on_after` | `ce_prior`
#'
#' This table counts specialty visits prior to, or on and after
#' cohort entry date
#'

compute_num_neph <- function(cohort=cdm_tbl('attrition_cohort')){


  visit_list <-
    compute_all_specialties()

  site_list <-
    cdm_tbl('encounter') %>%
    select(site) %>% distinct() %>% pull()


  output_list <- list()

  for(i in 1:length(site_list)) {

    site_nm <- site_list[[i]]

    unique_list <-
      dplyr::union(visit_list[[1]] %>% filter(site==site_nm) %>%
                     rename(sc=provider_sc) %>%
                     select(site,
                            patid,
                            enc_type,
                            admit_date,
                            sc),
                   visit_list[[2]] %>% filter(site==site_nm) %>%
                     rename(sc=factype_sc) %>%
                     select(site,
                            patid,
                            enc_type,
                            admit_date,
                            sc))  %>%
      dplyr::union(visit_list[[3]] %>% filter(site==site_nm) %>%
                     rename(sc=rawfac_sc) %>%
                     select(site,
                            patid,
                            enc_type,
                            admit_date,
                            sc)) %>%
      inner_join(cohort,
                by=c('patid', 'site')) %>%
      mutate(enc_timing=
               case_when(
                 admit_date < ce_date ~ 'ce_prior',
                 admit_date >= ce_date ~ 'ce_on_after',
                 TRUE ~ 'ce_none'
               )) %>%
      group_by(
        site,
        patid,
        sc,
        enc_timing
      ) %>% summarise(total_visits=n_distinct(admit_date)) %>%
      compute_new(temporary=TRUE)

    output_list[[i]] <- unique_list

  }

  reduce(.x=output_list,
         .f=dplyr::union) %>%
    pivot_wider(names_from=enc_timing,
                values_from=total_visits) %>%
    mutate(., across(contains('ce'), ~ifelse(is.na(.x),0,.x)))
}



#' computes number of urine protein labs ever (not tied to CE date)
#'
#' @param urine_labs codeset for both quantitative and qualitative labs
#' @param lab_tbl the lab results tbl
#' @param cohort the cohort for patients
#'
#' @return a database tbl with 4 columns:
#' `site` | `patid` | `total_labs` | `ce_date`
#'

compute_urine_protein <- function(urine_labs=dplyr::union(load_codeset('lab_urine_protein_qual','icccc'),
                                                          load_codeset('lab_urine_protein_quant','icccc')),
                                  lab_tbl=cdm_tbl('lab_result_cm'),
                                  cohort=cdm_tbl('attrition_cohort')) {

  labs <-
    lab_tbl %>%
    inner_join(cohort) %>%
    inner_join(
      select(urine_labs,
             concept_code),
      by=c('lab_loinc'='concept_code')
    ) %>% mutate(event_date=
                   case_when(ce_date < specimen_date ~ 'ce_on_after',
                             ce_date >= specimen_date ~ 'ce_prior')) %>%
    group_by(site,
             patid,
             event_date) %>%
    summarise(total_labs=n_distinct(encounterid))


}

#' computes number of antihypertensive meds
#'
#' @param drugs_rx a database tbl with all drugs of interest combined
#' @param drug_tbl the drug tbl to count - defaults to prescribing tbl
#' @param cohort the cohort of interest
#'
#' @return a database tbl with the following columns:
#'
#' `site` | `patid` | `total_drugs` | `ce_date`
#'

compute_drugs_rx <- function(drugs_rx=dplyr::union(load_codeset('rx_ace_inhibitor','icccccc'),
                                                   load_codeset('rx_arb','icccccc')) %>%
                               dplyr::union(load_codeset('rx_bb','icccccc')) %>%
                               dplyr::union(load_codeset('rx_ccb','icccccc')) %>%
                               dplyr::union(load_codeset('rx_thiazide','icccccc')) %>%
                               dplyr::union(load_codeset('rx_loop_diuretic','icccccc')),
                                  drug_tbl=cdm_tbl('prescribing'),
                                  cohort=cdm_tbl('attrition_cohort')) {

  rx <-
    drug_tbl %>%
    inner_join(cohort) %>%
    inner_join(
      select(drugs_rx,
             concept_code),
      by=c('rxnorm_cui'='concept_code')
    ) %>% mutate(event_date=
                   case_when(ce_date <= rx_start_date ~ 'ce_on_after',
                             ce_date > rx_start_date ~ 'ce_prior')) %>%
    group_by(site,
             patid,
             event_date) %>%
    summarise(total_drugs=n_distinct(encounterid)) %>%
    compute_new(temporary=TRUE)


}


#' computes number of BP measurements
#'
#' @param vital_tbl the tbl that contains BP measurements
#' @param cohort
#'
#' @return a database tbl that contains the following columns:
#'
#' `site` | `patid` | `total_bp` | `ce_date`
#'

compute_bp_sys <- function(vital_tbl=cdm_tbl('vital'),
                           cohort=cdm_tbl('attrition_cohort')) {

  site_list <-
    vital_tbl%>%
    select(site) %>% distinct() %>% pull()

  site_output <- list()

  for(i in 1:length(site_list)) {

    site_nm <- site_list[[i]]

    bp <-
      vital_tbl %>%
      inner_join(cohort) %>%
      filter(!is.na(systolic)) %>%
      mutate(event_date=
             case_when(ce_date <= measure_date ~ 'ce_on_after',
                       ce_date > measure_date ~ 'ce_prior')) %>%
      group_by(site,
               event_date,
              patid) %>%
      summarise(total_bp=n_distinct(encounterid))

    site_output[[i]] <- bp
  }

  reduce(.x=site_output,
         .f=dplyr::union)

}


#' median egfr values before anda fter ce
#'
#' @param egfr_tbl
#' @param cohort
#'
#'

compute_median_egfr <- function(egfr_tbl = results_tbl('egfr_bounded'),
                                demographic = cdm_tbl('demographic')) {
  egfr_tbl %>%
    inner_join(select(demographic,
                      patid,
                      site)) %>%
    mutate(
      event_date =
        case_when(
          ce_date < comb_lab_date ~ 'ce_on_after',
          ce_date >= comb_lab_date ~ 'ce_prior'
        )
    ) %>%
    group_by(site,
             event_date,
             patid) %>%
    summarise(patid_med = median(egfr)) %>%
    ungroup()
  
}

compute_median_egfr_washout <- function(egfr_tbl = results_tbl('egfr_bounded'),
                                demographic = cdm_tbl('demographic')) {
  egfr_tbl %>%
    inner_join(select(demographic,
                      patid,
                      site)) %>%
    mutate(washout = ifelse(comb_lab_date - ce_date >= -180 & comb_lab_date - ce_date < 0, "yes", "no")) %>%
    filter(washout == "no") %>%
    mutate(
      event_date =
        case_when(
          ce_date <= comb_lab_date ~ 'ce_on_after',
          ce_date > comb_lab_date ~ 'ce_prior'
        )
    ) %>%
    group_by(site,
             event_date,
             patid) %>%
    summarise(patid_med = median(egfr)) %>%
    ungroup()
  
}



#' median egfr values before anda fter ce
#'
#' @param egfr_tbl
#' @param cohort
#'
#'

compute_site_median_egfr <- function(egfr_tbl=results_tbl("egfr_bounded"),
                                     demographic=cdm_tbl('demographic')) {

 # fin <-
    egfr_tbl %>%
    inner_join(select(demographic,
                      patid,
                      site)) %>%
    mutate(event_date=
             case_when(ce_date <= comb_lab_date ~ 'ce_on_after',
                       ce_date > comb_lab_date ~ 'ce_prior')) %>%
    mutate(meas_year=sql('extract(year from comb_lab_date)')) %>%
    #collect() %>%
    group_by(site,
             patid,
             event_date,
             meas_year) %>%
    summarise(patid_median =median(egfr)) %>%
    ungroup() %>% collect() %>%
  group_by(site,
           event_date,
           meas_year) %>%
  summarise(site_median=median(patid_median))

}

