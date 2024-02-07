

#' all_specialty check
#' 
#' Lookup table for provider specialty data
#' 
#' @param provider_tbl the provider tbl that contains the specialty 
#' @param specialty_xwalk specialty crosswalk 
#' 
#' @return A database tbl with the following columns:
#' `provider_specialty_primary` | `raw_provider_specialty` | `specialty_type` | `specialty_class`
#' 
#' This table will categorize specifically the nephrology, cariology, urology, and heme onc 
#' specialty providers. Others will be calssified as `other_specialty` and if unknow, the 
#' value will be `not_available`.
#' 

find_prov_specialties <- function(provider_tbl=cdm_tbl('provider'),
                             specialty_xwalk=read_codeset('specialty_codes','cccc') %>%
                               copy_to_new(df=.)) {
  
  valid_specs_prov <- 
    provider_tbl %>%
    inner_join(
      specialty_xwalk,
      by=c('provider_specialty_primary'='code')
    ) %>% mutate(
      specialty_type='provider'
    ) %>% mutate(
      specialty_class=case_when(
        provider_specialty_primary %in% c('NI','OT') ~ 'not_available',
        provider_specialty_primary %in% c('207QA0000X','207Q00000X','208000000X',
                                          '2080A0000X','163WP0200X','2086S0120X',
                                          '363LF0000X','364SF0001X','363LP0200X', '364SP0200X')  ~ 'family_med_or_peds',
        provider_specialty_primary %in% c('207RN0300X','2080P0210X',
                                          '246ZN0300X','163WN0300X') ~ 'nephrology',
        provider_specialty_primary %in% c('2080P0202X','207RC0000X','246W00000X',
                                          '246X00000X','246XC2901X','246XC2903X',
                                          '2471C1101X','163WC3500X','207RA0001X',
                                          '207RC0001X','207RI0011X','207UN0901X') ~ 'cardiology',
        provider_specialty_primary %in% c('207RH0000X','207ZH0000X', '207RH0003X',
                                          '207RX0202X','2086X0206X', '2080P0207X',
                                          '2085R0001X','246QH0000X', '261QX0203X',
                                          '364SX0200X','364SX0204X', '163WP0218X',
                                          '163WX0200X') ~ 'heme_onc',
        provider_specialty_primary %in% c('2088P0231X','208800000X','163WU0100X') ~ 'urology',
        TRUE ~ 'other_specialty'
      )
    ) %>% select(provider_specialty_primary,
                 raw_provider_specialty_primary,
                 specialty_type,
                 specialty_class) %>% distinct()
  
  
  
  
}


#' Finds all provider specialties based 
#' on categorization from `provider_xwalk`
#' 
#' @param provider_xwalk the output from `find_prov_specialties`
#' @return a database tbl with the following columns:
#' 
#' `site` | `patid` | `encounterid` | `admit_date` | `enc_type` | `provider_sc`
#' 

find_provider_visits <- function(provider_xwalk,
                                 provider_tbl=cdm_tbl('provider'),
                                 encounter_tbl=cdm_tbl('encounter')) {
  
  
  provider_notraw <- 
    provider_xwalk %>%
    select(provider_specialty_primary,
           specialty_type,
           specialty_class) %>% distinct()
  
  prov_type <- 
    encounter_tbl %>%
    inner_join(select(
      provider_tbl,
      providerid,
      provider_specialty_primary
    )) %>% 
    inner_join(provider_notraw) %>%
   # filter(!specialty_class == 'other_specialty') %>%
    select(site,
           patid,
           encounterid,
           admit_date,
           enc_type,
           specialty_class) %>%
    rename(provider_sc=specialty_class)

}


#' Find facility_type specialties based on column `facility_type` in encounter table
#' New specialty value added by PCORnet for nephrology
#'
#' @param encounter_tbl the pcornet tbl that has all encounters for the patients
#' in the cohort
#' 
#' @return Table with encounters and the categorized specialty facility. Columns include:
#' 
#' `site` | `patid` | `encounterid` | `admit_date` | `enc_type` | `factype_sc`
#' 

find_factype_visits <- function(encounter_tbl=cdm_tbl('encounter')) {
  
  
   fac_type <- 
    encounter_tbl %>%
   # mutate(specialty_type='facility_fact') %>%
    mutate(factype_sc=case_when(
      facility_type %in% c('HOSPITAL_OUTPATIENT_PEDIATRIC_NEPHROLOGY_CLINIC') ~ 'nephrology',
      facility_type %in% c('HOSPITAL_OUTPATIENT_PEDIATRIC_UROLOGY_CLINIC','HOSPITAL_OUTPATIENT_UROLOGY_CLINIC') ~ 'urology',
      facility_type %in% c('HOSPITAL_OUTPATIENT_PEDIATRIC_HEMATOLOGY_ONCOLOGY_CLINIC','HOSPITAL_OUTPATIENT_ONCOLOGY_CLINIC') ~ 'heme_onc',
      facility_type %in% c('HOSPITAL_OUTPATIENT_PEDIATRIC_CARDIOLOGY_CLINIC','HOSPITAL_OUTPATIENT_CARDIOLOGY_CLINIC') ~ 'cardiology',
      TRUE ~ 'other_specialty'
    )) %>% 
    select(site,
           patid,
           encounterid,
           admit_date,
          enc_type,
          factype_sc)
  
  
  
}


#' find raw specialty
#' 
#' Interrogates raw field for facility
#' 
#' @param encounter_tbl the pcornet tbl that has all encounters for the patients
#' in the cohort
#' 
#' @return Table with encounters and the categorized specialty facility. Columns include:
#' 
#' `site` | `patid` | `encounterid` | `admit_date` | `enc_type` | `rawfac_sc`
#' 

find_rawfac_visits <- function(encounter_tbl=cdm_tbl('encounter')) {
  
 
  
  raw_fac_type <- 
    encounter_tbl %>%
   #mutate(specialty_type='facility_raw') %>%
    mutate(rawfac_sc=case_when(
      raw_facility_type %in% c('|207RN0300X','|2080P0210X', 
                               '|246ZN0300X','|163WN0300X') ~ 'nephrology',
      facility_type %in% c('|2080P0202X','|207RC0000X') ~ 'cardiology',
      facility_type %in% c('|207RH0000X','|207ZH0000X',
                           '|207RX0202X','|2086X0206X',
                           '|2085R0001X') ~ 'heme_onc',
      facility_type %in% c('|2088P0231X','|208800000X') ~ 'urology',
      TRUE ~ 'other_specialty'
    )) %>% 
    select(site,
           patid,
           encounterid,
           admit_date,
           enc_type,
           rawfac_sc)
  
  
}


#' computes provider, facility type, and raw facility visits:
#' uses functions `find_prov_specialties`, `find_provider_visits`,
#' `find_factype_visits`, and `find_rawfac_visits`
#' 
#' @return A 3-element list with the output 
#' from the functions listed above (except for the finding of the 
#' provider specialties)
#' 
#' 

compute_all_specialties <- function() {
  
  providers <- 
    find_prov_specialties() 
  provider_visits <- 
    find_provider_visits(providers) 
  factype_visits <- 
    find_factype_visits() 
  rawfact_visits <- 
    find_rawfac_visits() 
  
  
  specialty_list <- list(provider_visits,
                         factype_visits,
                         rawfact_visits)
  
  
}


#' Compute specialist visits based on categorizations above of provider/facility specialty
#' Will look for any specialty categorization and groups output based on encounter type,
#' before/after cohort entry date, and the specialty category
#' 
#' @param visit_list a list of visits, the output of `compute_all_specialties`
#' @param attrition_cohort the attrition cohort that has the cohort entry date
#' @param demog_tbl the demographic tbl
#' 
#' @return A local dataframe with the following columns:
#' 
#' `site` | `enc_type` | `enc_timing` | `visits_total` | `sc` | `visits_strat` | `pts_total` | `visits_total_per_pt` | `visits_strat_per_pt`
#' 
#' 
#' 

compute_anyspec_summary <- function(visit_list,
                                    attr_cohort=cdm_tbl('attrition_cohort'),
                                    demog_tbl=cdm_tbl('demographic')) {
  
  
  site_list <- 
    cdm_tbl('encounter') %>%
    select(site) %>% distinct() %>% pull() 
  
  rslts <- list()
  
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
      inner_join(attr_cohort,
                by=c('patid', 'site')) %>%
      mutate(enc_timing=
               case_when(
                 admit_date < ce_date ~ 'ce_prior',
                 admit_date >= ce_date ~ 'ce_on_after',
                 TRUE ~ 'ce_none'
               )) 
    
    summarized <- 
      unique_list %>%
      group_by(site,
               enc_type,
               enc_timing) %>%
      summarize(visits_total=n_distinct(admit_date)) %>% collect()
    
    summarized_type <- 
      unique_list %>%
      group_by(site,
               enc_type,
               enc_timing,
               sc) %>%
      summarize(visits_strat=n_distinct(admit_date)) %>% collect()
    
    total_pts <- 
      attr_cohort %>% 
      select(patid) %>%
      inner_join(
        select(demog_tbl,
               patid,
               site),
        by='patid'
      ) %>% group_by(site) %>%
      summarise(pts_total=n_distinct(patid)) %>% collect()
    
    output <- 
      summarized %>%
      left_join(summarized_type,
                by=c('site','enc_type','enc_timing')) %>%
      left_join(total_pts,
                by='site') %>%
      mutate(visits_total_per_pt=visits_total/pts_total) %>%
      mutate(visits_strat_per_pt=visits_strat/pts_total)
    
    rslts[[i]] <- output 
    
    
  }
  
  
  reduce(.x=rslts,
         .f=dplyr::union)
  
  
}