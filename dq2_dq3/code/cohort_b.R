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

## ip_fu_active
### For each year of cohort entry, calculate the cumulative sum of patients in the cohort by site
get_cohort_entries <- function(cohort, demographic = cdm_tbl('demographic')){
  cohort %>% left_join(select(demographic, patid, site)) %>%
    mutate(year = year(ce_date)) %>%
    group_by(site, year) %>%
  summarise(site_year_total = n_distinct(patid)) %>%
    window_order(year) %>%
    group_by(site) %>%
    mutate(site_total = cumsum(site_year_total))
}


### For each year and site, calculate the number and percent of patients with at least one encounter
get_with_visit <- function(cohort, encounter = cdm_tbl('encounter'), cohort_entries = results_tbl('cohort_entries')){
  encounter %>% inner_join(cohort) %>%
    filter(admit_date >= ce_date) %>%
    mutate(year = year(admit_date)) %>%
    filter(year < 2025) %>%
    group_by(site, year) %>%
    summarise(patients = n_distinct(patid)) %>%
    inner_join(cohort_entries, by=c('site','year')) %>%
    mutate(percent_patients = (patients / site_total)*100)
  }


visit_type_classification <- function(df) {
  df %>%
    mutate(
      visit_type = case_when(
        enc_type == 'AV' | enc_type == 'OA' ~ "outpatient",
        enc_type == 'ED' ~ "emergency",
        enc_type == 'EI' ~ "emergency->inpatient",
        enc_type == 'TH' ~ "telehealth",
        enc_type == 'IP' ~ "inpatient",
        TRUE ~ "other"
      )
    )
}

## visits_temporal

### Of the patients with at least one encounter, find the percent that had outpatient, inpatient, or ED visits by year and by site
get_visit_types <-
  function(cohort,
           encounter = cdm_tbl('encounter'),
           with_visit = results_tbl('with_visits')) {
    encounter %>% inner_join(cohort) %>%
      filter(admit_date >= ce_date) %>%
      mutate(year = year(admit_date)) %>%
      filter(year < 2025)  %>%
      visit_type_classification() %>% 
      group_by(site, year, visit_type) %>%
      summarise(patients_w_visits = n_distinct(patid)) %>%
      inner_join(select(with_visit, site, year, patients), by = c('site', 'year')) %>%
      mutate(percent_patients = (as.numeric(patients_w_visits) / as.numeric(patients)) *
               100)
  }


### Median number of encounters per person by year and by site (after cohort entry)
get_visits_per_person <- function(cohort, encounter = cdm_tbl('encounter')){
  encounter %>% inner_join(cohort) %>%
    filter(admit_date >= ce_date) %>%
  mutate(year = year(admit_date)) %>%
    filter(year < 2025) %>%
    visit_type_classification() %>% 
    group_by(site, year, patid, visit_type) %>%
    summarise(visits = n_distinct(encounterid)) %>%
  group_by(site, year, visit_type) %>%
    summarise(median_visits = median(visits))
}


## visits_nofacts
### Total number of encounters by year and by site for patients in the cohort
get_visits <- function(cohort, encounter = cdm_tbl('encounter')){
  encounter %>% inner_join(cohort) %>%
    filter(admit_date >= ce_date) %>%
  mutate(year = year(admit_date)) %>%
    filter(year < 2025) %>%
    group_by(site, year) %>%
    summarise(visits = n_distinct(encounterid))
}


### Number of encounters not associated with facts from other tables
get_nofacts <- function(cohort, encounter = cdm_tbl('encounter'), visits = results_tbl('visits'), table1, table2, table3, table4, table5){
  encounter %>% inner_join(cohort) %>%
    filter(admit_date >= ce_date) %>%
    visit_type_classification() %>% 
  anti_join(select(table1, encounterid)) %>%
    anti_join(select(table2, encounterid)) %>%
  anti_join(select(table3, encounterid)) %>%
    anti_join(select(table4, encounterid)) %>%
    anti_join(select(table5, encounterid)) %>%
  mutate(year = year(admit_date)) %>%
    filter(year < 2025) %>%
    group_by(site, year, visit_type) %>%
    summarise(nofacts = n_distinct(encounterid)) %>%
  left_join(visits, by=c('site', 'year')) %>%
    mutate(percent_nofacts = (as.numeric(nofacts) / as.numeric(visits)) *100)
}



## ip_fu_facts
### Median number of patient facts per year, stratified by year, site, and PCORnet CDM table
library(lubridate)
get_facts_per_person <- function(cohort, domain, domain_date){
domain %>%
  inner_join(cohort) %>%
    rename(use_date = domain_date) %>%
  mutate(year = year(use_date)) %>%
    filter(year >= 2009) %>%
    group_by(site, year, patid) %>%
  summarise(n = n()) %>%
    group_by(site, year) %>%
    summarise(median_per_person = median(n))
}



## provider_facility
### Percent of encounters that are nephrology provider specialty, nephrology facility specialty, both, or neither

get_nephrology_encounters <-
  function(cohort,
           encounter = cdm_tbl('encounter'),
           provider = cdm_tbl('provider')) {
    encounter %>%
      visit_type_classification() %>% 
      inner_join(cohort) %>%
      left_join(select(provider, providerid, provider_specialty_primary),
                by = 'providerid') %>%
      mutate(
        encounter_type = case_when(
          toupper(provider_specialty_primary) %in% c('2080P0210X', '163WN0300X', '207RN0300X', '246ZN0300X') &
            toupper(facility_type) == 'HOSPITAL_OUTPATIENT_PEDIATRIC_NEPHROLOGY_CLINIC' ~ 'both',
          toupper(provider_specialty_primary) %in% c('2080P0210X', '163WN0300X', '207RN0300X', '246ZN0300X') ~ 'nephrology_provider',
          toupper(facility_type) == 'HOSPITAL_OUTPATIENT_PEDIATRIC_NEPHROLOGY_CLINIC' ~ 'nephrology_facility',
          TRUE ~ 'neither'
        )
      ) %>% group_by(site, visit_type, encounter_type) %>% summarise(n = n_distinct(encounterid))
  }


### Total number of visits per year by site
get_visits_per_year <- function(cohort, encounter = cdm_tbl('encounter')){
  encounter %>%
    visit_type_classification() %>%
  inner_join(cohort) %>%
  mutate(year = year(admit_date)) %>%
    filter(year >= 2009) %>%
    group_by(site, visit_type, year) %>%
  summarise(total_visits = n_distinct(encounterid))
}

### Of all visits, find the percent associated with a nephrology provider by year and by site
get_neph_providers <-
  function(cohort,
           encounter = cdm_tbl('encounter'),
           provider = cdm_tbl('provider'),
           visits_per_year = results_tbl('visits_per_year')) {
    encounter %>%
      visit_type_classification() %>%
      inner_join(cohort) %>%
      left_join(select(provider, providerid, provider_specialty_primary),
                by = 'providerid') %>%
      mutate(year = year(admit_date)) %>%
      filter(
        year >= 2009,
        toupper(provider_specialty_primary) %in% c("163WN0300X", "207RN0300X", "2080P0210X", "246ZN0300X")
      ) %>%
      group_by(site, visit_type, year) %>%
      summarise(nephrology_provider = n_distinct(encounterid)) %>%
      left_join(visits_per_year) %>%
      mutate(percent_nephrology_provider = (as.numeric(nephrology_provider) /
                                              as.numeric(total_visits)) * 100)
  }

### Of all visits, find the percent associated with a nephrology provider by year and by site
get_neph_clinics <-
  function(cohort,
           encounter = cdm_tbl('encounter'),
           visits_per_year = results_tbl('visits_per_year')) {
    encounter %>%
      visit_type_classification() %>%
      inner_join(cohort) %>%
      mutate(year = year(admit_date)) %>%
      filter(
        year >= 2009,
        toupper(facility_type) == 'HOSPITAL_OUTPATIENT_PEDIATRIC_NEPHROLOGY_CLINIC'
      ) %>%
      group_by(site, visit_type, year) %>%
      summarise(nephrology_clinic = n_distinct(encounterid)) %>%
      left_join(visits_per_year) %>%
      mutate(percent_nephrology_clinic = (as.numeric(nephrology_clinic) /
                                            as.numeric(total_visits)) * 100)
  }

## Facility
### Number of encounters by nephrology clinic id
get_clinics <- function(cohort, encounter = cdm_tbl('encounter')) {
  encounter %>%
    visit_type_classification() %>%
    inner_join(cohort) %>%
    filter(toupper(facility_type) == 'HOSPITAL_OUTPATIENT_PEDIATRIC_NEPHROLOGY_CLINIC') %>%
    group_by(site, visit_type, facilityid) %>%
    summarise(number_encounters = n_distinct(encounterid))
}

