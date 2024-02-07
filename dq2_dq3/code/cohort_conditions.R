

#' function to compute side effects
#' 
#' @param codeset the codeset to identify conditions
#' @param cohort the cohort of interest
#' 
#' @return 
#' 

find_conditions <- function(codeset,
                            cohort,
                            diagnoses=cdm_tbl('diagnosis')) {
  
  all_pts <-
    cohort %>% group_by(site) %>%
    summarise(pts_all = n_distinct(patid))
  
  post_att <- 
    diagnoses %>%
    inner_join(
      codeset,
      by=c('dx'='concept_code')
    ) %>% filter(dx_type %in% c('09','10')) %>%
    inner_join(
      select(cohort, patid, ce_date),
      by='patid'
    ) %>% filter(
      coalesce(admit_date, dx_date) >= ce_date
    ) %>% group_by(site,
                   category) %>%
    summarise(pts_dx=n_distinct(patid))
  
  all_pts %>%
    inner_join(post_att,by = "site") %>%
    collect() %>%
    mutate(prop=round(pts_dx/pts_all,2))
  
}
