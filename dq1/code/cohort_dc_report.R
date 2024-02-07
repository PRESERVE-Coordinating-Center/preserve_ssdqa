#Functions for the report - move to cohort file
display_check<-function(check_name){
  if(check_name %in% dc_checks_results$checks){
    dc_check_tbl<-dc_checks_results%>%
      filter(checks %in% check_name)%>%
      select(site_name, Explanation, Additional_Information) %>% 
      rename(`Additional information` = Additional_Information)
  }else{
    checks<-'No Exceptions Found'
    dc_check_tbl<-tibble(checks)
  }
  
  dc_check_tbl#%>%
    #prettify_kable()
  
}

show_206_summary<-function(){
  
  #labs of interest for the study
  serum_creatinine<-load_codeset('serum_creatinine')
  urine_creatinine<-load_codeset('urine_creatinine')
  serum_cystatin<-load_codeset('serum_cystatin')
  urine_protein_qual<-load_codeset('urine_protein_qual')
  urine_protein_quant<-load_codeset('urine_protein_quant')
  
  
  labs_of_interest<-dplyr::union(serum_creatinine,
                                 urine_creatinine,
                                 serum_cystatin,
                                 urine_protein_qual,
                                 urine_protein_quant) %>% 
    collect()
  
  dc_206_summary <- get_results('lab_l3_loinc') %>%
    inner_join(labs_of_interest, by = c('LAB_LOINC' = 'concept_code')) %>%
    select(LAB_LOINC, concept_name, site_name, DISTINCT_PATID_N) %>%
    inner_join(n_pats, by = 'site_name') %>%
    mutate(RECORD_PCT = round(as.numeric(DISTINCT_PATID_N) / as.numeric(DISTINCT_N) *
                                100, 1)) %>%
    select(LAB_LOINC, concept_name, site_name, RECORD_PCT) %>%
    pivot_wider(names_from = site_name, values_from = RECORD_PCT) %>%
    arrange(LAB_LOINC) %>%
    prettify_kable()
  
  dc_206_summary
}

show_308_summary<-function(){
  
  presc_summary<-dplyr::union(get_results('pres_l3_rxcui_tier')%>%
                                filter(RXNORM_CUI_TIER=='Tier 1')%>%
                                mutate(measure='All Years')%>%
                                select(measure,site_name,RECORD_PCT),
                              get_results('pres_l3_rxcui_tier_5y')%>%
                                filter(RXNORM_CUI_TIER=='Tier 1')%>%
                                mutate(measure='5 Year')%>%
                                select(measure,site_name,RECORD_PCT))%>%
    collect()%>%
    pivot_wider(names_from = site_name,values_from = RECORD_PCT)%>%
    arrange(desc(measure))%>%
    prettify_kable()
  
  presc_summary
}

show_labs_summary<-function(){
  
  labs_summary<-dplyr::union(
    lab_measures%>%
      filter(TAG %in%
               c('KNOWN_TEST_RESULT_NUM'))%>%
      rename(LAB_TYPE_N=ALL_N)%>%
      left_join(select(lab_counts,site_name,DISTINCT_N),by='site_name')%>%
      group_by(site_name,TAG)%>%
      mutate(RECORD_PCT=round(as.numeric(LAB_TYPE_N)/as.numeric(DISTINCT_N)*100,1))%>%
      select(TAG,site_name,RECORD_PCT), 
    lab_measures%>%
      filter(TAG %in%
               c('KNOWN_TEST_RESULT_NUM_RANGE',
                 'KNOWN_TEST_RESULT_NUM_UNIT')
      )%>%
      rename(LAB_TYPE_N=ALL_N)%>%
      left_join(select(quant_lab_counts,site_name,ALL_N),by='site_name')%>%
      group_by(site_name,TAG)%>%
      mutate(RECORD_PCT=round(as.numeric(LAB_TYPE_N)/as.numeric(ALL_N)*100,1))%>%
      select(TAG,site_name,RECORD_PCT))%>%
    collect()%>%
    pivot_wider(names_from = site_name,values_from = RECORD_PCT)%>%
    rename(measure=TAG)%>%
    arrange(measure)%>%
    prettify_kable()
  
  labs_summary
}

show_315_summary<-function(){
  
  medadmn_summary<-dplyr::union(get_results('medadm_l3_rxcui_tier')%>%
                                  filter(RXNORM_CUI_TIER=='Tier 1')%>%
                                  mutate(measure='All Years')%>%
                                  select(measure,site_name,RECORD_PCT),
                                get_results('medadm_l3_rxcui_tier_5y')%>%
                                  filter(RXNORM_CUI_TIER=='Tier 1')%>%
                                  mutate(measure='5 Year')%>%
                                  select(measure,site_name,RECORD_PCT))%>%
    collect()%>%
    pivot_wider(names_from = site_name,values_from = RECORD_PCT)%>%
    arrange(desc(measure))%>%
    prettify_kable()
  
  medadmn_summary
}