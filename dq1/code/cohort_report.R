add_site_labels <- function(data,
                            xwalk=site_report_config) {
  
  data <-
    
    if (params$mask_sites && any(tbl_vars(data) == 'site_name')) {
      data_final <-
        left_join(collect(data),
                  xwalk) %>% select(-c(site_name)) %>%
        rename(site_name = site_mask) %>%
        arrange(site_name)
    } else {
      data_final <-
        left_join(collect(data),
                  xwalk) %>% select(c(-site_mask))
      
    }
  data_final
  
}

get_results <- function(tbl_name) {
  if (data_source == 'local') {
    rslt <- read_csv(paste0('../results/', tbl_name, '.csv'))
  }
  else {
    rslt <- results_tbl(tbl_name) %>% collect()
  }
  rslt %>%
    add_site_labels() %>% 
    arrange(site_name)
}
prettify_kable <- function(data,
                           table_caption = "add a caption") {
  data %>% kable(digits = 2, format.args = list(big.mark = ','), caption = table_caption) %>%
    kable_styling(bootstrap_options = c("striped", "hover")) %>%
    column_spec(1, bold = T, border_right = T)
}

preserve_feas_plot <- function(){
  theme_minimal() +
    theme(legend.position = "none")
}

cht_overlap_plot <- function(data) {
  data %>% ggplot(aes(
    y = site_name,
    x = counts,
    alpha = forcats::fct_rev(cohorts),
    fill = site_colour
  )) +
    geom_col(position = position_fill(), linetype = "solid") +
    # scale_fill_grey(start = 0.8, end = 0.4) +
    scale_fill_identity() +
    scale_alpha_manual(values = c(0.4, 0.65, 1)) +
    preserve_feas_plot() +
    scale_x_continuous(labels = scales::comma) +
    theme(legend.position = "bottom", legend.key.width = unit(1.5, "cm")) +
    ylab("Site") +
    xlab("Proportion union of both cohorts") +
    labs(alpha = "Cohort overlap")
}

get_cohort_count_summary <- function(attrition_table,
                                     count_table) {
  step3_counts <- attrition_table %>%
    filter(step_num == 3) %>%
    mutate(step3 = counts) %>%
    select(site_name, step3)

  cht_counts <- count_table %>%
    group_by(site_name, site_colour) %>%
    summarize(counts = sum(n_patids)) %>%
    ungroup() %>%
    # suppress small cell sizes
    mutate(counts = if_else(counts > 0 &
                              counts < 11, 10, counts)) %>% 
    full_join(step3_counts, by = "site_name") %>%
    mutate(percent_step3 = 100 * (counts / step3)) %>%
    select(site_name, site_colour, counts, percent_step3) %>%
    arrange(site_name)

  cht_plot <- cht_counts %>%
    ggplot(aes(
      x = site_name,
      y = percent_step3,
      label = scales::comma(counts,
                            accuracy = 1)
    )) +
    geom_col(alpha = 0.75, position = position_dodge()) +
    preserve_feas_plot() +
    scale_fill_grey() +
    # geom_hline(yintercept = mean(cht_counts$percent_step3),
    #            linetype = "dashed") +
    xlab("Site") +
    ylab("Percentage step 3") +
    scale_y_continuous(labels = scales::comma) +
    labs(caption = "Raw counts label each bar") +
    geom_label(fill = "white")

  return(list(cht_plot,
              cht_counts %>% select(-site_colour)))
}

# DEMOGRAPHIC	RACE	01	01=American  Indian  or  Alaska  Native
# DEMOGRAPHIC	RACE	02	02=Asian
# DEMOGRAPHIC	RACE	03	03=Black  or  African  American
# DEMOGRAPHIC	RACE	04	04=Native  Hawaiian  or  Other  Pacific  Islander
# DEMOGRAPHIC	RACE	05	05=White
# DEMOGRAPHIC	RACE	06	06=Multiple  race
# DEMOGRAPHIC	RACE	07	07=Refuse  to  answer
# DEMOGRAPHIC	RACE	NI	NI=No  information
# DEMOGRAPHIC	RACE	UN	UN=Unknown
# DEMOGRAPHIC	RACE	OT	OT=Other
parse_race <- function(tbl) {
  tbl %>% mutate(
    RACE = case_when(
      # RACE == "01" ~ "American  Indian  or  Alaska  Native",
      RACE == "02" ~ "Asian",
      RACE == "03" ~ "Black  or  African  American",
      # RACE == "04" ~ "Native  Hawaiian  or  Other  Pacific  Islander",
      RACE == "05" ~ "White",
      RACE == "06" ~ "Multiple  race",
      RACE == "01" | RACE == "04" ~ "American  Indian  or  Alaska  Native/
      Native  Hawaiian  or  Other  Pacific  Islander"
      RACE == "07" | RACE == "OT" | RACE == "NI" | RACE == "UN" ~
        "Refuse  to  answer / Other / No  information / Unknown",
      TRUE ~ "Not Mapped"
    )
  )
}


# DEMOGRAPHIC	HISPANIC	Y	Y=Yes
# DEMOGRAPHIC	HISPANIC	N	N=No
# DEMOGRAPHIC	HISPANIC	R	R=Refuse  to  answer
# DEMOGRAPHIC	HISPANIC	NI	NI=No  information
# DEMOGRAPHIC	HISPANIC	UN	UN=Unknown
# DEMOGRAPHIC	HISPANIC	OT	OT=Other
parse_ethnicity <- function(tbl) {
  tbl %>% mutate(
    HISPANIC = case_when(
      HISPANIC == "Y" ~ "Yes",
      HISPANIC == "N" ~ "No",
      HISPANIC == "R" | HISPANIC == "OT" | HISPANIC == "NI" | HISPANIC == "UN" ~
        "Refuse to answer / Other / No  information / Unknown",
      TRUE ~ "Not Mapped"
    )
  )
}

# ENCOUNTER	ENC_TYPE	AV	AV=Ambulatory  Visit
# ENCOUNTER	ENC_TYPE	ED	ED=Emergency  Department
# ENCOUNTER	ENC_TYPE	EI	EI=Emergency  Department  Admit  to  Inpatient  Hospital  Stay  (permissible  substitution)
# ENCOUNTER	ENC_TYPE	IP	IP=Inpatient  Hospital  Stay
# ENCOUNTER	ENC_TYPE	IS	IS=Non-Acute  Institutional  Stay
# ENCOUNTER	ENC_TYPE	OS	OS=Observation  Stay
# ENCOUNTER	ENC_TYPE	IC	IC=Institutional  Professional  Consult  (permissible  substitution)
# ENCOUNTER	ENC_TYPE	TH	TH=Telehealth
# ENCOUNTER	ENC_TYPE	OA	OA=Other  Ambulatory  Visit
# ENCOUNTER	ENC_TYPE	NI	NI=No  information
# ENCOUNTER	ENC_TYPE	UN	UN=Unknown
# ENCOUNTER	ENC_TYPE	OT	OT=Other
parse_enc_type <- function(tbl) {
  tbl %>% mutate(
    enc_type = case_when(
      ENC_TYPE == "AV" ~ "Ambulatory  Visit",
      ENC_TYPE == "ED" ~ "Emergency  Department",
      ENC_TYPE == "EI" ~ "Emergency  Department  Admit  to  Inpatient  Hospital  Stay",
      ENC_TYPE == "IP" ~ "Inpatient  Hospital  Stay",
      ENC_TYPE == "OS" ~ "Observation  Stay",
      ENC_TYPE == "IC" ~ "Institutional  Professional  Consult",
      ENC_TYPE == "TH" ~ "Telehealth",
      ENC_TYPE == "OA" ~ "Other  Ambulatory  Visit",
      ENC_TYPE == "NI" ~ "No  information",
      ENC_TYPE == "UN" ~ "Unknown",
      ENC_TYPE == "OT" ~ "Other",
      TRUE ~ "Not Mapped"
    )
  )
}

get_codeset_summary <- function(codeset_ct_tbl,
                                denom = enc_type_cts,
                                codeset_name,
                                ylim_min = 0,
                                ylim_max = 50,
                                colours = site_colours,
                                linetypes = site_linetypes) {
  denom <- denom %>%
    filter(enc_type == 'AV') %>%
    group_by(site_name,
             year) %>%
    summarize(n_patids_denom = sum(n_patids)) %>%
    ungroup()
  
  codeset_ct_tbl <- codeset_ct_tbl %>%
    filter(codeset == codeset_name) %>%
    group_by(site_name,
             site_colour,
             site_linetype,
             prefix,
             year) %>%
    summarize(n_patids = sum(n_patids),
              n_rows = sum(n_rows)) %>%
    ungroup()
  
  cts_plot <- codeset_ct_tbl %>%
    full_join(denom, by = c("site_name", "year")) %>%
    filter(!is.na(prefix)) %>%
    mutate(
      table = case_when(
        prefix == "med" ~ "MED_ADMIN",
        prefix == "pres" ~ "PRESCRIBING",
        prefix == "disp" ~ "DISPENSING",
        prefix == "lab" ~ "LAB_RESULT_CM",
        prefix == "proc" ~ "PROCEDURE",
        prefix == "cond" ~ "CONDITION",
        prefix == "diag" ~ "DIAGNOSIS"
      ),
      pct_patids_denom = 100 * (n_patids / n_patids_denom)
    ) %>%
    ggplot(aes(x = year,
               y = pct_patids_denom,
               colour = site_name,
               linetype = site_name)) +
    geom_line(size = 0.6) +
    preserve_feas_plot() +
    scale_color_manual(values = colours) +
    scale_linetype_manual(values = linetypes) +
    theme(legend.position = "bottom",
          legend.key.width = unit(1, "cm")) +
    xlab("Calendar year") +
    ylab("Percent patients") +
    facet_wrap( ~ table) +
    xlim(2009, 2021) +
    ylim(ylim_min, ylim_max)
  
  return(cts_plot)
  
}

get_vitals_cts_summary <- function(vitals_ct_tbl,
                                   denom = enc_type_cts,
                                   codeset_name) {

  vitals_ct_tbl <- vitals_ct_tbl %>%
    group_by(year, site_name, site_colour, site_linetype) %>%
    summarize(n_patids = sum(n_patids),
              n_rows = sum(n_rows)) %>%
    ungroup()
  
  denom <- denom %>%
    filter(ENC_TYPE == 'AV') %>%
    group_by(site_name,
             year) %>%
    summarize(n_patids_denom = sum(n_patids)) %>%
    ungroup()
  
  vitals_ct_tbl %>%
    full_join(denom, by = c("site_name", "year")) %>%
    mutate(meas_per_patid = 100 * (n_rows / n_patids)) %>%
    ggplot(aes(
      x = year,
      y = meas_per_patid,
      colour = site_name,
      linetype = site_name
    )) +
    geom_line(size = 0.6) +
    geom_point(size = 1) +
    preserve_feas_plot() +
    scale_color_manual(values = site_colours,                      breaks = attrition %>% distinct(site_name) %>% pull()) +
    scale_linetype_manual(values = site_linetypes,                      breaks = attrition %>% distinct(site_name) %>% pull()) +
    theme(legend.position = "bottom", legend.key.width = unit(1.5, "cm")) +
    scale_y_continuous(labels = scales::comma) +
    xlab("Calendar year") +
    ylab("Number of meas. per patient per year") +
    xlim(2009, 2021)
}

get_vitals_vals_summary <- function(vitals_vals_tbl) {
  
  vitals_vals_plot1 <- vitals_vals_tbl %>%
    group_by(site_name, age_years) %>% 
    summarize(grand_mean = mean(mean_meas)) %>% 
    ggplot(aes(x = age_years, y = grand_mean, colour = site_name, linetype = site_name)) +
    geom_line(size = 0.6) +
    scale_color_manual(values = site_colours,                      breaks = attrition %>% distinct(site_name) %>% pull()) +
    scale_linetype_manual(values = site_linetypes,                      breaks = attrition %>% distinct(site_name) %>% pull()) +
    preserve_feas_plot() +
    theme(legend.position = "bottom", legend.key.width = unit(1.5, "cm")) +
    xlab("Year of age") +
    xlim(0, 21)

  return(vitals_vals_plot1)

  # vitals_vals_plot2 <- vitals_vals %>%
  #   ggplot(aes(x = age_years, y = mean_meas, colour = site_name, linetype = site_name)) +
  #   geom_point(size = 2) +
  #   geom_errorbar(aes(
  #     ymin = mean_meas - sd_meas,
  #     ymax = mean_meas + sd_meas,
  #     colour = site_colour
  #   ),
  #   width = .1) +
  #   scale_color_manual(values = site_colours,                      breaks = attrition %>% distinct(site_name) %>% pull()) +
  #   scale_linetype_manual(values = site_linetypes,                      breaks = attrition %>% distinct(site_name) %>% pull()) +
  #   preserve_feas_plot() +
  #   facet_wrap(~ site_name) +
  #   theme(legend.position = "bottom", legend.key.width = unit(1.5, "cm"))

  # print(vitals_vals_plot2)

}

get_lab_vals1_summary <- function(lab_vals1_tbl,
                                  codeset) {
  lab_vals1_tbl %>%
    inner_join(codeset, by = c("LAB_LOINC" = "concept_code")) %>%
    group_by(site_name, LAB_LOINC, RESULT_QUAL) %>%
    summarize(n_patids = sum(n_patids),
              n_rows = sum(n_rows)) %>%
    ungroup() %>%
    ggplot(aes(x = RESULT_QUAL, y = n_rows, fill = site_name)) +
    geom_col(position = position_dodge()) +
    scale_fill_manual(values = site_colours) +
    facet_wrap(~LAB_LOINC) +
    preserve_feas_plot() +
    theme(legend.position = "bottom", legend.key.width = unit(1.5, "cm"))
}


get_lab_vals2_summary <- function(lab_vals2_tbl,
                                  codeset) {
  lab_vals2_tbl <- lab_vals2_tbl %>%
    inner_join(codeset, by = c("LAB_LOINC" = "concept_code"))

  lab_vals2_plot1 <- lab_vals2_tbl %>%
    ggplot(aes(x = RESULT_UNIT, y = n_rows, fill = site_name)) +
    geom_col() +
    scale_color_manual(values = site_colours,                      breaks = attrition %>% distinct(site_name) %>% pull()) +
    scale_fill_manual(values = site_colours) +
    preserve_feas_plot() +
    facet_wrap(~ LAB_LOINC) +
    coord_flip()

  print(lab_vals2_plot1)

  lab_vals2_plot2 <- lab_vals2_tbl %>%
    ggplot(aes(x = site_name, y = mean_result_num, colour = site_colour)) +
    geom_point(size = 2) +
    geom_errorbar(
      aes(
        ymin = mean_result_num - sd_result_num,
        ymax = mean_result_num + sd_result_num,
        colour = site_colour
      ),
      width = .1
    ) +
    scale_fill_identity() +
    scale_colour_identity() +
    preserve_feas_plot() +
    facet_wrap(~ RESULT_UNIT, scales = "free_y") +
    coord_flip()

  print(lab_vals2_plot2)

  lab_vals2_plot_median <- lab_vals2_tbl %>%
    ggplot(aes(x = site_name, y = median_result_num, fill = site_colour)) +
    geom_col() +
    scale_fill_identity() +
    scale_colour_identity() +
    preserve_feas_plot() +
    facet_wrap(~ RESULT_UNIT, scales = "free") +
    coord_flip()

  print(lab_vals2_plot_median)

  lab_vals2_plot_min <- lab_vals2_tbl %>%
    ggplot(aes(x = site_name, y = min_result_num, fill = site_colour)) +
    geom_col() +
    scale_fill_identity() +
    scale_colour_identity() +
    preserve_feas_plot() +
    facet_wrap(~ RESULT_UNIT, scales = "free") +
    coord_flip()

  print(lab_vals2_plot_min)

  lab_vals2_plot_max <- lab_vals2_tbl %>%
    ggplot(aes(x = site_name, y = max_result_num, fill = site_colour)) +
    geom_col() +
    scale_fill_identity() +
    scale_colour_identity() +
    preserve_feas_plot() +
    facet_wrap(~ RESULT_UNIT, scales = "free") +
    coord_flip()

  print(lab_vals2_plot_max)
}

#' function that takes a dataframe and produces medians for
#' all three cohortts
#'
#' Assumes that table is wide and each cohort has a flag
#'
#' Works with RAW COUNTS
#'
#' @param tbl_name table name tot use
#' @param tbl_name_string string describing the output
#' @param med_col the column to produce median for
#'
#' @return 5 columns:
#'  - measure (`tbl_name_string`)
#'  - cohort
#'  - median_
#'  - min_mdn
#'  - max_mdn
#'


find_medians_from_raw <- function(tbl_name,
                                  tbl_name_string,
                                  med_col) {
  
  # cohort list
  cohorts <- c('attr_cht', 'high_scr', 'ckd_stage23')
  
  # empty list of medians
  mdns_list <- list()
  
  # for each cohort in the cohort list
  for (i in cohorts) {

    # restrict to rows where cohort i is 1 and to
    # where cohort is not empty
    except <-
      tbl_name %>%
      filter(!!sym(i) == 1,
             n_patids > 0)
    
    # group by column to produce medians from and use uncount
    # to duplicate rows according to n_patids, allowing calculation
    # of median
    mdns <-
      except %>%
      group_by(!!sym(med_col)) %>%
      # duplicate rows according to n_patids
      uncount(n_patids) %>%
      ungroup() %>%
      summarise(median_value = median(!!sym(med_col)))
    
    # use same process as chunk above to find medians for each site
    mdns_site <-
      except %>%
      group_by(!!sym(med_col)) %>%
      uncount(n_patids) %>%
      ungroup() %>%
      group_by(site_name) %>%
      summarise(median_value = median(!!sym(med_col))) %>%
      ungroup() %>%
      summarise(min_value = min(median_value),
                max_value = max(median_value))
    
    # create table with measure, cohort, median, and min and max
    # median across sites
    combined <-
      mdns %>%
      mutate(
        measure = tbl_name_string,
        min_value = mdns_site$min_value,
        max_value = mdns_site$max_value,
        cohort = i
      ) %>%
      select(measure,
             cohort,
             median_value,
             min_value,
             max_value)
    
    # add table as element of mdns list
    mdns_list[[i]] <- combined
    
  }
  
  # take union of all elements of mdns list
  reduce(.x = mdns_list,
         .f = dplyr::union)
  
}


#' Function that takes a dataframe and produces medians for
#' all three cohortts
#'
#' Assumes that table is wide and each cohort has a flag
#'
#' Works with MEDIAN COUNTS
#'
#' @param tbl_name table name to use
#' @param tbl_name_string string describing the output
#' @param med_col the column to produce minimum for
#'
#' @return 5 columns:
#'  - measure (`tbl_name_string`)
#'  - cohort
#'  - median_
#'  - min_mdn
#'  - max_mdn
find_medians_from_computed <- function(tbl_name,
                                       tbl_name_string,
                                       med_col) {
  # cohort list
  cohorts <- c('attr_cht', 'high_scr', 'ckd_stage23')
  
  # empty list of medians
  mdns_list <- list()
  
  # for each cohort in the cohort list
  for (i in cohorts) {
    
    # commenting chunk below because unnecessary and
    # incorrect for some other tables
    # # get list of cohorts excluding cohort i
    # new_list <- cohorts[cohorts != i]
    # 
    # # get names of the other 2 cohorts
    # first <- new_list[1]
    # second <- new_list[2]
    # 
    # # restrict to rows where cohort i is 1 and
    # # other 2 cohorts are 0
    # except <-
    #   tbl_name %>%
    #   filter(tbl_name[first] == 0 &
    #            tbl_name[second] == 0 &
    #            !!sym(i) == 1)
    
    # restrict to rows where cohort i is 1 and to
    # where cohort is not empty
    except <-
      tbl_name %>%
      filter(!!sym(i) == 1,
             n_patids > 0)
    
    # compute median of medians across sites
    mdns <-
      except %>%
      summarise(median_ = median(!!sym(med_col)))
    
    # compute min and max median across sites
    mdns_site <-
      except %>%
      group_by(site_name) %>%
      summarise(median_ = median(!!sym(med_col))) %>%
      ungroup() %>%
      summarise(min_mdn = min(median_),
                max_mdn = max(median_))
    
    # create table with measure, cohort, median, and min and max
    # median across sites
    combined <-
      mdns %>%
      mutate(
        measure = tbl_name_string,
        min_mdn = mdns_site$min_mdn,
        max_mdn = mdns_site$max_mdn,
        cohort = i
      ) %>%
      select(measure,
             cohort,
             median_,
             min_mdn,
             max_mdn)
    
    # add table as element of mdns list
    mdns_list[[i]] <- combined
    
  }
  
  # take union of all elements of mdns list
  reduce(.x = mdns_list,
         .f = dplyr::union)
  
}

#' function that sums across years
#' Returns site as well as aggregate by year
#'
#' @param tbl_name
#' @param tbl_name_string
#'


find_sums <- function(tbl_name,
                      tbl_name_string) {

  cohorts <- c('attr_cht','high_scr','ckd_stage23')


  denoms_list <- list()


  for(i in cohorts) {

    # restrict to rows where cohort i is 1 and to
    # where cohort is not empty
    except <-
      tbl_name %>%
      filter(!!sym(i) == 1,
             n_patids > 0)


    sums_all <-
      except %>%
      summarise(
        sum_all_sites = sum(n_rows),
        sum_pts_sites = sum(n_patids)
      ) %>% mutate(measure = tbl_name_string)

    combined <-
      except %>%
      select(
        site_name,
        site_colour,
        site_linetype,
        n_patids,
        n_rows,
        year
      ) %>%
      left_join(
        sums_all
      ) %>%
      mutate(cohort = i)


    denoms_list[[i]] <- combined

  }

  reduce(.x=denoms_list,
         .f=dplyr::union)

}

find_counts <- function(tbl_name,
                        med_col) {
  
  cohorts <- c('attr_cht', 'high_scr', 'ckd_stage23')
  
  
  counts_list <- list()
  
  if (med_col == "RACE") {
    tbl_name <- tbl_name %>%
      parse_race()
  }
  else if (med_col == "HISPANIC") {
    tbl_name <- tbl_name %>%
      parse_ethnicity()
  }
  else if (med_col == "SEX") {
    tbl_name <- tbl_name %>%
      mutate(SEX = case_when(SEX == "F" ~ "Female",
                             SEX == "M" ~ "Male",
                             TRUE ~ "other"))
  }
  
  for (i in cohorts) {
    # restrict to rows where cohort i is 1 and to
    # where cohort is not empty
    except <-
      tbl_name %>%
      filter(!!sym(i) == 1,
             n_patids > 0)
    
    totals <- except %>%
      summarise(total = sum(n_patids)) %>%
      ungroup()
    
    counts <-
      except %>%
      group_by(!!sym(med_col)) %>%
      summarise(count = sum(n_patids)) %>%
      ungroup() %>%
      mutate(
        cohort = i,
        measure = tolower(med_col),
        percent = 100 * count / totals$total,
        category = !!sym(med_col)
      ) %>%
      select(measure, cohort, category, count, percent)
    
    counts_list[[i]] <- counts
    
  }
  
  reduce(.x = counts_list,
         .f = dplyr::union)
  
}

# Drug Counts for each cohoort
compute_drug_cts_cohort <-
  function(drug_cts,
           denom_tbl,
           tbl_name_string) {


    drug_cts_props <-
      drug_cts %>%
      select(
        year,
        cohort,
        sum_pts_sites
      ) %>%
      rename(drug_pts=sum_pts_sites) %>%
      inner_join(
        select(
          denom_tbl,
          year,
          cohort,
          sum_pts_sites
        )
      ) %>% distinct() %>%
      mutate(
        drug_pt_props = drug_pts/sum_pts_sites
      )

    drug_props_cohort <-
      drug_cts_props %>%
      group_by(cohort) %>%
      summarise(median_prop=
                  median(drug_pt_props),
                min_prop=min(drug_pt_props),
                max_prop=max(drug_pt_props)) %>%
      ungroup() %>% mutate(
        measure=tbl_name_string
      )

  }



find_missing_yrs <- function(cohort,
                             ct_tbl) {

  yrs <-
    ct_tbl %>%
    distinct(year) %>%
    filter(year>= 2009 & year<= 2021) %>%
    pull() %>% as.vector()

  sites <-
    ct_tbl %>% ungroup() %>%
    distinct(site_name)

  missing_yrs <- list()

  for(i in yrs) {

    yr_none_setup <-
      cohort %>%
      filter(
        year==i
      ) %>% select(site_name,
                   year) %>%
      distinct() %>%
      mutate(drug_cts=1)

    yr_none <-
      sites %>%
      left_join(
        yr_none_setup
      ) %>% #filter(year == i) %>%
      mutate(
        drug_cts =
          case_when(
            is.na(year) ~ 0,
            TRUE ~ 1
          )
      ) %>% mutate(year=i) %>%
      filter(drug_cts==0)
     # filter(is.na(year)) %>%
    #  mutate(year=i,
       #      drug_cts=0)

    t=as.character(i)
    missing_yrs[[t]] <- yr_none

  }

  reduce(.x=missing_yrs,
         .f=dplyr::union)
}


get_codeset_summary_flex <- function(cohort_cts,
                                     output_line,
                                     #facet_var,
                                     y_axis) {

  cts_plot <- cohort_cts %>%
    ggplot(aes(
      x = year,
      y = !! sym(output_line),
      colour = site_name,
      linetype = site_name
    )) +
    geom_line(size = 0.6) +
    preserve_feas_plot() +
    scale_color_manual(values = site_colours,                      breaks = attrition %>% distinct(site_name) %>% pull()) +
    scale_linetype_manual(values = site_linetypes,                      breaks = attrition %>% distinct(site_name) %>% pull()) +
    theme(legend.position = "bottom", legend.key.width = unit(1.5, "cm")) +
    xlab("Calendar year") +
    ylab(y_axis) +
   facet_wrap(~cohort) +
    xlim(2009, 2021)

  print(cts_plot)

}

## Reads in concept codes from SAS file
read_codes<-function(code_pathname){
  code_pathname %>%
    read_sas %>%
    select("concept_code", "concept_name") %>%
    distinct %>%
    return()
}


## Given a code count data frame, code_cts_df, and a vocabulary data frame, vocab_df 
##(such as one produced from a SAS file via read_codes), filters for concept codes
##in code_cts_df that are in vocab_df, then left joins to vocab_df to obtain concept names.
make_code_table<-function(code_cts_df, vocab_df){
  code_table<-code_cts_df %>%
    filter(concept_code %in% vocab_df$concept_code) %>%
    left_join(., vocab_df, by=c("concept_code"="concept_code"))
  code_table$code_concept<-paste(code_table$concept_code, " (",
                                 code_table$concept_name, ")", sep="")
  return(code_table)
}



##Given code count data frame code_cts_df, (such as one produced by make_code_table)
##creates a heatmap with rows given by grp_by (code_concept by default), 
##and columns given by site (scaled by column). Only counts in the attrition cohort
##unless specified otherwise.
make_heatmap<-function(code_cts_df, main=NULL, threshold=0, grp_by="code_concept", cexRow=0.9, ...){
  count_df<-code_cts_df %>%
    group_by(across(all_of(c("site_name",grp_by)))) %>%
    summarize(count=sum(n_patids)) %>%
    filter(count>0) %>%
    pivot_wider(names_from=site_name,
                values_from=count,
                values_fill=0)
  count_df<-filter(count_df, rowSums(count_df[,-1])/sum(rowSums(count_df[,-1]))>threshold)
  count_df<-arrange(count_df, count_df[[1]])
  codes<-count_df[[1]]
  count_df<-count_df[2:ncol(count_df)]
  rownames(count_df)<-str_trunc(codes, 100)
  codes_matrix<-as.matrix(count_df)
  coul <- brewer.pal(5, "BuPu") 
  coul <- colorRampPalette(coul)(100)
  return(heatmap(codes_matrix, Rowv=NA, Colv=NA, scale="column", col=coul, main=main, cexRow=cexRow,...))
}

# Rewrite heatmap function using mostly tidyverse syntax (to check understanding)
make_heatmap2 <-
  function(code_cts_df,
           main = NULL,
           threshold = 0,
           grp_by = "code_concept",
           cexRow = 0.9,
           ...) {
    code_cts_df <- code_cts_df %>%
      mutate(grand_total = sum(n_patids)) %>%
      group_by(!!sym(grp_by), grand_total) %>%
      mutate(grp_total = sum(n_patids)) %>%
      ungroup() %>%
      group_by(site_name, !!sym(grp_by), grp_total, grand_total) %>%
      summarize(count = sum(n_patids)) %>%
      ungroup() %>% 
      filter(count > 0) %>%
      pivot_wider(
        id_cols = c(!!sym(grp_by), grand_total, grp_total),
        names_from = site_name,
        values_from = count,
        values_fill = 0
      ) %>%
      mutate(!!sym(grp_by) := str_trunc(!!sym(grp_by), 100)) %>%
      filter(grp_total / grand_total > threshold) %>%
      select(-c(grp_total, grand_total)) 
    
    codes_matrix <- code_cts_df %>%
      arrange(!!sym(grp_by)) %>% 
      column_to_rownames(var = grp_by) %>% 
      as.matrix()
    
    coul <- brewer.pal(5, "BuPu")
    coul <- colorRampPalette(coul)(100)
    
    if (nrow(codes_matrix) > 1){
    return(
      heatmap(
        codes_matrix,
        Rowv = NA,
        Colv = NA,
        scale = "column",
        col = coul,
        main = main,
        cexRow = cexRow,
        ...
      )
    )
    }
    else if (nrow(codes_matrix) == 1){
      return(
        code_cts_df %>%
          rename(code = code_concept) %>% 
          select(code)
      )
    }
    
  }

# Rewrite heatmap rows function using mostly tidyverse syntax (to check understanding)
make_heatmap_rows <-
  function(code_cts_df,
           main = NULL,
           threshold = 0,
           grp_by = "code_concept",
           cexRow = 0.9,
           ...) {
    code_cts_df <- code_cts_df %>%
      mutate(grand_total = sum(n_patids)) %>%
      group_by(!!sym(grp_by), grand_total) %>%
      mutate(grp_total = sum(n_patids)) %>%
      ungroup() %>%
      group_by(site_name, !!sym(grp_by), grp_total, grand_total) %>%
      summarize(count = sum(n_rows)) %>%
      ungroup() %>% 
      filter(count > 0) %>%
      pivot_wider(
        id_cols = c(!!sym(grp_by), grand_total, grp_total),
        names_from = site_name,
        values_from = count,
        values_fill = 0
      ) %>%
      mutate(!!sym(grp_by) := str_trunc(!!sym(grp_by), 100)) %>%
      filter(grp_total / grand_total > threshold) %>%
      select(-c(grp_total, grand_total)) 
    
    codes_matrix <- code_cts_df %>%
      arrange(!!sym(grp_by)) %>% 
      column_to_rownames(var = grp_by) %>% 
      as.matrix()
    
    coul <- brewer.pal(5, "BuPu")
    coul <- colorRampPalette(coul)(100)
    
    if (nrow(codes_matrix) > 1){
      return(
        heatmap(
          codes_matrix,
          Rowv = NA,
          Colv = NA,
          scale = "column",
          col = coul,
          main = main,
          cexRow = cexRow,
          ...
        )
      )
    }
    else if (nrow(codes_matrix) == 1){
      return(
        code_cts_df %>%
          rename(code = code_concept) %>% 
          select(code)
      )
    }
    
  }


#make_counts_df<-function(code_cts_df, attr=1, grp_by="code_concept"){
#  count_df<-code_cts_df %>%
#    filter(attr_cht==attr) %>%
#    group_by(across(all_of(c("site_name",grp_by)))) %>%
#    summarize(count=sum(n_rows)) %>%
#    filter(count>0) %>%
#    pivot_wider(names_from=site_name,
#                values_from=count,
#                values_fill=0) 
#  return(count_df)
#}

make_yrs <- function(x){
  round(x / 365.25, 2)
}

## Functions for drugs 

drug_summary <- 
  function(tbl_name,
           tbl_name_string,
           denom_tbl) {
    
    drug_cts <- 
      find_sums(tbl_name = tbl_name,
                tbl_name_string=tbl_name_string)
    drugs <- 
      compute_drug_cts_cohort(drug_cts = drug_cts,
                              denom_tbl= denom_tbl,
                              tbl_name_string = tbl_name_string)
    
    
    
  }

get_drug_summary_table <- function(codeset_cts_tbl,
                                   denom_tbl){
  # Process drug data, separating prescriptions and administrations
  list_mdns <- list()
  ## ACE inhibitors
  list_mdns[['ace_rx']] <-
    drug_summary(
      tbl_name =
        codeset_cts %>%
        filter(prefix == 'pres' &
                 codeset == 'ace_inhibitor_rx') %>%
        group_by(year),
      tbl_name_string = 'Angiotensin-converting-enzyme inhibitors (prescription)',
      denom_tbl = denom_tbl
    )
  
  list_mdns[['ace_meds']] <-
    drug_summary(
      tbl_name =
        codeset_cts %>%
        filter(prefix == 'med' &
                 codeset == 'ace_inhibitor_rx') %>%
        group_by(year),
      tbl_name_string = 'Angiotensin-converting-enzyme inhibitors (administration)',
      denom_tbl = denom_tbl
    )
  ## Beta blockers
  list_mdns[['bb_rx']] <- drug_summary(
    tbl_name =
      codeset_cts %>%
      filter(prefix == 'pres' & codeset == 'bb_rx') %>%
      group_by(year),
    tbl_name_string = 'Beta blockers (prescription)',
    denom_tbl = denom_tbl
  )
  list_mdns[['bb_meds']] <- drug_summary(
    tbl_name =
      codeset_cts %>%
      filter(prefix == 'med' & codeset == 'bb_rx') %>%
      group_by(year),
    tbl_name_string = 'Beta blockers (administration)',
    denom_tbl = denom_tbl
  )
  ## ARBs
  list_mdns[['arbs_rx']] <- drug_summary(
    tbl_name =
      codeset_cts %>%
      filter(prefix == 'pres' & codeset == 'arb_rx') %>%
      group_by(year),
    tbl_name_string = 'Angiotensin receptor blockers (prescription)',
    denom_tbl = denom_tbl
  )
  list_mdns[['arbs_meds']] <- drug_summary(
    tbl_name =
      codeset_cts %>%
      filter(prefix == 'med' & codeset == 'arb_rx') %>%
      group_by(year),
    tbl_name_string = 'Angiotensin receptor blockers (administration)',
    denom_tbl = denom_tbl
  )
  ## Thiazides
  list_mdns[['thiazide_rx']] <- drug_summary(
    tbl_name =
      codeset_cts %>%
      filter(prefix == 'pres' &
               codeset == 'thiazide_rx') %>%
      group_by(year),
    tbl_name_string = 'Thiazides (prescription)',
    denom_tbl = denom_tbl
  )
  list_mdns[['thiazide_meds']] <- drug_summary(
    tbl_name =
      codeset_cts %>%
      filter(prefix == 'med' &
               codeset == 'thiazide_rx') %>%
      group_by(year),
    tbl_name_string = 'Thiazides (administration)',
    denom_tbl = denom_tbl
  )
  ## CCBs
  list_mdns[['ccb_rx']] <- drug_summary(
    tbl_name =
      codeset_cts %>%
      filter(prefix == 'pres' & codeset == 'ccb_rx') %>%
      group_by(year),
    tbl_name_string = 'Calcium channel blockers (prescription)',
    denom_tbl = denom_tbl
  )
  list_mdns[['ccb_meds']] <- drug_summary(
    tbl_name =
      codeset_cts %>%
      filter(prefix == 'med' & codeset == 'ccb_rx') %>%
      group_by(year),
    tbl_name_string = 'Calcium channel blockers (administration)',
    denom_tbl = denom_tbl
  )
  ## Loop diuretics
  list_mdns[['loop_diuretic_rx']] <- drug_summary(
    tbl_name =
      codeset_cts %>%
      filter(prefix == 'pres' & codeset == 'loop_diuretic_rx') %>%
      group_by(year),
    tbl_name_string = 'Loop diuretics (prescription)',
    denom_tbl = denom_tbl
  )
  list_mdns[['loop_diuretic_meds']] <- drug_summary(
    tbl_name =
      codeset_cts %>%
      filter(prefix == 'med' & codeset == 'loop_diuretic_rx') %>%
      group_by(year),
    tbl_name_string = 'Loop diuretics (administration)',
    denom_tbl = denom_tbl
  )
  
  
  all_drugs_df <- 
    reduce(.x=list_mdns,
           .f=union) %>% relocate('measure', .before = 'cohort')
}

draw_ckd_cohort_venn <- function(cohort_flags) {
  cohort_flags_uncount <- cohort_flags %>%
    uncount(n_patids)
  
  venn <- VennDiagram::draw.triple.venn(
    area1 = cohort_flags_uncount %>% filter(attr_cht == 1)  %>%
      count() %>% pull(),
    area2 = cohort_flags_uncount %>% filter(high_scr == 1) %>%
      count() %>% pull(),
    area3 = cohort_flags_uncount %>% filter(ckd_stage23 == 1) %>%
      count() %>% pull(),
    n12 = cohort_flags_uncount %>% filter(attr_cht == 1 &
                                            high_scr == 1) %>%
      count() %>% pull(),
    n23 = cohort_flags_uncount %>% filter(high_scr == 1 &
                                            ckd_stage23 == 1) %>%
      count() %>% pull(),
    n13 = cohort_flags_uncount %>% filter(attr_cht == 1 &
                                            ckd_stage23 == 1) %>%
      count() %>% pull(),
    n123 = cohort_flags_uncount %>% filter(attr_cht == 1 &
                                             high_scr == 1 & ckd_stage23 == 1) %>%
      count() %>% pull(),
    category = c("Attrition",
                 "High serum creatinine",
                 "CKD Stage 2/3"),
    scaled = FALSE,
    cat.pos = c(0, 0, 0),
    fontfamily = rep("sans", 7),
    cat.fontfamily = rep("sans", 3),
  )
  
  grid.draw(venn)
}

#' Function to summarize calendar years (default 2009-2021) with missing data for each site
#' for the union of all 3 mild-to-moderate CKD cohorts
#'
#' @param codeset_cts Table with patient count and code occurrence count in
#' each PCORnet table for each codeset per calendar year
#' @param codeset_name Name of codeset to be summarized
#'
#' @return Table with each site, number of calendar years with missing data, 
#' and list of calendar years with missing data (default 2009-2021)
#' 
get_missing_years <- function (codeset_ct_tbl,
                               codeset_name,
                               min_year = 2009,
                               max_year = 2021,
                               site_config = site_report_config) {

  
  expand.grid(
    site_name = site_config %>% distinct(site_name) %>% pull(),
    year = c(min_year:max_year)
  ) %>%
    left_join(
      codeset_cts %>%
        filter(codeset == codeset_name,
               year >= min_year,
               year <= max_year) %>%
        group_by(site_name,
                 year) %>%
        summarize(n_patids = sum(n_patids),
                  n_rows = sum(n_rows)) %>%
        ungroup(),
      by = c("site_name", "year")
    ) %>%
    mutate(missing = if_else(is.na(n_rows), "missing", "non_missing")) %>%
    group_by(site_name, missing) %>%
    summarize(years = list(year),
              count_years = n_distinct(year)) %>%
    ungroup() %>%
    pivot_wider(values_from = c(years, count_years),
                names_from = missing) %>%
    mutate(count_years_missing = if_else(is.na(count_years_missing), 0L, count_years_missing)) %>%
    select(site_name, count_years_missing, years_missing)
}

#' Function to summarize calendar years (default 2009-2021) with missing data for each site
#' for the union of all 3 mild-to-moderate CKD cohorts
#'
#' @param codeset_cts Table with patient count and code occurrence count in
#' each PCORnet table for each codeset per calendar year
#' @param codeset_name Name of codeset to be summarized
#'
#' @return Table with each site, number of calendar years with missing data, 
#' and list of calendar years with missing data (default 2009-2021)
#' 
get_missing_years_vitals <- function (vitals_tbl,
                                      min_year = 2009,
                                      max_year = 2021,
                                      site_config = site_report_config) {
  
  expand.grid(site_name = site_config %>% distinct(site_name) %>% pull(),
              year = c(min_year:max_year)) %>%
    left_join(
      vitals_tbl %>%
        filter(year >= min_year,
               year <= max_year) %>%
        group_by(site_name,
                 year) %>%
        summarize(n_patids = sum(n_patids),
                  n_rows = sum(n_rows)) %>%
        ungroup(),
      by = c("site_name", "year")
    ) %>%
    mutate(missing = if_else(is.na(n_rows), "missing", "non_missing")) %>%
    group_by(site_name, missing) %>%
    summarize(years = list(year),
              count_years = n_distinct(year)) %>%
    ungroup() %>%
    pivot_wider(values_from = c(years, count_years),
                names_from = missing) %>%
    mutate(count_years_missing = if_else(is.na(count_years_missing), 0L, count_years_missing)) %>%
    select(site_name, count_years_missing, years_missing)
}

result_type_summary <- function (codeset_name, unit_type='mg/dl') {
  all_result_types_sum <- lab_vals2 %>%
    inner_join(read_codeset(codeset_name,'icccc'), 
               by = c("LAB_LOINC" = "concept_code")
    ) %>%
    mutate(RESULT_UNIT = tolower(RESULT_UNIT)) %>%
    group_by(site_name,RESULT_UNIT) %>% 
    summarise(sum_type = sum(n_rows)) %>% 
    ungroup()
  
  all_result_types_sum %>% 
    group_by(site_name) %>%
    summarise(all_sum = sum(sum_type)) %>% 
    ungroup() %>% 
    left_join(all_result_types_sum %>% 
                filter(RESULT_UNIT!=tolower(unit_type)) %>%
                group_by(site_name) %>%
                summarise(non_unit_type_sum = sum(sum_type)) %>%
                ungroup(),
              by = 'site_name') %>%
    mutate(proportion_not_in_units = ifelse(!is.na(non_unit_type_sum/all_sum),
                                            non_unit_type_sum/all_sum,
                                            0)
    ) %>%
    left_join(lab_vals1 %>%
                inner_join(read_codeset(codeset_name,'icccc'), 
                           by = c("LAB_LOINC" = "concept_code")
                ) %>% 
                group_by(site_name) %>% 
                summarise(sum_qual = sum(n_rows)) %>% ungroup(), 
              by = 'site_name') %>% 
    mutate(proportion_with_qual = sum_qual/all_sum) %>%
    select(site_name, proportion_not_in_units, proportion_with_qual)
}

#' note that result floor is inclusive of the floor in detecting outliers, ie <=
#' while result ceiling is not inclusive, ie >
get_outliers <- function(codeset_name, 
                         result_ceiling, 
                         result_floor, 
                         result_unit='mg/dl') {
  df <- lab_vals2 %>%
    inner_join(read_codeset(codeset_name,'icccc'), 
               by = c("LAB_LOINC" = "concept_code")
    ) %>% mutate(outlier = ifelse(
      (mean_result_num>result_ceiling) | 
        (median_result_num>result_ceiling) | 
        (mean_result_num<=result_floor) | 
        (median_result_num<=result_floor) | 
        (is.na(median_result_num)),
      1,
      0)
    ) %>% 
    filter(tolower(RESULT_UNIT)==tolower(result_unit)) %>%
    group_by(site_name, LAB_LOINC) %>% 
    summarise(outlier = sum(outlier)) %>%
    ungroup()
  rv <- tibble()
  for (s in df %>% select(site_name) %>% distinct() %>% pull()) {
    loinc_list <- df %>% 
      filter(site_name==s & outlier>=1) %>%
      select(LAB_LOINC) %>% 
      pull()
    rv <- rbind(rv, 
                tibble(site_name=s, 
                       outliers_LOINCs = ifelse(length(loinc_list)>=1,
                                                loinc_list,
                                                c('None')
                       )
                )
    )
  }
  rv
}

dot_plot_results <-
  function(lab_vals2_tbl,
           codeset_name,
           unit_type,
           result_ceiling = 100,
           title = "") {

    df <- lab_vals2_tbl %>%
      inner_join(read_codeset(codeset_name, 'icccc'),
                 by = c("LAB_LOINC" = "concept_code")) %>%
      filter(tolower(RESULT_UNIT) == tolower(unit_type)) %>% 
      group_by(site_name, LAB_LOINC) %>%
      summarize(median_result_num = mean(median_result_num),
                mean_result_num = mean(mean_result_num)) %>% 
      ungroup() %>% 
      mutate(label_col = paste0(site_name, ' - ',
                               LAB_LOINC),
             median_res = ifelse(
               median_result_num>result_ceiling,
               result_ceiling,
               median_result_num),
             mean_res = ifelse(
               mean_result_num>result_ceiling,
               result_ceiling,
               mean_result_num)) %>%
      select(label_col, median_res, mean_res) %>% 
      arrange(desc(label_col))
    dotchart(
      df$median_res,
      pch = 21,
      labels = df$label_col,
      bg = "green",
      pt.cex = 1.5,
      xlim = range(0, result_ceiling),
      xlab = paste0('Value', ' (', unit_type, ')')
      # main = title
    )
    abline(v = result_ceiling,
           col = "blue",
           lty = 2)
    points(df$mean_res,
           1:nrow(df),
           col = "red",
           pch = 15,
           cex = 1)
    legend(
      x = result_ceiling - (result_ceiling/4),
      y = 3,
      legend = c('median', 'mean'),
      fill = c("green", "red")
    )
  }

dot_plot_result_vitals <-
  function(vital_mean_medians,
           result_ceiling = 100,
           xlab_name,
           xmin) {
    
    df <- vital_mean_medians %>%
      group_by(site_name) %>% 
      summarize(median_res = mean(median_res),
                mean_res = mean(mean_res)) %>% 
      ungroup() %>% 
      select(site_name, median_res, mean_res) %>%
      arrange(desc(site_name))
    dotchart(
      df$median_res,
      pch = 21,
      labels = df$site_name,
      bg = "green",
      pt.cex = 1.5,
      xlim = range(xmin, result_ceiling),
      xlab = xlab_name
    )
    points(
      df$mean_res,
      1:nrow(df),
      col = "red",
      pch = 15,
      cex = 1
    )
    legend(
      x = xmin,
      y = 16,
      legend = c('median', 'mean'),
      fill = c("green", "red")
    )
    # return(df)
  }


##Creates a table needed to create the clustered violin plots.
make_bp_table<-function(bp_df, meas, cohort_name = "union_no_J_high_scr", age_years_min=1, age_years_max=21, n_clust=5){
  
  if(cohort_name == "union_no_J_high_scr" & params$mask_sites == TRUE) {
    cht_bp <- bp_df %>%
      filter(!(prefix == "high_scr" & site_name == "J"))
  }
  else if(cohort_name == "union_no_J_high_scr" & params$mask_sites == FALSE) {
    cht_bp <- bp_df %>%
      filter(!(prefix == "high_scr" & site_name == "site_J"))
  }
  
  ## Set range for what are extreme values
  if (grepl("sys_bp", meas)==1){
    min<-10
    max<-250
  }
  else if(grepl("dia_bp", meas)==1){
    min<-0
    max<-150
  }
  else if(grepl("weight", meas)==1){
    min=0
    max=800
  }
  else if(grepl("height", meas)==1){
    min=0
    max=100
  }
  else if(grepl("fu", meas)==1){
    min=-1
    max=20
  }
  
  if (grepl("height", meas)==1|grepl("weight", meas)==1|grepl("dia_bp", meas)==1|grepl("sys_bp", meas)==1){
    cht_bp<-bp_df %>%
      filter(age_years>=age_years_min, age_years<=age_years_max) %>%
      pivot_longer(cols=!!sym(meas)) %>%
      select("age_years", "site_name", "value", "n_patids")
  }
  else if (grepl("fu", meas)==1){
    cht_bp<-bp_df %>%
      pivot_longer(cols=!!sym(meas)) %>%
      select("site_name", "value", "n_patids") %>% 
      mutate(value = value/365.25)
  }

  
  # filter out extreme values
  reasonable_cht_bp<-cht_bp %>%
    filter(value>=min, value<=max)
  
  #make weights, with and without extreme values
  total_sum<-cht_bp %>%
    group_by(site_name) %>%
    summarize(site_total=sum(n_patids))
  cht_bp <- cht_bp %>%
    left_join(., total_sum, by=c("site_name"="site_name"))%>%
    mutate(weight=n_patids/site_total)
  
  reasonable_total_sum<-reasonable_cht_bp %>%
    group_by(site_name) %>%
    summarize(site_total=sum(n_patids))
  reasonable_cht_bp<-reasonable_cht_bp %>%
    left_join(., reasonable_total_sum, by=c("site_name"="site_name"))%>%
    mutate(reasonable_weight=n_patids/site_total)
  
  
  #cluster by mean measurement weighted by n_patids
  #cluster using reasonable site means (calculated with extreme values omitted)
  # (regular cluster is computed but not used)
  cht_bp_sum<-cht_bp %>%
    group_by(site_name) %>%
    summarize(site_mean=mean(value), weighted_site_mean=weighted.mean(value, n_patids)) %>%
    na.omit()
  reasonable_cht_bp_sum<-reasonable_cht_bp %>%
    group_by(site_name) %>%
    summarize(reasonable_site_mean=mean(value), reasonable_weighted_site_mean=weighted.mean(value, n_patids)) %>%
    na.omit()
  #cht_bp_sum$clust<-kmeans(cht_bp_sum$weighted_site_mean, n_clust)$cluster
  reasonable_cht_bp_sum$reasonable_clust<-kmeans(reasonable_cht_bp_sum$reasonable_weighted_site_mean, n_clust)$cluster
  
  cht_bp<-cht_bp %>%
    left_join(., cht_bp_sum[,
                            c("site_name",
                              "site_mean", "weighted_site_mean")],
              by=c("site_name"="site_name")) %>%
    left_join(., reasonable_cht_bp_sum[, c("site_name", "reasonable_site_mean", "reasonable_weighted_site_mean", "reasonable_clust")],
              by=c("site_name"="site_name"))
  cht_bp<-cht_bp[!is.na(cht_bp$reasonable_clust),]
  overall_mean<-weighted.mean(cht_bp$value, cht_bp$n_patids)
  reasonable_overall_mean<-weighted.mean(reasonable_cht_bp$value, reasonable_cht_bp$n_patids)
  
  cht_bp %>%
    mutate(overall_mean=overall_mean, reasonable_overall_mean=reasonable_overall_mean) %>%
    return()
}



make_violin_plot <- function(cht_bp,
                             xlab = "",
                             ylab = "",
                             xmin = 80,
                             xmax = 400)
{
  cht_bp %>%
    ggplot(aes(x = value, y = forcats::fct_rev(site_name), weights = weight)) +
    geom_violin(aes(colour = site_name)) +
    guides(colour = "none") +
    xlab(xlab) +
    ylab(ylab) +
    geom_point(aes(x = weighted_site_mean), col = "red") +
    geom_point(aes(x = reasonable_weighted_site_mean), col = "black") +
    coord_cartesian(xlim = c(xmin, xmax)) +
    geom_vline(xintercept = cht_bp$overall_mean[1], col = "red") +
    geom_vline(xintercept =
                 cht_bp$reasonable_overall_mean[1],
               col = "black") +
    preserve_feas_plot() +
    scale_color_manual(values = site_colours,                      breaks = attrition %>% distinct(site_name) %>% pull()) +
    scale_linetype_manual(values = site_linetypes) %>% 
    return()
}


cross_codeset_comparison <- function(denom = enc_type_cts,
                                     codeset_ct_tbl = codeset_cts,
                                     ymax = 100) {
  denom <- denom %>%
    filter(ENC_TYPE == 'AV') %>%
    group_by(site_name, year) %>%
    summarize(n_patids_denom = sum(n_patids)) %>%
    ungroup()
  
  codeset_ct_tbl %>%
    group_by(prefix, codeset, site_name, year) %>%
    summarize(n_patids = sum(n_patids)) %>%
    ungroup() %>%
    full_join(denom, by = c("site_name", "year")) %>%
    filter(!is.na(n_patids),
           !is.na(n_patids_denom),
           year >= 2009,
           year <= 2021) %>%
    mutate(pct_patids_denom = 100 * (n_patids / n_patids_denom)) %>%
    group_by(prefix, year, codeset) %>%
    summarize(pct_patids_denom = mean(pct_patids_denom)) %>%
    ungroup() %>%
    mutate(
      table = case_when(
        prefix == "med" ~ "MED_ADMIN",
        prefix == "pres" ~ "PRESCRIBING",
        prefix == "disp" ~ "DISPENSING",
        prefix == "lab" ~ "LAB_RESULT_CM",
        prefix == "proc" ~ "PROCEDURE",
        prefix == "cond" ~ "CONDITION",
        prefix == "diag" ~ "DIAGNOSIS"
      ),
      codeset = case_when(
        codeset == "arb_rx" ~ "Angiotensin receptor blockers",
        codeset == "bb_rx" ~ "Beta blockers",
        codeset == "ccb_rx" ~ "Calcium channel blockers",
        codeset == "loop_diuretic_rx" ~ "Loop diuretics",
        codeset == "thiazide_rx" ~ "Thiazide",
        codeset == "ace_inhibitor_rx" ~ "ACE inhibitors",
        codeset == "serum_creatinine" ~ "Serum creatinine",
        codeset == "serum_cystatin" ~ "Serum cystatin",
        codeset == "urine_protein_quant" ~ "Quantitative urine protein",
        codeset == "urine_protein_qual" ~ "Qualitative urine protein",
        codeset == "urine_creatinine" ~ "Urine creatinine")
    ) %>%
    ggplot(aes(
      x = year,
      y = pct_patids_denom,
      colour = codeset,
      linetype = codeset
    )) +
    geom_line(size = 0.6) +
    preserve_feas_plot() +
    theme(legend.position = "none") +
    xlab("Calendar year") +
    ylab("Percent patients") +
    facet_wrap(~ table) +
    scale_color_brewer(palette = "Dark2") +
    scale_x_continuous(limits = c(2009, 2023),
                       breaks = c(2010, 2015, 2020)) +
    ylim(0, ymax) +
    geom_text(
      data = . %>% group_by(codeset) %>%
        filter(year == 2021),
      aes(label = codeset),
      nudge_x = 1
    )
}


add_cohort_fullnames <- function(tbl_w_cohort_fields){
  tbl_w_cohort_fields %>% 
    rename(`Attrition cohort` = attr_cht,
           `CKD Stage 2/3 cohort` = ckd_stage23,
           `High serum creatinine cohort` = high_scr)
}

## Modified functions for row level query 1 aggregate data

filter_cohorts <- function(tbl) {
  tbl_fltrd <- tbl %>%
    filter(prefix == "attr_cht")
}


get_site_refs <- function(site_config) {
  if (params$mask_sites) {
    sites <- site_config %>%
      mutate(masked_site = site_mask) %>%
      select(site_name, masked_site) %>%
      mutate(site_name = toupper(site_name)) %>%
      pivot_wider(names_from = site_name,
                  values_from = masked_site)
  } else {
    sites <- site_config %>%
      mutate(masked_site = toupper(site_name)) %>%
      select(site_name, masked_site) %>%
      mutate(site_name = toupper(site_name)) %>%
      pivot_wider(names_from = site_name,
                  values_from = masked_site)
  }
  return(sites)
}
