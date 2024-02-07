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

get_site_refs <- function(site_config) {
  if (params$mask_sites) {
    sites <- site_config %>%
      mutate(masked_site = site_mask) %>%
      select(site, masked_site) %>%
      mutate(site = toupper(site)) %>%
      pivot_wider(names_from = site,
                  values_from = masked_site)
  } else {
    sites <- site_config %>%
      mutate(masked_site = toupper(site)) %>%
      select(site, masked_site) %>%
      mutate(site = toupper(site)) %>%
      pivot_wider(names_from = site,
                  values_from = masked_site)
  }
  return(sites)
}

get_results <- function(tbl_name) {
  if (data_source == 'local') {
    rslt <- read_csv(paste0('../results/', tbl_name, '.csv'))
  }
  else {
    rslt <- results_tbl(tbl_name) %>% collect_new()
  }
  if (any(tbl_vars(rslt) == 'site_name')) rslt <- rename(rslt, site = site_name)
  rslt %>%
    add_site_labels()
}

prettify_kable <- function(data,
                           table_caption = "add a caption") {
  data %>% kable(digits = 2, format.args = list(big.mark = ','), caption = table_caption) %>%
    kable_styling(bootstrap_options = c("striped", "hover")) %>%
    column_spec(1, bold = T, border_right = T)
}

preserve_dq2_plot <- function(legend = FALSE) {
  if (legend) {
    theme_minimal() +
      theme(legend.position = "bottom")
  }
  else {
    theme_minimal() +
      theme(legend.position = "none")
  }
}

preserve_plot_config <- function(plot,
                                 fill_values = site_colours,
                                 linetype_values = site_linetypes) {
  plot +
    scale_fill_manual(values = fill_values)  +
    scale_linetype_manual(values = linetype_values)
}

add_site_labels <- function(data,
                            xwalk = site_report_config,
                            defer_collect = FALSE) {
  if (defer_collect && params$mask_sites && any(tbl_vars(data) == 'site')) {
    data_final <- data %>%
      select(-starts_with("site_col"), -starts_with("site_linetype"),
             -any_of('site_mask')) %>%
      left_join(xwalk, by = "site", copy = TRUE) %>%
      select(-site) %>%
      mutate(site = site_mask)
  }
  else if (defer_collect && any(tbl_vars(data) == 'site')) {
    data_final <- data %>%
      select(-starts_with("site_col"), -starts_with("site_linetype"),
             -any_of('site_mask')) %>%
      left_join(xwalk, by = "site", copy = TRUE) %>%
      arrange(site)
  }
  else if (params$mask_sites && any(tbl_vars(data) == 'site')) {
    data_final <- data %>%
      collect() %>%
      select(-starts_with("site_col"), -starts_with("site_linetype"),
             -any_of('site_mask')) %>%
      left_join(xwalk, by = "site") %>%
      select(-site) %>%
      mutate(site = site_mask) %>%
      arrange(site)
  }
  else if (any(tbl_vars(data) == 'site')) {
    data_final <- data %>%
      collect() %>%
      select(-starts_with("site_col"), -starts_with("site_linetype")) %>%
      left_join(xwalk, by = "site") %>%
      arrange(site)
  }
  else {
    data_final <- collect(data)

  }
  data_final

}

my.render.cat <- function(x) {
  c("", sapply(stats.default(x), function(y)
    with(y,
         sprintf(
           "%s (%0.1f%%)", format(FREQ, big.mark = ","), PCT
         ))))
}

my.render.cont <- function(x) {
  with(
    stats.apply.rounding(stats.default(x), digits = 3),
    c(
      "",
      "Mean (SD)" = sprintf("%s (%s)", MEAN, SD),
      "Median [IQR]" = sprintf("%s [%s-%s]", MEDIAN, Q1, Q3),
      "Min-Max" = sprintf("%s-%s", MIN, MAX)
    )
  )
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
      group_by(site,!!sym(grp_by), grp_total, grand_total) %>%
      summarize(count = sum(n_patids)) %>%
      ungroup() %>%
      filter(count > 0) %>%
      pivot_wider(
        id_cols = c(!!sym(grp_by), grand_total, grp_total),
        names_from = site,
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

    if (nrow(codes_matrix) > 1) {
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
    else if (nrow(codes_matrix) == 1) {
      return(code_cts_df %>%
               rename(code = code_concept) %>%
               select(code))
    }

  }

get_cohort_count_summary <- function(attrition_table,
                                     count_table) {
  step3_counts <- attrition_table %>%
    filter(step_num == 3) %>%
    mutate(step3 = counts) %>%
    select(site, step3)

  cht_counts <- count_table %>%
    group_by(site) %>%
    summarize(counts = sum(n_patids)) %>%
    ungroup() %>%
    # suppress small cell sizes
    mutate(counts = if_else(counts > 0 &
                              counts < 11, 10, counts)) %>%
    full_join(step3_counts, by = "site") %>%
    mutate(percent_step3 = 100 * (counts / step3)) %>%
    select(site, counts, percent_step3) %>%
    arrange(site)

  cht_plot <- cht_counts %>%
    ggplot(aes(
      x = site,
      y = percent_step3,
      label = scales::comma(counts,
                            accuracy = 1)
    )) +
    geom_col(alpha = 0.75, position = position_dodge()) +
    preserve_dq2_plot() +
    scale_fill_grey() +
    xlab("Site") +
    ylab("Percentage step 3") +
    scale_y_continuous(labels = scales::comma) +
    labs(caption = "Raw counts label each bar") +
    geom_label(fill = "white")

  return(list(cht_plot,
              cht_counts %>% select(starts_with("-site_colour"))))
}


# demographic	race	01	01=American  Indian  or  Alaska  Native
# demographic	race	02	02=Asian
# demographic	race	03	03=Black  or  African  American
# demographic	race	04	04=Native  Hawaiian  or  Other  Pacific  Islander
# demographic	race	05	05=White
# demographic	race	06	06=Multiple  race
# demographic	race	07	07=Refuse  to  answer
# demographic	race	NI	NI=No  information
# demographic	race	UN	UN=Unknown
# demographic	race	OT	OT=Other
parse_race <- function(tbl) {
  tbl %>% mutate(
    race = case_when(
      # race == "01" ~ "American  Indian  or  Alaska  Native",
      # race == "02" ~ "Asian",
      race == "03" ~ "Black  or  African  American",
      # race == "04" ~ "Native  Hawaiian  or  Other  Pacific  Islander",
      race == "05" ~ "White",
      # race == "06" ~ "Multiple  race",
      race == "07" | race == "OT" | race == "NI" | race == "UN" ~
        "Refuse  to  answer / Other / No  information / Unknown",
      TRUE ~ "American  Indian  or  Alaska  Native /
      Asian /
      Native  Hawaiian  or  Other  Pacific  Islander /
      Multiple  race"
    )
  )
}


# demographic	hispanic	Y	Y=Yes
# demographic	hispanic	N	N=No
# demographic	hispanic	R	R=Refuse  to  answer
# demographic	hispanic	NI	NI=No  information
# demographic	hispanic	UN	UN=Unknown
# demographic	hispanic	OT	OT=Other
parse_ethnicity <- function(tbl) {
  tbl %>% mutate(
    hispanic = case_when(
      hispanic == "Y" ~ "Yes",
      hispanic == "N" ~ "No",
      hispanic == "R" | hispanic == "OT" | hispanic == "NI" | hispanic == "UN" ~
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

get_pct_cohort_below_thresh <- function(egfr_tbl,
                                        thresh_vctr = seq(10L, 90L, by = 10L)) {
  below_thresh <- list()
  for (thresh in thresh_vctr) {
    below_thresh[[paste0('thresh_', thresh)]] <- egfr_tbl %>%
      filter(egfr < thresh) %>%
      group_by(site) %>%
      summarize(n_patids_low = n_distinct(patid)) %>%
      ungroup() %>%
      collect() %>%
      mutate(threshold = thresh)
  }
  return(below_thresh)
}
