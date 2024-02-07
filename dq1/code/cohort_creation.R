library(haven)
library(reticulate)
source_python(paste0(base_dir,'/code/test_log.py'))

#'Reads all sas files as dataframes and assigns them to the global environment
#'RETURNS: a character vector of dataframe names
#'@params
#'table_dir - a directory with sas data files
#'site - a string of the site whose files are to be read
#'error logs - an existing list that will be appended in place with data files
#'              that could not be read
read_all_dfs <- function(table_dir, site, error_logs) {
  sas_files_vec <- list.files(table_dir)
  return_filenames <- c()
  for (f in sas_files_vec) {
    df <- data.frame()
    f_name <- gsub('.sas7bdat','',f)
    if (grepl('.log',f)) {
      f_name <- gsub('.log','',f_name)
      logs <- try(clean_logs(paste(table_dir,f,sep='/')) %>%
                    mutate(site_name=site)
                  )
      if (class(logs) == 'try-error') {
          message(paste(f, 'not loaded for', site, sep=' '))
          error_logs <- append(error_logs, tibble(site_name=site,file=f))
        } else if (exists(f_name)) {
          running_df <- get(f_name)
          colnames(logs) <- colnames(running_df)
          assign(f_name, rbind(running_df, logs), envir = .GlobalEnv)
        } else {
          assign(f_name, logs, envir = .GlobalEnv)
          return_filenames <- append(return_filenames, f_name)
        }
      } else {
      df <- try(read_sas(paste(table_dir,f,sep='/')) %>%
                  mutate(site_name = site)
      )
      if (f_name=='attrition'){
        colnames(df) <- tolower(colnames(df))
        df <- attrition_percents(df)}
      
      if (class(df) == 'try-error') {
        message(paste(f, 'not loaded for', site, sep=' '))
        error_logs <- append(error_logs, tibble(site_name=site,file=f))
      } else if (exists(f_name)) {
        running_df <- get(f_name)
        colnames(df) <- colnames(running_df)
        assign(f_name, rbind(running_df, df), envir = .GlobalEnv)
      } else {
        assign(f_name, df, envir = .GlobalEnv)
        return_filenames <- append(return_filenames, f_name)
      }
    }
  }
  return_filenames
}

#'Adds a percentage change column for each of the prior step, step 0, and step 3
#'RETURNS: A dataframe
#'@params
#'attr_tbl - a dataframe of the attrition table     
#'
attrition_percents <- function(attr_tbl){
  names(attr_tbl)[3] <- 'counts'
  attr_tbl['step_num'] <- 0:(nrow(attr_tbl)-1)
  step0 <- attr_tbl %>% filter(step_num==0) %>% select(counts) %>% pull()
  step3 <- attr_tbl %>% filter(step_num==3) %>% select(counts) %>% pull()
  if (!length(step0)) {step0=NA}
  if (!length(step3)) {step3=NA}

  attr_tbl %>%
    mutate(percent_prior_step = counts/lag(counts)*100,
           percent_step0 = counts/step0*100,
           percent_step3 = counts/step3*100)
}

#'Makes a dictionary based on substring matching where the substring is the key
#'and a list of the strings that contain the key is the value
#'RETURNS: A list of key-value pairs
#'@params
#'substr_vec - A character vector of substrings to match on
#'fullstr_vec - A character vector of strings to match
#'
make_dict <- function(substr_vec, fullstr_vec){
  x <- list()
  for (s in substr_vec) {
    x[[s]] = fullstr_vec[grepl(s,fullstr_vec)]
  }
  return(x)
}

#'Combines dataframes per a vector of substrings upon which to match
#'RETURNS: A character vector of tables to upload to the database
#'@params
#'substr_vec - A character vector used to aggregate tables
#'file_vec - A character vector of table names to be considered for aggregation
#'
upload_prep <- function(file_vec, substr_vec) {
  file_dict <- make_dict(substr_vec, file_vec)
  for (n in names(file_dict)){
    df <- tibble()
    for (i in file_dict[[n]]) {
      messy_prefix <- gsub(n,'',i)
      final_char <- nchar(messy_prefix)
      df<- rbind(df, 
                 get(i) %>%
                   mutate(prefix = case_when(
                     substr(messy_prefix,1,1
                            )=='_' ~ substr(messy_prefix,2,final_char),
                     substr(messy_prefix, final_char, final_char
                            ) =='_' ~ substr(messy_prefix,1,final_char-1),
                     TRUE ~ messy_prefix)
                     )
                 )
      file_vec <- file_vec[!file_vec == i]
      }
    assign(n,df,envir=.GlobalEnv)
    file_vec <- append(file_vec, n)
  }
  return(file_vec)
}

#'A wrapper function to iterate through subfolders of parent_dir,
#'read sas data files, perform processing, and upload the resulting tables to
#'the database
#'RETURNS: A list of files that were not successfully uploaded
#'@params
#'parent_dir - The directory containing the site-specific folders of sas data files
#'substr_vec - A character vector used to aggregate tables
#'
upload_sites <- function(parent_dir, substr_names){
  child_dirs <- list.files(parent_dir)
  error_logs <- c()
  for (site in child_dirs) {
    config('results_schema', site)
    table_dir <- paste0(parent_dir, site)
    returned_filenames <- read_all_dfs(table_dir, site, error_logs)
    final_files <- upload_prep(returned_filenames, substr_names)
    for (t in union(returned_filenames, final_files)) {
      if (t %in% final_files) {
        get(t) %>% output_tbl(name=t, results_tag=FALSE)
      }
      rm(list=c(t),envir = .GlobalEnv)
    }
  }
  return(error_logs)
}

upload_combined <- function(parent_dir, 
                            res_schema_name='pcc_feas',
                            substr_names=c()
                            ) {
  config('results_schema',res_schema_name)
  child_dirs <- list.files(parent_dir)
  error_logs <- c()
  returned_filenames <- c()
  for (site in child_dirs) {
    table_dir <- paste0(parent_dir, site)
    local_rfs <- read_all_dfs(table_dir, site, error_logs)
    returned_filenames <- union(returned_filenames,local_rfs)
  }
  final_files <- upload_prep(returned_filenames, substr_names)
  for (t in union(returned_filenames, final_files)) {
    if (t %in% final_files) {
      get(t) %>% output_tbl(name=t, results_tag=FALSE)
      }
      rm(list=c(t),envir = .GlobalEnv)
  }
  return(error_logs)
}

