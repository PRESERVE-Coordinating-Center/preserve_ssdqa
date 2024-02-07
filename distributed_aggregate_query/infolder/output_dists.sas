/************************/
/*     DEMOGRAPHICS     */
/************************/

/* Distribution of CED calendar year*/
proc sql;
create table output.dem_ce_yr as
select ce_year as ce_calendar_year, attr_cht, high_scr, ckd_stage23, count(distinct patid) as n_patids from local.comb_cohort
group by ce_calendar_year, attr_cht, high_scr, ckd_stage23;
run;

/* Distribution of CED year of age*/
proc sql;
create table output.dem_ce_age as
select ce_age_years, attr_cht, high_scr, ckd_stage23, count(distinct patid) as n_patids from local.comb_cohort
group by ce_age_years, attr_cht, high_scr, ckd_stage23;
run;

/* Distribution of birth calendar year*/
proc sql;
create table output.dem_birth_yr as
select birth_calendar_year, attr_cht, high_scr, ckd_stage23, count(distinct patid) as n_patids from
(select year(birth_date) as birth_calendar_year, patid, attr_cht, high_scr, ckd_stage23 from local.cht_demographic)
group by birth_calendar_year, attr_cht, high_scr, ckd_stage23;
run;

/* Distribution of demographic categories: Sex, Race, Ethnicity*/
proc sql;
create table output.dem_cats as
select sex, race, hispanic, attr_cht, high_scr, ckd_stage23, count(distinct patid) as n_patids from local.cht_demographic
group by sex, race, hispanic, attr_cht, high_scr, ckd_stage23;
run;

/************************/
/*     VISITS           */
/************************/

/* Distribution of encounter type per calendar year*/
proc sql;
create table output.enc_type_cts as
select enc_type, attr_cht, high_scr, ckd_stage23, year(admit_date) as year, count (distinct patid) as n_patids, count (distinct encounterid) as n_rows from local.cht_encounter
group by year, enc_type, attr_cht, high_scr, ckd_stage23
order by enc_type, year;
run;

/*Create table of in-person follow-up*/
proc sql;
create table local.enc_ip_fu_rows as
select distinct patid, attr_cht, high_scr, ckd_stage23, max(admit_date) as max_enc,
min(admit_date) as min_enc from local.cht_encounter
where enc_type in ('AV', 'ED', 'EI', 'IP', 'OS')
group by patid, attr_cht, high_scr, ckd_stage23;
run;

/* Distribution of in-person follow-up across attr_cht cohort */
proc sql;
create table output.enc_ip_fu_attr_cht as
select  count (distinct patid) as n_patids,
avg(max_enc - min_enc) as mean_days_ip_fu,
std(max_enc - min_enc) as sd_days_ip_fu,
median(max_enc - min_enc) as median_days_ip_fu,
min(max_enc - min_enc) as min_days_ip_fu,
max(max_enc - min_enc) as max_days_ip_fu
from local.enc_ip_fu_rows
where attr_cht = 1;
run;

/* Distribution of in-person follow-up across high_scr cohort */
proc sql;
create table output.enc_ip_fu_high_scr as
select  count (distinct patid) as n_patids,
avg(max_enc - min_enc) as mean_days_ip_fu,
std(max_enc - min_enc) as sd_days_ip_fu,
median(max_enc - min_enc) as median_days_ip_fu,
min(max_enc - min_enc) as min_days_ip_fu,
max(max_enc - min_enc) as max_days_ip_fu
from local.enc_ip_fu_rows
where high_scr = 1;
run;

/* Distribution of in-person follow-up across ckd_stage23 cohort */
proc sql;
create table output.enc_ip_fu_ckd_stage23 as
select  count (distinct patid) as n_patids,
avg(max_enc - min_enc) as mean_days_ip_fu,
std(max_enc - min_enc) as sd_days_ip_fu,
median(max_enc - min_enc) as median_days_ip_fu,
min(max_enc - min_enc) as min_days_ip_fu,
max(max_enc - min_enc) as max_days_ip_fu
from local.enc_ip_fu_rows
where ckd_stage23 = 1;
run;

/*Create table of ced to last in-person visit follow-up*/
proc sql;
create table local.enc_ced_fu_rows as
select distinct patid, attr_cht, high_scr, ckd_stage23, max(admit_date) as max_enc,
ce_date from local.cht_encounter
where enc_type in ('AV', 'ED', 'EI', 'IP', 'OS')
group by patid, attr_cht, high_scr, ckd_stage23;
run;

/* Distribution of ced to last in-person visit follow-up across attr_cht cohort */
proc sql;
create table output.enc_ced_fu_attr_cht as
select count (distinct patid) as n_patids,
avg(max_enc - ce_date) as mean_days_ced_fu,
std(max_enc - ce_date) as sd_days_ced_fu,
median(max_enc - ce_date) as median_days_ced_fu,
min(max_enc - ce_date) as min_days_ced_fu,
max(max_enc - ce_date) as max_days_ced_fu
from local.enc_ced_fu_rows
where attr_cht = 1;
run;

/* Distribution of ced to last in-person visit follow-up across high_scr cohort */
proc sql;
create table output.enc_ced_fu_high_scr as
select count (distinct patid) as n_patids,
avg(max_enc - ce_date) as mean_days_ced_fu,
std(max_enc - ce_date) as sd_days_ced_fu,
median(max_enc - ce_date) as median_days_ced_fu,
min(max_enc - ce_date) as min_days_ced_fu,
max(max_enc - ce_date) as max_days_ced_fu
from local.enc_ced_fu_rows
where high_scr = 1;
run;

/* Distribution of ced to last in-person visit follow-up across ckd_stage23 cohort */
proc sql;
create table output.enc_ced_fu_ckd_stage23 as
select count (distinct patid) as n_patids,
avg(max_enc - ce_date) as mean_days_ced_fu,
std(max_enc - ce_date) as sd_days_ced_fu,
median(max_enc - ce_date) as median_days_ced_fu,
min(max_enc - ce_date) as min_days_ced_fu,
max(max_enc - ce_date) as max_days_ced_fu
from local.enc_ced_fu_rows
where ckd_stage23 = 1;
run;

/************************/
/*     SPECIALTY        */
/************************/

/* Distributions of nephrology visits per calendar year*/
proc sql;
create table output.prov_neph as
select enc_type, attr_cht, high_scr, ckd_stage23, year(admit_date) as year, count (distinct patid) as n_patids, count (distinct encounterid) as n_rows from local.cht_encounter as enc
inner join local.cht_neph_provider as prov
on enc.providerid = prov.providerid
group by year, enc_type, attr_cht, high_scr, ckd_stage23
order by enc_type, year;
run;


/************************/
/*     VITALS           */
/************************/

/*Create table of height and weight measurements with year of age*/
proc sql;
create table local.vital_ht_wt_age_rows as
select vit.patid as patid, dem.attr_cht as attr_cht, dem.high_scr as high_scr, dem.ckd_stage23 as ckd_stage23, floor((measure_date - birth_date)/365.25) as age_years, ht, wt, measure_date, birth_date from
(select * from local.cht_vital
where (wt is not null or ht is not null)) as vit
inner join local.cht_demographic as dem
on vit.patid = dem.patid;
run;

/* Distributions of patients with weight measurements and number of measurements per calendar year*/
proc sql;
create table output.vital_wt_cts as
select attr_cht, high_scr, ckd_stage23, year(measure_date) as year, count (distinct patid) as n_patids, count (distinct vitalid) as n_rows from local.cht_vital
where wt is not null
group by year, attr_cht, high_scr, ckd_stage23
order by year;
run;

/* Distribution of weight values per patient year of age for attr_cht */
proc sql;
create table output.vital_wt_vals_attr_cht as
select distinct count (distinct patid) as n_patids,
age_years, avg(wt) as mean_weight_lb,
std(wt) as sd_weight_lb,
median(wt) as median_weight_lb,
min(wt) as min_weight_lb,
max(wt) as max_weight_lb from local.vital_ht_wt_age_rows
where wt is not null
and attr_cht = 1
group by age_years
order by age_years;
run;

/* Distribution of weight values per patient year of age for high_scr */
proc sql;
create table output.vital_wt_vals_high_scr as
select distinct count (distinct patid) as n_patids,
age_years, avg(wt) as mean_weight_lb, std(wt) as sd_weight_lb, median(wt) as median_weight_lb,
min(wt) as min_weight_lb,
max(wt) as max_weight_lb from local.vital_ht_wt_age_rows
where wt is not null
and high_scr = 1
group by age_years
order by age_years;
run;

/* Distribution of weight values per patient year of age for ckd_stage23 */
proc sql;
create table output.vital_wt_vals_ckd_stage23 as
select distinct count (distinct patid) as n_patids,
age_years, avg(wt) as mean_weight_lb, std(wt) as sd_weight_lb, median(wt) as median_weight_lb,
min(wt) as min_weight_lb,
max(wt) as max_weight_lb from local.vital_ht_wt_age_rows
where wt is not null
and ckd_stage23 = 1
group by age_years
order by age_years;
run;

/* Distributions of patients with height measurements and number of measurements per calendar year*/
proc sql;
create table output.vital_ht_cts as
select attr_cht, high_scr, ckd_stage23, year(measure_date) as year, count (distinct patid) as n_patids, count (distinct vitalid) as n_rows from local.cht_vital
where ht is not null
group by year, attr_cht, high_scr, ckd_stage23
order by year;
run;

/* Distribution of height values per patient year of age for attr_cht */
proc sql;
create table output.vital_ht_vals_attr_cht as
select count (distinct patid) as n_patids,
age_years, avg(ht) as mean_height_in, std(ht) as sd_height_in, median(ht) as median_height_in,
min(wt) as min_height_in,
max(wt) as max_height_in from local.vital_ht_wt_age_rows
where ht is not null
and attr_cht = 1
group by age_years
order by age_years;
run;

/* Distribution of height values per patient year of age for high_scr */
proc sql;
create table output.vital_ht_vals_high_scr as
select count (distinct patid) as n_patids,
age_years, avg(ht) as mean_height_in, std(ht) as sd_height_in, median(ht) as median_height_in,
min(wt) as min_height_in,
max(wt) as max_height_in from local.vital_ht_wt_age_rows
where ht is not null
and high_scr = 1
group by age_years
order by age_years;
run;

/* Distribution of height values per patient year of age for ckd_stage23 */
proc sql;
create table output.vital_ht_vals_ckd_stage23 as
select count (distinct patid) as n_patids,
age_years, avg(ht) as mean_height_in, std(ht) as sd_height_in, median(ht) as median_height_in,
min(wt) as min_height_in,
max(wt) as max_height_in from local.vital_ht_wt_age_rows
where ht is not null
and ckd_stage23 = 1
group by age_years
order by age_years;
run;

/*Create table of blood pressure measurements with year of age*/
proc sql;
create table local.vital_bp_age_rows as
select dem.attr_cht as attr_cht, dem.high_scr as high_scr, dem.ckd_stage23 as ckd_stage23, vit.patid as patid, floor((measure_date - birth_date)/365.25) as age_years, systolic, diastolic, measure_date, birth_date from
(select * from local.cht_vital
where (systolic is not null or diastolic is not null)) as vit
inner join local.cht_demographic as dem
on vit.patid = dem.patid;
run;

/* Distributions of patients and measurements per year for systolic blood pressure */
proc sql;
create table output.vital_sys_bp_cts as
select attr_cht, high_scr, ckd_stage23, year(measure_date) as year, count (distinct patid) as n_patids, count (distinct vitalid) as n_rows from local.cht_vital
where systolic is not null
group by year, attr_cht, high_scr, ckd_stage23
order by year;
run;

/* Distribution of systolic bp values per patient year of age for attr_cht */
proc sql;
create table output.vital_sys_bp_vals_attr_cht as
select count (distinct patid) as n_patids,
age_years, avg(systolic) as mean_sys_bp, std(systolic) as sd_sys_bp, median(systolic) as median_sys_bp,
min(systolic) as min_sys_bp,
max(systolic) as max_sys_bp from local.vital_bp_age_rows
where systolic is not null
and attr_cht = 1
group by age_years
order by age_years;
run;

/* Distribution of systolic bp values per patient year of age for high_scr */
proc sql;
create table output.vital_sys_bp_vals_high_scr as
select count (distinct patid) as n_patids,
age_years, avg(systolic) as mean_sys_bp, std(systolic) as sd_sys_bp, median(systolic) as median_sys_bp,
min(systolic) as min_sys_bp,
max(systolic) as max_sys_bp  from local.vital_bp_age_rows
where systolic is not null
and high_scr = 1
group by age_years
order by age_years;
run;

/* Distribution of systolic bp values per patient year of age for ckd_stage23 */
proc sql;
create table output.vital_sys_bp_vals_ckd_stage23 as
select count (distinct patid) as n_patids,
age_years, avg(systolic) as mean_sys_bp, std(systolic) as sd_sys_bp, median(systolic) as median_sys_bp,
min(systolic) as min_sys_bp,
max(systolic) as max_sys_bp  from local.vital_bp_age_rows
where systolic is not null
and ckd_stage23 = 1
group by age_years
order by age_years;
run;

/* Distributions of patients and measurements per year for diastolic blood pressure */
proc sql;
create table output.vital_dia_bp_cts as
select  attr_cht, high_scr, ckd_stage23, year(measure_date) as year, count (distinct patid) as n_patids, count (distinct vitalid) as n_rows from local.cht_vital
where diastolic is not null
group by year, attr_cht, high_scr, ckd_stage23
order by year;
run;


/* Distribution of diastolic bp values per patient year of age for attr_cht */
proc sql;
create table output.vital_dia_bp_vals_attr_cht as
select count (distinct patid) as n_patids,
age_years, avg(diastolic) as mean_dia_bp, std(diastolic) as sd_dia_bp, median(diastolic) as median_dia_bp,
min(diastolic) as min_dia_bp,
max(diastolic) as max_dia_bp from local.vital_bp_age_rows
where diastolic is not null
and attr_cht = 1
group by age_years
order by age_years;
run;

/* Distribution of diastolic bp values per patient year of age for high_scr */
proc sql;
create table output.vital_dia_bp_vals_high_scr as
select count (distinct patid) as n_patids,
age_years, avg(diastolic) as mean_dia_bp, std(diastolic) as sd_dia_bp, median(diastolic) as median_dia_bp,
min(diastolic) as min_dia_bp,
max(diastolic) as max_dia_bp from local.vital_bp_age_rows
where diastolic is not null
and high_scr = 1
group by age_years
order by age_years;
run;

/* Distribution of diastolic bp values per patient year of age for ckd_stage23 */
proc sql;
create table output.vital_dia_bp_vals_ckd_stage23 as
select count (distinct patid) as n_patids,
age_years, avg(diastolic) as mean_dia_bp, std(diastolic) as sd_dia_bp, median(diastolic) as median_dia_bp,
min(diastolic) as min_dia_bp,
max(diastolic) as max_dia_bp from local.vital_bp_age_rows
where diastolic is not null
and ckd_stage23 = 1
group by age_years
order by age_years;
run;

/************************/
/*     CONDITIONS       */
/************************/

/*Diagnosis: Counts of patients and entries per year for each dx code*/
proc sql;
create table output.diag_dx_cts as
select attr_cht, high_scr, ckd_stage23, dx, dx_type, year(dx_date) as year,
count (distinct patid) as n_patids, count (distinct diagnosisid) as n_rows from local.cht_diagnosis
group by year, dx, dx_type, attr_cht, high_scr, ckd_stage23
order by dx, year;
run;

/*Diagnosis: Counts of patients and entries per year for each codeset*/
proc sql;
create table output.diag_codeset_cts as
select attr_cht, high_scr, ckd_stage23, codeset, year(dx_date) as year,
count (distinct patid) as n_patids, count (distinct diagnosisid) as n_rows from local.cht_diagnosis
group by year, codeset, attr_cht, high_scr, ckd_stage23
order by year;
run;

/*Condition: Counts of patients and entries per year for each condition code*/
proc sql;
create table output.cond_dx_cts as
select attr_cht, high_scr, ckd_stage23, condition, condition_type, year(report_date) as year,
count (distinct patid) as n_patids, count (distinct conditionid) as n_rows from local.cht_condition
group by year, condition, condition_type, attr_cht, high_scr, ckd_stage23
order by condition, year;
run;

/*Condition: Counts of patients and entries per year for each codeset*/
proc sql;
create table output.cond_codeset_cts as
select attr_cht, high_scr, ckd_stage23, codeset, year(report_date) as year,
count (distinct patid) as n_patids, count (distinct conditionid) as n_rows from local.cht_condition
group by year, codeset, attr_cht, high_scr, ckd_stage23
order by year;
run;

/************************/
/*     DRUGS            */
/************************/

/*Prescribing: Counts of patients and entries per year for each key drug code*/
proc sql;
create table output.pres_rx_cts as
select attr_cht, high_scr, ckd_stage23, rxnorm_cui, year(coalesce(rx_start_date, rx_order_date)) as year,
count (distinct patid) as n_patids, count (distinct prescribingid) as n_rows from local.cht_prescribing
group by year, rxnorm_cui, attr_cht, high_scr, ckd_stage23
order by rxnorm_cui, year;
run;

/*Prescribing: Counts of patients and entries per year for each drug codeset*/
proc sql;
create table output.pres_codeset_cts as
select attr_cht, high_scr, ckd_stage23, codeset, year(coalesce(rx_start_date, rx_order_date)) as year,
count (distinct patid) as n_patids, count (distinct prescribingid) as n_rows from local.cht_prescribing
group by year, codeset, attr_cht, high_scr, ckd_stage23
order by year;
run;

/* Dispensing: Counts of patients and entries per year for each key drug code*/
proc sql;
create table output.disp_rx_cts as
select attr_cht, high_scr, ckd_stage23, ndc, year(dispense_date) as year,
count (distinct patid) as n_patids,
count (distinct dispensingid) as n_rows from local.cht_dispensing
group by year, ndc, attr_cht, high_scr, ckd_stage23
order by ndc, year;
run;

/* Dispensing: Counts of patients and entries per year for each drug codeset*/
proc sql;
create table output.disp_codeset_cts as
select attr_cht, high_scr, ckd_stage23, codeset, year(dispense_date) as year,
count (distinct patid) as n_patids,
count (distinct dispensingid) as n_rows from local.cht_dispensing
group by year, codeset, attr_cht, high_scr, ckd_stage23
order by codeset, year;
run;

/* Med admin: Counts of patients and entries per year for each key drug code*/
proc sql;
create table output.med_rx_cts as
select attr_cht, high_scr, ckd_stage23, year(medadmin_start_date) as year, medadmin_code, medadmin_type,
count (distinct patid) as n_patids, count (distinct medadminid) as n_rows from local.cht_med_admin
group by year, medadmin_code, medadmin_type, attr_cht, high_scr, ckd_stage23
order by medadmin_code, medadmin_type, year;
run;

/* Med admin: Counts of patients and entries per year for each drug codeset*/
proc sql;
create table output.med_codeset_cts as
select attr_cht, high_scr, ckd_stage23, year(medadmin_start_date) as year, codeset,
count (distinct patid) as n_patids, count (distinct medadminid) as n_rows from local.cht_med_admin
group by year, codeset, attr_cht, high_scr, ckd_stage23
order by year;
run;


/************************/
/*     PROCEDURES       */
/************************/

/*
Counts of patients and procedures per year for each key px code
*/
proc sql;
create table output.proc_px_cts as
select attr_cht, high_scr, ckd_stage23, px, px_type, year(px_date) as year,
count (distinct patid) as n_patids, count (distinct proceduresid) as n_rows from local.cht_procedures
group by year, px, px_type, attr_cht, high_scr, ckd_stage23
order by px, year;
run;

/*
Counts of patients and procedures per year for each px codeset
*/
proc sql;
create table output.proc_codeset_cts as
select attr_cht, high_scr, ckd_stage23, codeset, year(px_date) as year,
count (distinct patid) as n_patids, count (distinct proceduresid) as n_rows from local.cht_procedures
group by year, codeset, attr_cht, high_scr, ckd_stage23
order by year;
run;

/************************/
/*     LABS             */
/************************/

/*
Counts of patients and measurements per year for each key lab_loinc code
*/
proc sql;
create table output.lab_loinc_cts as
select attr_cht, high_scr, ckd_stage23, lab_loinc, year(comb_lab_date) as year,
count (distinct patid) as n_patids, count (distinct lab_result_cm_id) as n_rows from local.cht_lab_result_cm
group by year, lab_loinc, attr_cht, high_scr, ckd_stage23
order by lab_loinc, year;
run;

/*
Counts of patients and measurements per year for each lab_loinc codeset
*/
proc sql;
create table output.lab_codeset_cts as
select distinct attr_cht, high_scr, ckd_stage23, codeset, year(comb_lab_date) as year,
count (distinct patid) as n_patids, count (distinct lab_result_cm_id) as n_rows from local.cht_lab_result_cm
group by year, lab_loinc, attr_cht, high_scr, ckd_stage23
order by codeset, year;
run;

/*
Counts of result_qual and result_snomed provided for each key lab_loinc code
*/
proc sql;
create table output.lab_vals1 as
select attr_cht, high_scr, ckd_stage23, lab_loinc, result_qual, result_snomed, count (distinct patid) as n_patids,
count (distinct lab_result_cm_id) as n_rows from local.cht_lab_result_cm
group by lab_loinc, result_qual, result_snomed, attr_cht, high_scr, ckd_stage23
order by lab_loinc, result_qual, result_snomed;
run;

/*
result_num summary statistics for each result_unit and key lab_loinc code combination for attr_cht
*/
proc sql;
create table output.lab_vals2_attr_cht as
select count (distinct patid) as n_patids,
lab_loinc, result_unit, avg(result_num) as mean_result_num,
median(result_num) as median_result_num,
std(result_num) as sd_result_num,
min(result_num) as min_result_num,
max(result_num) as max_result_num,
count (distinct lab_result_cm_id) as n_rows,
count (distinct patid) as n_patids from local.cht_lab_result_cm
where attr_cht = 1
group by lab_loinc, result_unit;
run;

/*
result_num summary statistics for each result_unit and key lab_loinc code combination for high_scr
*/
proc sql;
create table output.lab_vals2_high_scr as
select count (distinct patid) as n_patids,
lab_loinc, result_unit, avg(result_num) as mean_result_num,
median(result_num) as median_result_num,
std(result_num) as sd_result_num,
min(result_num) as min_result_num,
max(result_num) as max_result_num,
count (distinct lab_result_cm_id) as n_rows,
count (distinct patid) as n_patids from local.cht_lab_result_cm
where high_scr = 1
group by lab_loinc, result_unit;
run;

/*
result_num summary statistics for each result_unit and key lab_loinc code combination for ckd_stage23
*/
proc sql;
create table output.lab_vals2_ckd_stage23 as
select count (distinct patid) as n_patids,
lab_loinc, result_unit, avg(result_num) as mean_result_num,
median(result_num) as median_result_num,
std(result_num) as sd_result_num,
min(result_num) as min_result_num,
max(result_num) as max_result_num,
count (distinct lab_result_cm_id) as n_rows,
count (distinct patid) as n_patids from local.cht_lab_result_cm
where ckd_stage23 = 1
group by lab_loinc, result_unit;
run;


/*
Counts of normal range information provided for each result_unit and key lab_loinc code combination
*/
proc sql;
create table output.lab_vals3 as
select attr_cht, high_scr, ckd_stage23, lab_loinc, result_unit, norm_range_high, norm_range_low,
count (distinct patid) as n_patids, count (distinct lab_result_cm_id) as n_rows from local.cht_lab_result_cm
group by lab_loinc, result_unit, norm_range_high, norm_range_low, attr_cht, high_scr, ckd_stage23;
run;

/*
Counts of raw_result to result_qual mapping for each key lab_loinc code
Remove rows where n_patids < 3 due to inclusion of raw field in output table
*/
proc sql;
create table output.lab_vals4 as
select * from
(select attr_cht, high_scr, ckd_stage23, lab_loinc, concept_name, raw_result, result_qual,
count (distinct patid) as n_patids, count (distinct lab_result_cm_id) as n_rows from
((select attr_cht, high_scr, ckd_stage23, patid, lab_loinc, raw_result, result_qual, lab_result_cm_id from local.cht_lab_result_cm
where result_qual is not null
and not result_qual = 'OT') as cdm
inner join local.all_labs as codeset
on cdm.lab_loinc=codeset.concept_code)
group by lab_loinc, concept_name, result_qual, raw_result, attr_cht, high_scr, ckd_stage23)
where n_patids >= 3;
run;

/*
Counts of raw_result to result_qual mapping for each key lab_loinc code
Remove rows where n_patids < 3 due to inclusion of raw field in output table
*/
proc sql;
create table output.lab_vals5 as
select * from
(select attr_cht, high_scr, ckd_stage23, lab_loinc, raw_lab_name,
count (distinct patid) as n_patids, count (distinct lab_result_cm_id) as n_rows from local.cht_lab_result_cm
group by lab_loinc, raw_lab_name, attr_cht, high_scr, ckd_stage23)
where n_patids >= 3;
run;

/*
result_num missingness count
*/
proc sql;
create table output.lab_vals6 as
select lab_loinc, count (distinct lab_result_cm_id) as n_rows from local.cht_lab_result_cm
where result_unit is null
group by lab_loinc;
run;

/*
norm_range_low missingness count
*/
proc sql;
create table output.lab_vals7 as
select lab_loinc, count (distinct lab_result_cm_id) as n_rows from local.cht_lab_result_cm
where norm_range_low is null
group by lab_loinc;
run;

/*
norm_range_high missingness count
*/
proc sql;
create table output.lab_vals8 as
select lab_loinc, count (distinct lab_result_cm_id) as n_rows from local.cht_lab_result_cm
where norm_range_high is null
group by lab_loinc;
run;
