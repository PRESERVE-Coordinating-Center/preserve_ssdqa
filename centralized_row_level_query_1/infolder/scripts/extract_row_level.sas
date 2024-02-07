/* Row level data for the following tables, restricted to patients in the union of the
three chronic kidney disease cohorts (the "broad CKD cohort"), study period, and specified fields.
1.	condition
2.	death
3.	death_cause
4.	demographic
5.	diagnosis
6.	dispensing
7.	encounter
8.	enrollment
9.	lab_result_cm
10.	med_admin
11.	obs_clin (additionally restricted to birth weight)
12.	obs_gen_gest_age (obs_gen additionally restricted to gestational age)
13.	prescribing
14.	procedures
15.	provider
16.	vital
*/

libname output "&querypkg.output_row";

/* create table for logging counts */
proc sql;
create table output.row_counts
(cdm_table varchar(50), n_patids int, n_rows int);
run;

/* output combined cohort */
proc sql;
create table output.broad_ckd_cht_info as
select *
from local.comb_cohort;
run;
proc sql;
create index patid
on output.broad_ckd_cht_info (patid);
run;


/* create patid only cohort table */
proc sql;
create table local.broad_ckd_cht as
select distinct patid
from local.comb_cohort;
run;
proc sql;
create index patid
on local.broad_ckd_cht (patid);
run;

/*
demographic
*/
proc sql;
create table output.demographic as
select cht.patid, birth_date, birth_time, sex, hispanic, race
from indata.demographic as cdm
inner join local.broad_ckd_cht as cht
on cht.patid = cdm.patid;
run;
/* log counts */
proc sql;
insert into output.row_counts
SELECT 'demographic' as cdm_table, COUNT(DISTINCT patid) as n_patids, COUNT(*) as n_rows
FROM output.demographic;
run;

/*
enrollment
*/
proc sql;
create table output.enrollment as
select cht.patid, enr_start_date, enr_end_date, enr_basis
from indata.enrollment as cdm
inner join local.broad_ckd_cht as cht
on cht.patid = cdm.patid;
run;
/*
log counts
*/
proc sql;
insert into output.row_counts
SELECT 'enrollment' as cdm_table, COUNT(DISTINCT patid) as n_patids, COUNT(*) as n_rows
FROM output.enrollment;
run;

/*
encounter
*/
proc sql;
create table output.encounter as
select encounterid, cht.patid, admit_date, admit_time, discharge_date, discharge_time, providerid,
enc_type, facilityid, discharge_disposition, discharge_status, drg, drg_type, admitting_source,
payer_type_primary, payer_type_secondary, facility_type, raw_facility_type
from indata.encounter as cdm
inner join local.broad_ckd_cht as cht
on cht.patid = cdm.patid
where admit_date between '01JAN2009'd and '31DEC2021'd;
run;
/*
log counts
*/
proc sql;
insert into output.row_counts
SELECT 'encounter' as cdm_table, COUNT(DISTINCT patid) as n_patids, COUNT(*) as n_rows
FROM output.encounter;
run;

/*
diagnosis
*/
proc sql;
create table output.diagnosis as
select diagnosisid, cht.patid, encounterid, enc_type, admit_date, providerid, dx, dx_type,
dx_date, dx_source, dx_origin, pdx, dx_poa, raw_dx
from indata.diagnosis as cdm
inner join local.broad_ckd_cht as cht
on cht.patid = cdm.patid
where coalesce(admit_date, dx_date) between '01JAN2009'd and '31DEC2021'd;
run;
/*
log counts
*/
proc sql;
insert into output.row_counts
SELECT 'diagnosis' as cdm_table, COUNT(DISTINCT patid) as n_patids, COUNT(*) as n_rows
FROM output.diagnosis;
run;

/*
procedures
*/
proc sql;
create table output.procedures as
select proceduresid, cht.patid, encounterid, enc_type, admit_date, providerid, px_date,
px, px_type, px_source, ppx, raw_px, raw_px_type
from indata.procedures as cdm
inner join local.broad_ckd_cht as cht
on cht.patid = cdm.patid
where coalesce(admit_date, px_date) between '01JAN2009'd and '31DEC2021'd;
run;
/*
log counts
*/
proc sql;
insert into output.row_counts
SELECT 'procedures' as cdm_table, COUNT(DISTINCT patid) as n_patids, COUNT(*) as n_rows
FROM output.procedures;
run;

/*
vital
*/
proc sql;
create table output.vital as
select vitalid, cht.patid, encounterid, measure_date, measure_time, vital_source, ht, wt,
diastolic, systolic, original_bmi, bp_position, smoking, tobacco, tobacco_type,
raw_diastolic, raw_systolic, raw_bp_position
from indata.vital as cdm
inner join local.broad_ckd_cht as cht
on cht.patid = cdm.patid
where measure_date between '01JAN2009'd and '31DEC2021'd;
run;
/*
log counts
*/
proc sql;
insert into output.row_counts
SELECT 'vital' as cdm_table, COUNT(DISTINCT patid) as n_patids, COUNT(*) as n_rows
FROM output.vital;
run;

/*
dispensing
*/
proc sql;
create table output.dispensing as
select dispensingid, cht.patid, prescribingid, dispense_date, ndc, dispense_source,
dispense_sup, dispense_amt, dispense_dose_disp, dispense_dose_disp_unit, dispense_route,
raw_ndc
from indata.dispensing as cdm
inner join local.broad_ckd_cht as cht
on cht.patid = cdm.patid
where dispense_date between '01JAN2009'd and '31DEC2021'd;
run;
/*
log counts
*/
proc sql;
insert into output.row_counts
SELECT 'dispensing' as cdm_table, COUNT(DISTINCT patid) as n_patids, COUNT(*) as n_rows
FROM output.dispensing;
run;

/*
lab_result_cm
*/
proc sql;
create table output.lab_result_cm as
select lab_result_cm_id, cht.patid, encounterid, specimen_source, lab_loinc, lab_result_source,
lab_loinc_source, priority, result_loc, lab_px, lab_px_type, lab_order_date, specimen_date,
specimen_time, result_date, result_time, result_qual, result_snomed, result_num, result_modifier,
result_unit, norm_range_low, norm_modifier_low, norm_range_high, norm_modifier_high,
abn_ind, raw_lab_code, raw_result, raw_unit
from indata.lab_result_cm as cdm
inner join local.broad_ckd_cht as cht
on cht.patid = cdm.patid
where coalesce(specimen_date, result_date, lab_order_date) between '01JAN2009'd and '31DEC2021'd;
run;
/*
log counts
*/
proc sql;
insert into output.row_counts
SELECT 'lab_result_cm' as cdm_table, COUNT(DISTINCT patid) as n_patids, COUNT(*) as n_rows
FROM output.lab_result_cm;
run;

/*
condition
*/
proc sql;
create table output.condition as
select conditionid, cht.patid, encounterid, report_date, resolve_date, onset_date, condition_status,
condition, condition_type, condition_source, raw_condition, raw_condition_type
from indata.condition as cdm
inner join local.broad_ckd_cht as cht
on cht.patid = cdm.patid
where coalesce(onset_date, report_date, resolve_date) between '01JAN2009'd and '31DEC2021'd;
run;
/*
log counts
*/
proc sql;
insert into output.row_counts
SELECT 'condition' as cdm_table, COUNT(DISTINCT patid) as n_patids, COUNT(*) as n_rows
FROM output.condition;
run;

/*
prescribing
*/
proc sql;
create table output.prescribing as
select prescribingid, cht.patid, encounterid, rx_providerid, rx_order_date, rx_order_time,
rx_start_date, rx_end_date, rx_dose_ordered, rx_dose_ordered_unit, rx_quantity,
rx_dose_form, rx_refills, rx_days_supply, rx_frequency, rx_prn_flag, rx_route, rx_basis,
rxnorm_cui, rx_source, rx_dispense_as_written, raw_rxnorm_cui
from indata.prescribing as cdm
inner join local.broad_ckd_cht as cht
on cht.patid = cdm.patid
where coalesce(rx_order_date, rx_start_date) between '01JAN2009'd and '31DEC2021'd;
run;
/*
log counts
*/
proc sql;
insert into output.row_counts
SELECT 'prescribing' as cdm_table, COUNT(DISTINCT patid) as n_patids, COUNT(*) as n_rows
FROM output.prescribing;
run;

/*
death
*/
proc sql;
create table output.death as
select cht.patid, death_date, death_date_impute, death_source, death_match_confidence
from indata.death as cdm
inner join local.broad_ckd_cht as cht
on cht.patid = cdm.patid
where death_date between '01JAN2009'd and '31DEC2021'd;
run;
/*
log counts
*/
proc sql;
insert into output.row_counts
SELECT 'death' as cdm_table, COUNT(DISTINCT patid) as n_patids, COUNT(*) as n_rows
FROM output.death;
run;

/*
death_cause (restricted to patients in death table)
*/
proc sql;
create table output.death_cause as
select death.patid, death_cause, death_cause_code, death_cause_type, death_cause_source,
death_cause_confidence
from indata.death_cause as cdm
inner join output.death as death
on death.patid = cdm.patid;
run;
/*
log counts
*/
proc sql;
insert into output.row_counts
SELECT 'death_cause' as cdm_table, COUNT(DISTINCT patid) as n_patids, COUNT(*) as n_rows
FROM output.death_cause;
run;

/*
med_admin
*/
proc sql;
create table output.med_admin as
select medadminid, cht.patid, encounterid, prescribingid, medadmin_providerid, medadmin_start_date,
medadmin_start_time, medadmin_stop_date, medadmin_stop_time, medadmin_type, medadmin_code,
medadmin_dose_admin, medadmin_dose_admin_unit, medadmin_route, medadmin_source,
raw_medadmin_code
from indata.med_admin as cdm
inner join local.broad_ckd_cht as cht
on cht.patid = cdm.patid
where coalesce(medadmin_start_date, medadmin_stop_date) between '01JAN2009'd and '31DEC2021'd;
run;
/*
log counts
*/
proc sql;
insert into output.row_counts
SELECT 'med_admin' as cdm_table, COUNT(DISTINCT patid) as n_patids, COUNT(*) as n_rows
FROM output.med_admin;
run;

/*
temporary table for obs_clin (restricted to birth weight)
*/
proc sql;
create table work.temp_obs_clin as
select obsclinid, patid, encounterid, obsclin_providerid, obsclin_start_date,
obsclin_start_time, obsclin_stop_date, obsclin_stop_time, obsclin_type, obsclin_code,
obsclin_result_qual, obsclin_result_snomed, obsclin_result_num,
obsclin_result_modifier, obsclin_result_unit, obsclin_source, obsclin_abn_ind,
raw_obsclin_code, raw_obsclin_type
from indata.obs_clin
where obsclin_code = '8339-4';
run;


/*
obs_clin (restricted to birth weight)
*/
proc sql;
create table output.obs_clin as
select obsclinid, cht.patid, encounterid, obsclin_providerid, obsclin_start_date,
obsclin_start_time, obsclin_stop_date, obsclin_stop_time, obsclin_type, obsclin_code,
obsclin_result_qual, obsclin_result_snomed, obsclin_result_num,
obsclin_result_modifier, obsclin_result_unit, obsclin_source, obsclin_abn_ind,
raw_obsclin_code, raw_obsclin_type from work.temp_obs_clin as cdm
inner join local.broad_ckd_cht as cht
on cht.patid = cdm.patid;
run;
/*
log counts
*/
proc sql;
insert into output.row_counts
SELECT 'obs_clin' as cdm_table, COUNT(DISTINCT patid) as n_patids, COUNT(*) as n_rows
FROM output.obs_clin;
run;

/*
obs_gen (restricted to gestational age)
*/
proc sql;
create table output.obs_gen_gest_age as
select obsgenid, cht.patid, encounterid, obsgen_providerid, obsgen_start_date,
obsgen_start_time, obsgen_stop_date, obsgen_stop_time, obsgen_type, obsgen_code,
obsgen_result_qual, obsgen_result_text, obsgen_result_num, obsgen_result_unit,
obsgen_table_modified, obsgen_id_modified, obsgen_source, obsgen_abn_ind,
raw_obsgen_code, raw_obsgen_type, raw_obsgen_result, raw_obsgen_unit
from indata.obs_gen as cdm
inner join local.broad_ckd_cht as cht
on cht.patid = cdm.patid
where obsgen_code = '18185-9'
or lower(raw_obsgen_name) like '%gestational age%';
run;
/*
log counts
*/
proc sql;
insert into output.row_counts
SELECT 'obs_gen_gest_age' as cdm_table, COUNT(DISTINCT patid) as n_patids, COUNT(*) as n_rows
FROM output.obs_gen_gest_age;
run;

/* create union of all relevant providerids */
create table local.providerid_cohort as
select distinct providerid from
(select providerid from output.encounter
union
select providerid from output.diagnosis
union
select providerid from output.procedures
union
select rx_providerid as providerid from output.prescribing
union
select medadmin_providerid as providerid from output.med_admin
union
select obsclin_providerid as providerid from output.obs_clin);

/*
provider
*/
proc sql;
create table output.provider as
select cht.providerid, provider_specialty_primary, raw_provider_specialty_primary
from indata.provider as cdm
inner join local.providerid_cohort as cht
on cht.providerid = cdm.providerid;
run;
/*
log counts
*/
proc sql;
insert into output.row_counts
SELECT 'provider' as cdm_table, 0 as n_patids, COUNT(*) as n_rows
FROM output.provider;
run;
