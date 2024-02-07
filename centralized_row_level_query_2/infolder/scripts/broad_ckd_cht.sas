/* The broad CKD cohort is defined as the union of:
Step 7 of the mild-to-moderate CKD computational phenotype attrition
CKD diagnosis cohort: Patients with >= 1 chronic kidney disease diagnosis, aged >=1 and <18 years
High serum creatinine cohort: Patients with >=2 serum creatinine measurements age and sex-specific thresholds, separated by >=90 days, aged >=1 and <18 years
*/


/* Create CKD dx cohort: Patients with at least 1 diagnosis from a CKD stages 2 & 3 codeset
cohort entry date (ce_date) is the first diagnosis
count of distinct dates with a diagnosis is also provided (dx_count)*/
proc sql;
create table local.ckd_dx_cohort as
select patid, min(coalesce(admit_date, dx_date)) as ce_date format=date9., count(distinct dx_date) as dx_count from
(select * from indata.diagnosis
where coalesce(admit_date, dx_date) between '01JAN2009'd and '31DEC2021'd) as cdm /*diagnosis table from pcornet cdm*/
inner join infolder.dx_ckd_allstages as codeset /*CKD stages 2 & 3 codeset*/
on cdm.dx=codeset.concept_code and cdm.dx_type=codeset.pcornet_vocabulary_id /*inner join on concept_code and dx_type*/
group by patid;
run;

/* Create table of serum creatinine measurements where result is based on age and sex specific thresholds
from table created in feas_query.sas table of serum creatinine measurements where patients aged >=1 and
< 18 years
*/
proc sql;
create table local.high_scr_meas as
select patid, lab_loinc, comb_lab_date, result_num, sc.sex, sc.age from
(select patid, lab_loinc, comb_lab_date, result_num, sex, age_at_measurement as age
from local.ser_creat) as sc
left join infolder.high_scr_lookup as lkp
on (sc.sex=lkp.sex and sc.age=lkp.age)
where result_num > serum_creatinine_high_mgdl;
run;

/* Create high serum creatinine cohort: Patients with at least 2 serum creatinine measurement
above the upper threshold, separated by 90 days */
proc sql;
create table local.high_scr_cohort as
select * from
(select patid,
min(comb_lab_date) as ce_date format=date9.,
max(comb_lab_date) as max_high_scr_date format=date9.,
count(distinct comb_lab_date) as high_scr_count
from local.high_scr_meas
group by patid)
where max_high_scr_date - ce_date >= 90;
run;

/* Compute overlap for high serum creatinine and CKD cohort */
proc sql;
create table work.comb_cohort_prelim1 as
select coalesce(ckd_dx.patid, high_scr.patid) as patid,
min(ckd_dx.ce_date, high_scr.ce_date) as ce_date format=date9.,
coalesce(ckd_dx, 0) as ckd_dx,
coalesce(high_scr, 0) as high_scr from
((select patid, ce_date, 1 as ckd_dx from local.ckd_dx_cohort) as ckd_dx
full join (select patid, ce_date, 1 as high_scr from local.high_scr_cohort) as high_scr
on ckd_dx.patid = high_scr.patid)
order by patid;
run;

/* Limit age to >= 1 and < 18 years at cohort entry*/
proc sql;
create table work.comb_cohort_prelim2 as
select * from
(select cht.patid,
ce_date,
coalesce(ckd_dx, 0) as ckd_dx,
coalesce(high_scr, 0) as high_scr,
birth_date,
(ce_date-birth_date)/365.25 as ce_age_years
from
work.comb_cohort_prelim1 as cht
inner join indata.demographic as dem
on cht.patid = dem.patid)
where ce_age_years < 18 and ce_age_years >= 1;
run;

/* Compute age field for ced_step7 */
proc sql;
create table local.ced_step7_age as
select step7.patid, ce_date,
(ce_date-birth_date)/365.25 as ce_age_years from
local.ced_step7 as step7
inner join indata.demographic as cdm
on step7.patid = cdm.patid;
run;


/* Compute overlap for high serum creatinine, CKD, and step 7 of the attrition */
proc sql;
create table local.comb_cohort as
select coalesce(ced_step7_age.patid, ckd_high_scr.patid) as patid,
coalesce(ced_step7_age.ce_date, ckd_high_scr.ce_date) as ce_date format=date9.,
year(coalesce(ced_step7_age.ce_date, ckd_high_scr.ce_date)) as ce_year,
floor(coalesce(ced_step7_age.ce_age_years, ckd_high_scr.ce_age_years)) as ce_age_years,
coalesce(ced_step7_age.attr_cht, 0) as attr_cht,
coalesce(high_scr, 0) as high_scr,
coalesce(ckd_dx, 0) as ckd_dx from
((select * from work.comb_cohort_prelim2) as ckd_high_scr
full join (select patid, ce_date, ce_age_years, 1 as attr_cht from local.ced_step7_age) as ced_step7_age
on ckd_high_scr.patid = ced_step7_age.patid)
order by patid;
run;
proc sql;
create index patid
on local.comb_cohort (patid);
run;
