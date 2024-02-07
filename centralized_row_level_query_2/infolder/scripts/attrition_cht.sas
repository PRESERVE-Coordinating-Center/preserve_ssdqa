/*
Implementation of the mild-to-moderate chronic kidney disease phenotype, implemented as follows:
0. Patients with >=1 visit between January 2009 and December 2021
1. Patients with >=1 in-person visit between January 2009 and December 2021
2. Patients with >=1 serum creatinine measurement
3. Patients aged >=1 and < 18 at time of >=1 serum creatinine measurement
4. Patients with height measurement available <=180 days of serum creatinine value (aged >=1 and < 18)
5. Patients with >=1 eGFR value >=30 and <90mL/min/1.73m2 (aged >=1 and < 18)
6. Patients with >=2 eGFRs >=30 and <90mL/min/1.73m2 which are >=90 days apart (aged >=1 and < 18)
7. Patients with >=2 eGFRs >=30 and <90mL/min/1.73m2 which are >=90 days apart, without an intervening eGFR value >= 90 mL/min/1.73m2 (aged >=1 and < 18)
8. Exclude patients with no in-person visits with a nephrology provider or facility at any time between January 2009 and December 2021
9. Exclude patients with >=1 chronic dialysis procedure or associated diagnosis on or before cohort entry date
10. Exclude patients with >=1 kidney transplant procedure or associated diagnosis on or before cohort entry date
*/

/*
Create empty attrition table for logging counts at each step
*/
proc sql;
create table output.attrition
(step varchar(100), step_num varchar(3), count int);
run;

/*
Denominator count: Log count of patients in demographic table
*/
proc sql;
insert into output.attrition
SELECT 'Denominator' as step, 'NA' as step_num, COUNT(DISTINCT patid) as count
FROM indata.demographic;
run;

/*
Step 0: Log count of patients with >=1 visit between January 2009 and December 2021
*/
proc sql;
insert into output.attrition
SELECT 'Step 0 - >=1 visit' as step, '0' as step_num, COUNT(DISTINCT patid) as count
FROM indata.encounter
WHERE admit_date between '01JAN2009'd and '31DEC2021'd;
run;

/*
Step 1: Log count of patients with >=1 *in-person* visit between January 2009 and December 2021
*/
proc sql;
insert into output.attrition
SELECT 'Step 1 - >=1 in-person visit' as step, '1' as step_num, COUNT(DISTINCT patid) as count
FROM indata.encounter
WHERE (admit_date between '01JAN2009'd and '31DEC2021'd)
AND (enc_type in ('AV', 'ED', 'EI', 'IP', 'OS'));
run;

/* 
Create temporary lab_result_cm table, which can be merged with the serum_creatinine codeset
to make our output serum creatinine table, queried for potentially unmapped serum creatinine labs
and used to generate distributions of key variables
*/
proc sql;
create table work.temp_lab_result_cm as 
select labs.patid, sex, comb_lab_date,
lab_loinc, result_num, 
result_unit, norm_range_high, norm_modifier_high,
raw_lab_name, raw_lab_code, raw_result, specimen_source,
norm_modifier_low, norm_range_low, result_modifier,
result_qual, result_snomed,
lab_result_cm_id, birth_date from 
(select patid, coalesce(specimen_date, result_date, lab_order_date) as comb_lab_date,
lab_loinc, result_num, 
result_unit, norm_range_high, norm_modifier_high,
raw_lab_name, raw_lab_code, raw_result, specimen_source,
norm_modifier_low, norm_range_low, result_modifier,
result_qual, result_snomed,
lab_result_cm_id
FROM indata.lab_result_cm
WHERE coalesce(specimen_date, result_date, lab_order_date) between '01JAN2009'd and '31DEC2021'd) as labs 
inner join (SELECT patid, sex, birth_date
FROM indata.demographic) as demo on labs.patid=demo.patid;


/*
Step 2: Log count of patients with >=1 serum creatinine measurement
*/
proc sql;
insert into output.attrition
SELECT 'Step 2 - >=1 serum creatinine meas' as step, '2' as step_num, COUNT(DISTINCT patid) as count 
from (select patid, lab_loinc FROM work.temp_lab_result_cm
where result_num <= 50
and result_num >0) as cdm
inner join (select concept_code from infolder.serum_creatinine) as codeset
on cdm.lab_loinc = codeset.concept_code;
run;

/*
Create local serum creatinine table of measurements where patients aged >=1 and < 18 years
Restrict serum creatinine values to <= 50 and >0 (plausible bounds)
*/
proc sql;
create table local.ser_creat as
select patid, sex, comb_lab_date,
lab_loinc, result_num, 
result_unit, norm_range_high, norm_modifier_high,
raw_lab_name, raw_lab_code, raw_result,
lab_result_cm_id, round((comb_lab_date-birth_date)/365.25) as age_at_measurement
from 
(select * from work.temp_lab_result_cm
where result_num <= 50
and result_num >0
and comb_lab_date-birth_date >= 365
and comb_lab_date-birth_date < 365.25*18) as cdm
inner join (select concept_code from infolder.serum_creatinine) as codeset
on cdm.lab_loinc=codeset.concept_code;
run;


/*
Step 3: Log count of patients aged >=1 and < 18 at time of >=1 serum creatinine measurement
*/
proc sql;
insert into output.attrition
SELECT 'Step 3 - meets age criteria at serum creatinine meas' as step, '3' as step_num, COUNT(DISTINCT patid) as count
FROM local.ser_creat;
run;

/*
Create local table of heights for cohort of patients with >= 1 value in serum creatinine table
*/
proc sql;
create table local.heights as
select distinct y.patid, comb_lab_date, measure_date, ht
from (select h.patid as patid, comb_lab_date, min(abs(comb_lab_date - measure_date)) as days from
(select patid, comb_lab_date
from local.ser_creat) as x 
inner join (SELECT patid, measure_date, ht
			FROM indata.vital
			WHERE ht IS NOT null) as h 
on x.patid=h.patid
where abs(comb_lab_date - measure_date) <= 180
group by h.patid, comb_lab_date) as y
inner join(SELECT patid, measure_date, ht
			FROM indata.vital
			WHERE ht IS NOT null) as z
on y.patid=z.patid
where abs(comb_lab_date - measure_date) = days;
run;


/*
Step 4: Log count of patients with height measurement available <=180 days of
serum creatinine value (aged >=1 and < 18)
*/
proc sql;
insert into output.attrition
SELECT 'Step 4 - accompanying height measurement' as step, '4' as step_num, COUNT(DISTINCT patid) as count
FROM local.heights;
run;

/*
Create temporary table of heights and ser_creat to merge with egfr_lookup
*/
proc sql;
create table work.heights_ser_creat as
select heights.patid as patid, measure_date, ht*0.0254 as ht_meters,
ser_creat.comb_lab_date, result_num, sex, age_at_measurement
from local.heights inner join local.ser_creat 
on (heights.patid=ser_creat.patid and heights.comb_lab_date=ser_creat.comb_lab_date);
run;

/*
Create local eGFR table using CKiD U25 serum creatinine equation, restricted to values 30-90 mL/min/1.73m2
*/
proc sql;
create table local.egfr as
select heights_ser_creat.patid as patid, measure_date,
comb_lab_date, ht_meters, heights_ser_creat.sex as sex, age_at_measurement as age,
k_coeff, result_num, k_coeff*(ht_meters/result_num) as egfr
from work.heights_ser_creat left join infolder.age_sex_bounds 
on (heights_ser_creat.sex=age_sex_bounds.sex and heights_ser_creat.age_at_measurement=age_sex_bounds.age)
where ht_meters between height_meters_upper and height_meters_lower
and k_coeff*(ht_meters/result_num) >= 30
and k_coeff*(ht_meters/result_num) < 90;
run;


/*
Step 5: Log count of patients with >=1 eGFR value >=30 and <90 mL/min/1.73m2 (aged >=1 and < 18)
*/
proc sql;
insert into output.attrition
SELECT 'Step 5 - egfr within range (30-90)' as step, '5' as step_num, COUNT(DISTINCT patid) as count
from local.egfr;
run;


/*
Step 6: Patients with >=2 eGFRs >=30 and <90 mL/min/1.73m2 which are >=90 days apart (aged >=1 and < 18)
*/
proc sql;
insert into output.attrition
SELECT 'Step 6 - 2 egfrs within range' as step, '6' as step_num, COUNT(DISTINCT l.patid) as count
FROM (
(select patid, comb_lab_date as first_date
from local.egfr) as l 
inner join (select patid, comb_lab_date as second_date
from local.egfr) as r
on l.patid=r.patid and r.second_date-l.first_date>=90);
run;


/*
Create table of cohort entry dates for patients with 
2 eGFRs 30-90 mL/min/1.73m2 separated by 90 days from egfr table,
without an intervening egfr >= 90mL/min/1.73m2
The cohort entry date (CED) is defined as
the date of the first eGFR of the earliest pair of eGFRs >=30 & <90 mL/min/1.73m2, 
separated by >=90 days, without an intervening eGFR >=90
*/
proc sql;
create table local.ced_step7 as
select u_ninety.patid, min(u_ninety.first_date) as ce_date format=date9. from
(select l.patid as patid, l.comb_lab_date as first_date, r.comb_lab_date as second_date
from local.egfr as l inner join local.egfr as r 
on l.patid=r.patid and r.comb_lab_date - l.comb_lab_date >= 90) as u_ninety
left join (
select heights_ser_creat.patid as patid, 
heights_ser_creat.comb_lab_date
from work.heights_ser_creat left join infolder.age_sex_bounds 
on (heights_ser_creat.sex=age_sex_bounds.sex and heights_ser_creat.age_at_measurement=age_sex_bounds.age)
where ht_meters between height_meters_upper and height_meters_lower
and k_coeff*(ht_meters/result_num) >=90) as o_ninety
on u_ninety.patid=o_ninety.patid 
and o_ninety.comb_lab_date between u_ninety.first_date and u_ninety.second_date
where missing(o_ninety.comb_lab_date)
group by u_ninety.patid;


/*
Step 7: Log count of patients with >=2 eGFRs >=30 and <90 mL/min/1.73m2 which are >=90 days apart,
without an intervening eGFR value >= 90 mL/min/1.73m2 (aged >=1 and < 18)
*/
proc sql;
insert into output.attrition
SELECT 'Step 7 - no intervening egfr above threshold' as step, '7' as step_num, COUNT(DISTINCT patid) as count
FROM local.ced_step7;
run;


/* Create encounter table restricted to cohort for computing nephrology visits */
proc sql;
create table work.cht_encounter as
select encounterid, cht.patid, admit_date, admit_time, discharge_date, discharge_time, providerid,
enc_type, facilityid, discharge_disposition, discharge_status, drg, drg_type, admitting_source,
payer_type_primary, payer_type_secondary, facility_type, raw_facility_type from
(SELECT *
FROM indata.encounter
WHERE admit_date between '01JAN2009'd and '31DEC2021'd
AND enc_type in ('AV', 'ED', 'EI', 'IP', 'OS'))  as enc
inner join (select patid from local.ced_step7) as cht
on cht.patid = enc.patid;
run;

/*
Create a temporary table of nephrology provider visits to create the required nephrology table
in the next step
*/
proc sql;
create table work.neph_visits_provider as
select distinct patid from 
(SELECT * from work.cht_encounter) as x
inner join (
SELECT providerid
FROM indata.provider
WHERE provider_specialty_primary IN ('163WN0300X', '207RN0300X', '2080P0210X', '246ZN0300X')) as y
on x.providerid=y.providerid;
run;

/*
Create a temporary table of nephrology facility visits to create the required nephrology table
in the next step
*/
proc sql;
create table work.neph_visits_facility as
SELECT encounterid, patid, admit_date, admit_time, discharge_date, discharge_time, providerid,
enc_type, facilityid, discharge_disposition, discharge_status, drg, drg_type, admitting_source,
payer_type_primary, payer_type_secondary, facility_type, raw_facility_type from work.cht_encounter
where (facility_type = 'HOSPITAL_OUTPATIENT_PEDIATRIC_NEPHROLOGY_CLINIC'
OR lower(raw_facility_type) like '%nephrolog%'
OR lower(raw_facility_type) like '%163WN0300X%'
OR lower(raw_facility_type) like '%207RN0300X%'
OR lower(raw_facility_type) like '%2080P0210X%'
OR lower(raw_facility_type) like '%246ZN0300X%');
run;

/*
Create table of nephrology provider visits Jan 2009 to Dec 2021
for patients in attrition cohort step 7.
*/
proc sql;
create table local.nephrology_visits as
select distinct c.patid from
(select patid from work.neph_visits_provider
union
select patid from work.neph_visits_facility) as nv
inner join (select patid from local.ced_step7) as c
on nv.patid = c.patid;

/*
Step 8: Log count of patients in cohort after excluding patients with no in-person visits with a nephrology
provider at any time between January 2009 and December 2021
*/
proc sql;
insert into output.attrition
SELECT 'Step 8 - >= 1 neph visit' as step, '8' as step_num, COUNT(DISTINCT patid) as count
FROM local.ced_step7
where patid in (select patid from local.nephrology_visits);
run;

/* Create temporary table of patid and cohort entry dates for cohort*/
proc sql;
create table work.cht_ced as
select patid, ce_date from local.ced_step7;
run;

/* 
Create a temporary table of procedures before cohort entry date to be used to create
chronic dialysis and kidney transplant procedures tables in future steps
*/
proc sql;
create table work.px_ced_qualified as
select proceduresid, cohort.patid, px_date, px, px_type from
(select * from work.cht_ced) as cohort
inner join 
(select *
FROM indata.procedures
WHERE px_date <= '31DEC2021'd) as px
on cohort.patid=px.patid and px.px_date<=cohort.ce_date;
run;

/* 
Create a temporary table of diagnoses before cohort entry date to be used to create
chronic dialysis and kidney transplant diagnosis tables in future steps
*/
/* Create temporary table of diagnoses before cohort entry dates*/
proc sql;
create table work.dx_ced_qualified as
select diagnosisid, cohort.patid, admit_date, dx_date, dx, dx_type from
(select * from work.cht_ced) as cohort
inner join 
(select *
FROM indata.diagnosis
WHERE coalesce(admit_date, dx_date) <= '31DEC2021'd) as dx
on cohort.patid=dx.patid
and coalesce(admit_date, dx_date) <= cohort.ce_date;

/* 
Create a temporary table of conditions to be used to create
chronic dialysis and kidney transplant condition tables in future steps
*/
/* Create temporary table of conditions before cohort entry dates*/
proc sql;
create table work.condition_ced_qualified as
select conditionid, cohort.patid, onset_date, report_date, resolve_date,
condition, condition_type from
(select * from work.cht_ced) as cohort
inner join 
(select *
FROM indata.condition
WHERE coalesce(onset_date, report_date, resolve_date) <= '31DEC2021'd) as condition
on cohort.patid=condition.patid
and coalesce(onset_date, report_date, resolve_date) <= cohort.ce_date;

/*
Create table of dialysis procedures Jan 2009 to Dec 2021 for patients in attrition cohort step 7. 
Restrict procedures <= ced and to patients with 
at least 2 dialysis procedures separated by >= 90 days.
*/
proc sql;
create table local.chronic_dialysis_px as
select *
FROM work.px_ced_qualified
inner join (select concept_code, pcornet_vocabulary_id 
from infolder.kidney_dialysis_px) as codeset
on px = concept_code and px_type = pcornet_vocabulary_id
group by patid
having count(proceduresid) >= 2 and max(px_date) - min(px_date) >=90;
run;

/*
Create table of kidney dialysis-associated diagnoses from diagnosis table between
Jan 2009 to Dec 2021 for patients in attrition cohort step 7. Restrict diagnoses <= ced.
*/
proc sql;
create table local.chronic_dialysis_diagnosis as
SELECT *
FROM dx_ced_qualified
inner join (select concept_code, pcornet_vocabulary_id
from infolder.kidney_dialysis_dx) as codeset
on dx = concept_code and dx_type = pcornet_vocabulary_id;
run;

/*
Create table of kidney dialysis-associated conditions from condition table between
Jan 2009 to Dec 2021 for patients in attrition cohort step 7. Restrict conditions <= ced.
*/
proc sql;
create table local.chronic_dialysis_condition as
SELECT *
FROM condition_ced_qualified
inner join (select concept_code, pcornet_vocabulary_id
from infolder.kidney_dialysis_dx) as codeset
on condition = concept_code and condition_type = pcornet_vocabulary_id;
run;

/*
Create table of patients with chronic dialysis-associated diagnoses or
chronic dialysis procedures prior to cohort entry date
*/
proc sql;
create table local.chronic_dialysis as
select distinct patid from
(select patid from local.chronic_dialysis_px
union (select patid from local.chronic_dialysis_diagnosis)
union (select patid from local.chronic_dialysis_condition));
run;


/*
Step 9: Log count of patients in cohort after excluding patients with >=1 chronic dialysis
procedure or associated diagnosis on or before cohort entry date
*/
proc sql;
insert into output.attrition
SELECT 'Step 9 - exclude chronic dialysis before ced' as step, '9' as step_num, COUNT(DISTINCT patid) as count
from
(select patid, ce_date
FROM local.ced_step7
where patid in (select patid from local.nephrology_visits)
and patid not in (select patid from local.chronic_dialysis));
run;

/*
Create table of kidney transplant procedures Jan 2009 to Dec 2021 
for patients in attrition cohort step 7. Restrict procedures <= ced.
*/
proc sql;
create table local.kidney_transplant_px as
SELECT *
FROM px_ced_qualified
inner join (select concept_code, pcornet_vocabulary_id
from infolder.kidney_transplant_px) as codeset
on px = concept_code and px_type = pcornet_vocabulary_id;
run;

/*
Create table of kidney transplant-associated diagnoses from diagnosis table between
Jan 2009 to Dec 2021 for patients in attrition cohort step 7. Restrict diagnoses <= ced.
*/
proc sql;
create table local.kidney_transplant_diagnosis as
SELECT *
FROM dx_ced_qualified
inner join (select concept_code, pcornet_vocabulary_id
from infolder.kidney_transplant_dx) as codeset
on dx = concept_code and dx_type = pcornet_vocabulary_id;
run;

/*
Create table of kidney transplant-associated conditions from condition table between
Jan 2009 to Dec 2021 for patients in attrition cohort step 7. Restrict conditions <= ced.
*/
proc sql;
create table local.kidney_transplant_condition as
SELECT *
FROM condition_ced_qualified
inner join (select concept_code, pcornet_vocabulary_id
from infolder.kidney_transplant_dx) as codeset
on condition = concept_code and condition_type = pcornet_vocabulary_id;
run;

/*
Create table of patients with kidney transplant-associated diagnoses or
kidney transplant procedures on or prior to cohort entry date
*/
proc sql;
create table local.kidney_transplant as
select distinct patid from
(select patid from local.kidney_transplant_px
union (select patid from local.kidney_transplant_diagnosis)
union (select patid from local.kidney_transplant_condition));
run;

/*
Create local table of final attrition cohort
*/
proc sql;
create table local.attrition_cohort as
select * from
(select patid, ce_date from local.ced_step7
where patid in (select patid from local.nephrology_visits)
and patid not in (select patid from local.chronic_dialysis)
and patid not in (select patid from local.kidney_transplant));
quit;

/*
Step 10: Log count of patients in cohort after excluding patients with >=1 kidney transplant
procedure/ associated diagnosis on or before cohort entry date
*/
proc sql;
insert into output.attrition
SELECT 'Step 10 - exclude transplant before ced' as step, '10' as step_num, COUNT(DISTINCT patid) as count
from local.attrition_cohort;
quit;
