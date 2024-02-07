/*
Step 0: Log count of patients with >=1 visit between January 2009 and December 2021
*/
proc sql;
create table output.attrition as
SELECT 'Step 0 - >=1 visit' as step, '0' as step_num, COUNT(DISTINCT patid)
FROM indata.encounter
WHERE admit_date between '01JAN2009'd and '31DEC2021'd;
run;


/*
Step 1: Log count of patients with >=1 in-person visit between January 2009 and December 2021
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
select labs.patid, comb_lab_date,
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
inner join (SELECT patid, birth_date
FROM indata.demographic) as demo on labs.patid=demo.patid;


/*
Log count of patients with >=1 serum creatinine measurement
*/
proc sql;
insert into output.attrition
SELECT 'Step 2 - >=1 serum creatinine meas' as step, '2' as step_num, COUNT(DISTINCT patid) as count 
from (select patid, lab_loinc FROM work.temp_lab_result_cm) as cdm
inner join (select concept_code from infolder.serum_creatinine) as codeset
on cdm.lab_loinc = codeset.concept_code;
run;

/*
Create local serum creatinine table of measurements where patients aged >=1 and < 18 years
*/
proc sql;
create table local.ser_creat as
select patid, comb_lab_date,
lab_loinc, result_num, 
result_unit, norm_range_high, norm_modifier_high,
raw_lab_name, raw_lab_code, raw_result,
lab_result_cm_id
from 
(select * from work.temp_lab_result_cm
where comb_lab_date-birth_date between 365 and 365.25*18) as cdm
inner join (select concept_code from infolder.serum_creatinine) as codeset
on cdm.lab_loinc=codeset.concept_code;
run;


/*
Step3: Log count of patients aged >=1 and < 18 at time of >=1 serum creatinine measurement
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
where abs(comb_lab_date - measure_date) <= 90
group by h.patid, comb_lab_date) as y
inner join(SELECT patid, measure_date, ht
			FROM indata.vital
			WHERE ht IS NOT null) as z
on y.patid=z.patid
where abs(comb_lab_date - measure_date) = days;
run;

/*
Step 4: Log count of patients with height measurement available <=90 days of
serum creatinine value (aged >=1 and < 18)
*/
proc sql;
insert into output.attrition
SELECT 'Step 4 - accompanying height measurement' as step, '4' as step_num, COUNT(DISTINCT patid) as count
FROM local.heights;
run;

/*
Create local eGFR table, restricted to values 30-89 mL/min/1.73m2
Revised Bedside Schwartz formula used to compute eGFR with unit conversaion from
inches to cm
*/
proc sql;
create table local.egfr as
select heights.patid as patid, measure_date, 
ser_creat.comb_lab_date, 0.413*ht*2.54/result_num as egfr
from local.heights inner join local.ser_creat 
on (heights.patid=ser_creat.patid and heights.comb_lab_date=ser_creat.comb_lab_date)
where 0.413*ht*2.54/result_num between 30 and 89;
run;

/*
Step 5: Log count of patients with >=1 eGFR value >=30 and <=89 mL/min/1.73m2 (aged >=1 and < 18)
*/
proc sql;
insert into output.attrition
SELECT 'Step 5 - egfr within range (30-89)' as step, '5' as step_num, COUNT(DISTINCT patid) as count
from local.egfr;
run;


/*
Step 6: Patients with >=2 eGFRs >=30 and <=89 mL/min/1.73m2 which are >=90 days apart (aged >=1 and < 18)
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
2 eGFRs 30-89 mL/min/1.73m2 separated by 90 days from egfr table,
without an intervening egfr >= 90mL/min/1.73m2
The cohort entry date (CED) is defined as
the date of the first eGFR of the earliest pair of eGFRs >=30 & <=89 mL/min/1.73m2, 
separated by >=90 days, without an intervening eGFR >=90
*/
proc sql;
create table local.ced as
select u_ninety.patid, min(u_ninety.first_date) as ce_date format=date9. from
(select l.patid as patid, l.comb_lab_date as first_date, r.comb_lab_date as second_date
from local.egfr as l inner join local.egfr as r 
on l.patid=r.patid and r.comb_lab_date - l.comb_lab_date >= 90) as u_ninety
left join (
select heights.patid as patid, 
ser_creat.comb_lab_date
from local.heights inner join local.ser_creat 
on (heights.patid=ser_creat.patid and heights.comb_lab_date=ser_creat.comb_lab_date)
where 0.413*ht*2.54/result_num > 89) as o_ninety 
on u_ninety.patid=o_ninety.patid 
and o_ninety.comb_lab_date between u_ninety.first_date and u_ninety.second_date
where missing(o_ninety.comb_lab_date)
group by u_ninety.patid;

/*
Step 7: Log count of patients with >=2 eGFRs >=30 and <=89 mL/min/1.73m2 which are >=90 days apart,
without an intervening eGFR value >= 90 mL/min/1.73m2 (aged >=1 and < 18)
*/
proc sql;
insert into output.attrition
SELECT 'Step 7 - no intervening egfr above threshold' as step, '7' as step_num, COUNT(DISTINCT patid) as count
FROM local.ced;
run;

/*
Create a temporary table of nephrology visits to create the required nephrology table
in the next step
*/
proc sql;
create table work.neph_visits as
select * from 
(SELECT *
FROM indata.encounter
WHERE admit_date between '01JAN2009'd and '31DEC2021'd
AND enc_type in ('AV', 'ED', 'EI', 'IP', 'OS')) as x
inner join (
SELECT providerid
FROM indata.provider
WHERE provider_specialty_primary IN ('163WN0300X', '207RN0300X', '2080P0210X', '246ZN0300X')) as y
on x.providerid=y.providerid;
run;

/*
Create table of nephrology provider visits Jan 2009 to Dec 2021
for patients in ced table.
*/
proc sql;
create table local.nephrology_visits as
select * from
(select * from work.neph_visits) as nv
inner join (select patid from local.ced) as c
on nv.patid = c.patid;

/*
Step 8: Log count of patients in cohort after excluding patients with no in-person visits with a nephrology
provider at any time between January 2009 and December 2021
*/
proc sql;
insert into output.attrition
SELECT 'Step 8 - >= 1 neph visit' as step, '8' as step_num, COUNT(DISTINCT patid) as count
FROM local.ced
where patid in (select patid from local.nephrology_visits);
run;

/* 
Create a temporary table of procedures to be used to create
chronic dialysis and kidney transplant tables in future steps
*/
/* Create temporary table of patid and cohort entry dates for cohort*/
proc sql;
create table work.ced_for_px as
select * from local.ced;
/* Create temporary table of procedure before cohort entry dates*/
proc sql;
create table work.px_ced_qualified as
select * from
(select * from work.ced_for_px) as cohort
inner join 
(select *
FROM indata.procedures
WHERE px_date between '01JAN2009'd and '31DEC2021'd) as px
on cohort.patid=px.patid and px.px_date<=cohort.ce_date;

/*
Create table of dialysis procedures Jan 2009 to Dec 2021 for patients in ced table. 
Restrict procedures <= ced and to patients with 
at least 2 dialysis procedures separated by >= 90 days.
*/
proc sql;
create table local.chronic_dialysis as
select *
FROM work.px_ced_qualified
inner join (select concept_code, pcornet_vocabulary_id 
from infolder.kidney_dialysis_px) as codeset
on px = concept_code and px_type = pcornet_vocabulary_id
where px_date <= ce_date
group by patid
having count(proceduresid) >= 2 and max(px_date) - min(px_date) >=90;
run;

/*
Step 9: Log count of patients in cohort after excluding patients with >=1 chronic dialysis
procedure on or before cohort entry date
*/
proc sql;
insert into output.attrition
SELECT 'Step 9 - exclude chronic dialysis before ced' as step, '9' as step_num, COUNT(DISTINCT patid) as count
from
(select ced.patid, ced.ce_date
FROM local.ced
where patid in (select patid from local.nephrology_visits)
and patid not in (select patid from local.chronic_dialysis));
run;

/*
Create table of kidney transplant procedures Jan 2009 to Dec 2021 
for patients in ced table. Restrict procedures <= ced.
*/
proc sql;
create table local.kidney_transplant as
SELECT *
FROM px_ced_qualified
inner join (select concept_code, pcornet_vocabulary_id
from infolder.kidney_transplant_px) as codeset
on px = concept_code and px_type = pcornet_vocabulary_id
WHERE px_date <= ce_date;
run;

/*
Create local table of final cohort (applying criteria of
>=1 kidney transplant procedure on or before cohort entry date)
*/
proc sql;
create table local.attrition_cohort as
select * from
(select patid, ce_date from local.ced
where patid in (select patid from local.nephrology_visits)
and patid not in (select patid from local.chronic_dialysis)
and patid not in (select patid from local.kidney_transplant));
quit;

/*
Step 10: Log count of patients in cohort after excluding patients with >=1 kidney transplant procedure on or before cohort entry date
*/
proc sql;
insert into output.attrition
SELECT 'Step 10 - exclude transplant before ced' as step, '10' as step_num, COUNT(DISTINCT patid) as count
from local.attrition_cohort;
quit;
