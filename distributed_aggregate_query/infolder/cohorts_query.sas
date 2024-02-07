/* Create CKD dx cohort: Patients with at least 1 diagnosis from a CKD stages 2 & 3 codeset
cohort entry date (ce_date) is the first diagnosis
count of distinct dates with a diagnosis is also provided (dx_count)*/
proc sql;
create table local.ckd_stage23_cohort as
select patid, min(dx_date) as ce_date format=date9., count(distinct dx_date) as dx_count from
(select * from indata.diagnosis
where dx_date between '01JAN2009'd and '31DEC2021'd) as cdm /*diagnosis table from pcornet cdm*/
inner join infolder.ckd_stage23_dx as codeset /*CKD stages 2 & 3 codeset*/
on cdm.dx=codeset.concept_code and cdm.dx_type=codeset.pcornet_vocabulary_id /*inner join on concept_code and dx_type*/
group by patid;
run;


/* Create temporary table of serum creatinine measurements where result is above 1.5*upper limit of normal
from table created in feas_query.sas table of serum creatinine measurements where patients aged >=1 and
< 18 years
*/
proc sql;
create table local.high_scr_meas as
select * from
(select patid, lab_loinc, comb_lab_date, result_num, norm_range_high, norm_modifier_high, input(norm_range_high, 8.8) as uln format=z8.8
from local.ser_creat
where result_num is not null
and norm_range_high is not null)
/* If modifier is less than, result is high if equal or greater than */
where ((norm_modifier_high in ('LT') and result_num >= 1.5*uln)
/* For all other modifiers, result is high if greater than*/
or (norm_modifier_high not in ('LT') and result_num > 1.5*uln));
run;

/* Create high serum creatinine cohort: Patients with at least 2 serum creatinine measurement
above the 1.5*upper limit of normal, separated by 90 days */
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
coalesce(ckd_dx.ce_date, high_scr.ce_date) as ce_date format=date9.,
coalesce(ckd_stage23, 0) as ckd_stage23,
coalesce(high_scr, 0) as high_scr from
((select patid, ce_date, 1 as ckd_stage23 from local.ckd_stage23_cohort) as ckd_dx
full join (select patid, ce_date, 1 as high_scr from local.high_scr_cohort) as high_scr
on ckd_dx.patid = high_scr.patid)
order by patid;
run;

/* Compute overlap for high serum creatinine, CKD, and attrition cohort cohort */
proc sql;
create table work.comb_cohort_prelim2 as
select coalesce(attr_cht.patid, ckd_high_scr.patid) as patid,
coalesce(attr_cht.ce_date, ckd_high_scr.ce_date) as ce_date format=date9.,
coalesce(attr_cht, 0) as attr_cht,
ckd_stage23, high_scr from
((select * from work.comb_cohort_prelim1) as ckd_high_scr
full join (select patid, ce_date, 1 as attr_cht from local.attrition_cohort) as attr_cht
on ckd_high_scr.patid = attr_cht.patid)
order by patid;
run;

/* Limit age to >= 1 and < 18 years at cohort entry*/
proc sql;
create table local.comb_cohort as
select * from
(select cht.patid,
ce_date,
year(ce_date) as ce_year,
coalesce(attr_cht, 0) as attr_cht,
coalesce(ckd_stage23, 0) as ckd_stage23,
coalesce(high_scr, 0) as high_scr,
birth_date,
floor((ce_date - birth_date)/365.25) as ce_age_years from
work.comb_cohort_prelim2 as cht
inner join indata.demographic as dem
on cht.patid = dem.patid)
where ce_age_years < 18 and ce_age_years >= 1;
run;
proc sql;
create index patid
on local.comb_cohort (patid);
run;



