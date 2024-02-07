/************************/
/*   SERUM CREATININE   */
/************************/

/*
Simple search for potentially unmapped creatinine measurements in raw_lab_name
This table will help identify both unmapped urine creatinine and serum creatinine measurements
Remove rows where n_patids < 3 due to inclusion of raw field in output table
*/
proc sql;
create table output.unmap_scr1 as
select * from
(select lab_loinc, raw_lab_name, specimen_source, count(distinct patid) as n_patids,
count (distinct lab_result_cm_id) as n_rows from work.temp_lab_result_cm
where lower(raw_lab_name) like '%creat%'
group by lab_loinc, specimen_source, raw_lab_name)
where n_patids >= 3;
run;

/*
Search for potentially unmapped serum creatinine measurements in raw_lab_code
*/
proc sql;
create table output.unmap_scr2 as
select lab_loinc, raw_lab_code, count(distinct patid) as n_patids, count (distinct lab_result_cm_id) as n_rows from work.temp_lab_result_cm
where raw_lab_code in (select concept_code from infolder.serum_creatinine)
and lab_loinc not in (select concept_code from infolder.serum_creatinine)
group by lab_loinc;
run;

/*
Simple search for potentially unmapped urine protein measurements in raw_lab_name
Remove rows where n_pats < 3 due to inclusion of raw field in output table
*/
proc sql;
create table output.unmap_prot as
select * from
(select lab_loinc, raw_lab_name, specimen_source, count(distinct patid) as n_patids, count (distinct lab_result_cm_id) as n_rows from work.temp_lab_result_cm
where lower(raw_lab_name) like '%protein%'
group by lab_loinc, specimen_source, raw_lab_name)
where n_patids >= 3;
run;

/************************/
/* NEPHROLOGY PROVIDERS */
/************************/

/*
Simple search for potentially unmapped nephrology specialty in raw_provider_specialty_primary
Remove rows where n_providers < 3 due to inclusion of raw field in output table
*/
proc sql;
create table output.unmap_neph as
select distinct * from
(select provider_specialty_primary, raw_provider_specialty_primary, count(distinct providerid) as n_providers from indata.provider
where (lower(raw_provider_specialty_primary) like '%nephr%'
or lower(raw_provider_specialty_primary) like '%renal%'
or lower(raw_provider_specialty_primary) like '%kidney%'
or lower(raw_provider_specialty_primary) like '%dialy%'))
where n_providers >= 3;
run;
