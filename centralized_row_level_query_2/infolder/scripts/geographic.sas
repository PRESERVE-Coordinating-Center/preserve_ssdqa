/* 
Restricted to patients in the union of the three chronic kidney disease cohorts ("broad CKD cohort"),
study period, and specified fields.
1.	lds_address_history_zip5
2.	lds_address_history_zip9
3.	obs_gen_geo (obs_gen additionally restricted to census tract or census block group)
*/

libname output "&querypkg.output_geo";

/* create table for logging counts */
proc sql;
create table output.geo_counts
(cdm_table varchar(50), n_patids int, n_rows int);
run;

/* lds_address_history with zip5 */
proc sql;
create table output.lds_address_history_zip5 as
select addressid, cht.patid, address_use, address_type, address_preferred, address_city,
address_state, address_zip5, address_period_start, address_period_end
from indata.lds_address_history as cdm
inner join local.broad_ckd_cht as cht
on cht.patid = cdm.patid;
run;
/* log counts */
proc sql;
insert into output.geo_counts
SELECT 'lds_address_history_zip5' as cdm_table, COUNT(DISTINCT patid) as n_patids, COUNT(*) as n_rows
FROM output.lds_address_history_zip5;
run;

/* lds_address_history with zip5 AND zip9 */
proc sql;
create table output.lds_address_history_zip9 as
select addressid, cht.patid, address_use, address_type, address_preferred, address_city,
address_state, address_zip5, address_zip9, address_period_start, address_period_end
from indata.lds_address_history as cdm
inner join local.broad_ckd_cht as cht
on cht.patid = cdm.patid;
run;
/* log counts */
proc sql;
insert into output.geo_counts
SELECT 'lds_address_history_zip9' as cdm_table, COUNT(DISTINCT patid) as n_patids, COUNT(*) as n_rows
FROM output.lds_address_history_zip9;
run;

/* obs_gen (restricted to census tract and census block group) */
proc sql;
create table output.obs_gen_geo as
select obsgenid, cht.patid, encounterid, obsgen_providerid, obsgen_start_date,
obsgen_start_time, obsgen_stop_date, obsgen_stop_time, obsgen_type, obsgen_code,
obsgen_result_qual, obsgen_result_text, obsgen_result_num, obsgen_result_unit,
obsgen_table_modified, obsgen_id_modified, obsgen_source, obsgen_abn_ind,
raw_obsgen_code, raw_obsgen_type, raw_obsgen_result, raw_obsgen_unit
from indata.obs_gen as cdm
inner join local.broad_ckd_cht as cht
on cht.patid = cdm.patid
where (obsgen_code = '42026-5' or
obsgen_code = '49084-7' or
obsgen_code = 'LP6978-3' or
lower(obsgen_result_text) like '%census tract%' or
lower(obsgen_result_text) like '%census block group%' or
obsgen_table_modified = 'LDS');
run;
/* log counts */
proc sql;
insert into output.geo_counts
SELECT 'obs_gen_geo' as cdm_table, COUNT(DISTINCT patid) as n_patids, COUNT(*) as n_rows
FROM output.obs_gen_geo;
run;
