
/************************/
/*  COMBINE CODESETS    */
/************************/

/* Create combined condition codeset */
proc sql;
create table local.all_dx as
select concept_code, pcornet_vocabulary_id, 'ckd_stage23_dx' as codeset from infolder.ckd_stage23_dx
union
select concept_code, pcornet_vocabulary_id, 'kidney_dialysis_dx' as codeset from infolder.kidney_dialysis_dx
union
select concept_code, pcornet_vocabulary_id, 'kidney_transplant_dx' as codeset from infolder.kidney_transplant_dx
run;
/* Create indexes for combined medication codeset */
proc sql;
create index concept_code
on local.all_dx (concept_code);
run;

/* Create combined medication codeset */
proc sql;
create table local.all_rx as
select concept_code, pcornet_vocabulary_id, 'ace_inhibitor_rx' as codeset from infolder.ace_inhibitor_rx
union 
select concept_code, pcornet_vocabulary_id, 'arb_rx' as codeset from infolder.arb_rx
union 
select concept_code, pcornet_vocabulary_id, 'bb_rx' as codeset from infolder.bb_rx
union 
select concept_code, pcornet_vocabulary_id, 'ccb_rx' as codeset from infolder.ccb_rx
union 
select concept_code, pcornet_vocabulary_id, 'loop_diuretic_rx' as codeset from infolder.loop_diuretic_rx
union 
select concept_code, pcornet_vocabulary_id, 'thiazide_rx' as codeset from infolder.thiazide_rx
run;
/* Create indexes for combined medication codeset */
proc sql;
create index concept_code
on local.all_rx (concept_code);
run;

/* Create combined procedure codeset */
proc sql;
create table local.all_px as
select concept_code, pcornet_vocabulary_id, 'kidney_transplant_px' as codeset from infolder.kidney_transplant_px
union 
select concept_code, pcornet_vocabulary_id, 'kidney_dialysis_px' as codeset from infolder.kidney_dialysis_px
run;
/* Create indexes for combined procedure codeset */
proc sql;
create index concept_code
on local.all_px (concept_code);
run;

/* Create combined lab codeset */
proc sql;
create table local.all_labs as
select concept_code, concept_name, pcornet_vocabulary_id, 'upcr' as codeset from infolder.upcr
union 
select concept_code, concept_name, pcornet_vocabulary_id, 'serum_creatinine' as codeset from infolder.serum_creatinine
union 
select concept_code, concept_name, pcornet_vocabulary_id, 'serum_cystatin' as codeset from infolder.serum_cystatin
union 
select concept_code, concept_name, pcornet_vocabulary_id, 'urine_creatinine' as codeset from infolder.urine_creatinine
union 
select concept_code, concept_name, pcornet_vocabulary_id, 'urine_protein_qual' as codeset from infolder.urine_protein_qual
union 
select concept_code, concept_name, pcornet_vocabulary_id, 'urine_protein_quant' as codeset from infolder.urine_protein_quant
run;
/* Create indexes for combined lab codeset */
proc sql;
create index concept_code
on local.all_labs (concept_code);
run;

/************************/
/*     DEMOGRAPHICS     */
/************************/

/* Create table of all demographic information for combined cohort*/
proc sql;
create table local.cht_demographic as
select cdm.patid, cdm.birth_date, sex, hispanic, race, ce_date, ce_year, attr_cht, ckd_stage23, high_scr, ce_age_years from indata.demographic as cdm
inner join local.comb_cohort as cht
on cdm.patid = cht.patid;
run;

/************************/
/*     VISITS           */
/************************/

/* Create table of all visit information for combined cohort*/
proc sql;
create table local.cht_encounter as
select cht.patid as patid, attr_cht, high_scr, ckd_stage23, encounterid, ce_date, admit_date, discharge_date, enc_type, facility_type, raw_facility_type, providerid,
payer_type_primary, payer_type_secondary, raw_payer_type_primary
from indata.encounter as cdm
inner join local.comb_cohort as cht
on cdm.patid = cht.patid;
run;

/************************/
/*     SPECIALTY        */
/************************/

/* Create table of all nephrology providers*/
proc sql;
create table local.cht_neph_provider as
select providerid, provider_specialty_primary, raw_provider_specialty_primary
from indata.provider
where provider_specialty_primary in (select valueset_item from infolder.nephrology_spec_prov);
run;

/************************/
/*     VITALS           */
/************************/

/* Create table of all vitals data for combined cohort*/
proc sql;
create table local.cht_vital as
select cht.patid as patid, attr_cht, high_scr, ckd_stage23, vitalid, encounterid, measure_date, ht, wt, diastolic, systolic, original_bmi
from indata.vital as cdm
inner join local.comb_cohort as cht
on cdm.patid = cht.patid;
run;

/************************/
/*     CONDITIONS       */
/************************/

/* Create table of all key diagnosis information for combined cohort*/
proc sql;
create table local.cht_diagnosis as
select cht.patid as patid, attr_cht, high_scr, ckd_stage23, codeset, diagnosisid, encounterid, dx, dx_type, dx_date, raw_dx, providerid
from indata.diagnosis as cdm
inner join local.comb_cohort as cht
on cdm.patid = cht.patid
inner join local.all_dx as cds
on cdm.dx = cds.concept_code and cdm.dx_type = cds.pcornet_vocabulary_id;
run;


/* Create table of all key condition information for combined cohort*/
proc sql;
create table local.cht_condition as
select cht.patid as patid, attr_cht, high_scr, ckd_stage23, codeset, conditionid, encounterid, report_date, resolve_date, onset_date, condition, condition_type, raw_condition
from indata.condition as cdm
inner join local.comb_cohort as cht
on cdm.patid = cht.patid
inner join local.all_dx as cds
on cdm.condition = cds.concept_code and cdm.condition_type = cds.pcornet_vocabulary_id;
run;


/************************/
/*     DRUGS            */
/************************/

/* Create table of all key prescribing information for combined cohort*/
proc sql;
create table local.cht_prescribing as
select cht.patid as patid, attr_cht, high_scr, ckd_stage23, codeset, prescribingid, encounterid, rx_order_date, rx_start_date, rx_end_date, rxnorm_cui, raw_rx_med_name,
rx_route, raw_rx_route, rx_providerid
from indata.prescribing as cdm
inner join local.comb_cohort as cht
on cdm.patid = cht.patid
inner join
(select concept_code, pcornet_vocabulary_id, codeset from local.all_rx
where pcornet_vocabulary_id = 'RX') as cds
on cdm.rxnorm_cui = cds.concept_code;
run;


/* Create table of all key dispensing information for combined cohort*/
proc sql;
create table local.cht_dispensing as
select cht.patid as patid, attr_cht, high_scr, ckd_stage23, codeset, dispensingid, dispense_date, ndc,
dispense_route, raw_dispense_route
from indata.dispensing as cdm
inner join local.comb_cohort as cht
on cdm.patid = cht.patid
inner join
(select concept_code, pcornet_vocabulary_id, codeset from local.all_rx
where pcornet_vocabulary_id = 'ND') as cds
on cdm.ndc = cds.concept_code;
run;


/* Create table of all key med_admin information for combined cohort*/
proc sql;
create table work.temp_cht_med_admin as
select cht.patid as patid, attr_cht, high_scr, ckd_stage23, medadminid, encounterid, medadmin_start_date, medadmin_stop_date, medadmin_code, medadmin_type,
medadmin_route, raw_medadmin_route, medadmin_providerid
from indata.med_admin as cdm
inner join local.comb_cohort as cht
on cdm.patid = cht.patid;
run;
proc sql;
create table local.cht_med_admin as
select *
from work.temp_cht_med_admin as cdm
inner join local.all_rx as cds
on cdm.medadmin_code = cds.concept_code and cdm.medadmin_type = cds.pcornet_vocabulary_id;
run;


/************************/
/*     PROCEDURES       */
/************************/

/* Create table of all key procedure information for combined cohort */
proc sql;
create table work.temp_cht_procedures as
select cht.patid as patid, attr_cht, high_scr, ckd_stage23, proceduresid, admit_date, px_date, px, px_type, raw_px, providerid
from indata.procedures as cdm
inner join local.comb_cohort as cht
on cdm.patid = cht.patid;
run;
proc sql;
create table local.cht_procedures as
select *
from work.temp_cht_procedures as cdm
inner join local.all_px as cds
on cdm.px = cds.concept_code and cdm.px_type = cds.pcornet_vocabulary_id;
run;

/************************/
/*     LABS             */
/************************/

/* Create table of all key labs
Note lab_result_cm temporary table created in feas_query.sas is queried*/
proc sql;
create table local.cht_lab_result_cm as
select cht.patid as patid, attr_cht, high_scr, ckd_stage23, codeset, lab_result_cm_id, lab_loinc,
comb_lab_date, result_qual, result_snomed,
result_num, result_modifier, result_unit, norm_range_low, norm_modifier_low,
norm_range_high, norm_modifier_high, raw_lab_name, raw_result
from work.temp_lab_result_cm  as cdm
inner join local.comb_cohort as cht
on cdm.patid = cht.patid
inner join local.all_labs as cds
on cdm.lab_loinc = cds.concept_code;
run;
