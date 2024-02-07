/*----------------------------------------------------------------------------
 *    User Inputs
 *----------------------------------------------------------------------------
 */


/* Data in PCORnet CDM Format
 * Please edit to point to the location of your data; trailing slash is required
 */
libname indata  'C:/pcornet/data/sas_view/';


/* Query Package
 * Please edit to point to the location of the query package,
 * which must have these subdirectories:
 *    sasprogram - the directory containing this file
 *    infolder   - support files for this query
 *    output     - directory containing files to return to PEDSnet DCC
 * The trailing slash is required.
 */    
%LET querypkg=H:/preserve/row_level_query_1/;

/*----------------------------------------------------------------------------
 *     End of User Inputs
 *----------------------------------------------------------------------------
*/

%LET REQUESTID=preserve_rl_v01;
libname infolder "&querypkg.infolder";
libname local "&querypkg.local";
libname output "&querypkg.output_agg";
options fullstimer;

 
/* Redirect log to file for debugging */
proc printto log="&querypkg.output_agg/&REQUESTID..log";
run;

/* Execute queries in order */
%include "&querypkg.infolder/scripts/attrition_cht.sas";
%include "&querypkg.infolder/scripts/broad_ckd_cht.sas";
%include "&querypkg.infolder/scripts/extract_row_level.sas";
%include "&querypkg.infolder/scripts/geographic.sas";
%include "&querypkg.infolder/scripts/local_tbl_gen.sas";
%include "&querypkg.infolder/scripts/output_dists.sas";
%include "&querypkg.infolder/scripts/identify_unmap.sas";

/* Turn off redirect log */
proc printto;
run;