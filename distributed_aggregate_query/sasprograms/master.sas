/*----------------------------------------------------------------------------
 *    User Inputs
 *----------------------------------------------------------------------------
 */


/* Data in PCORnet CDM Format
 * Please edit to point to the location of your data; trailing slash is required
 */
    
libname indata  "C:/pcornet/data/sas_view/";

/* Query Package
 * Please edit to point to the location of the query package,
 * which must have these subdirectories:
 *    sasprogram - the directory containing this file
 *    infolder   - support files for this query
 *    output     - directory containing files to return to PEDSnet DCC
 * The trailing slash is required.
 */    
%LET querypkg=H:/preserve/feasibility_sas/;

/*----------------------------------------------------------------------------
 *     End of User Inputs
 *----------------------------------------------------------------------------
*/

%LET REQUESTID=preserve_feas_v02;
libname infolder "&querypkg.infolder";
libname local "&querypkg.local";
libname output "&querypkg.output";
options fullstimer;

 
/* Redirect log to file for debugging */
proc printto log="&querypkg.output/&REQUESTID..log";
run;

/* Execute queries in order */
%include "&querypkg.infolder/feas_query.sas";
%include "&querypkg.infolder/cohorts_query.sas";
%include "&querypkg.infolder/local_tbl_gen.sas";
%include "&querypkg.infolder/output_dists.sas";
%include "&querypkg.infolder/identify_unmap.sas";

/* Turn off redirect log */
proc printto;
run;
