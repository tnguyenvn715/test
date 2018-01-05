/* Name: get_slate_nl_guid_err.sql
-- Author: Tam Nguyen
-- Created: December 22, 2017
-- Description: Script calls import_slate_data_2_pkg.p_get_nl_goradid_err
-- that search for "-NL" tags in srrprel lis file and retrieve the text 
-- to write into an attachment file and email out to Admission.
*/
exec atowner.import_slate_data_2_pkg.p_get_nl_goradid_err('&1','&2');
exit; 
