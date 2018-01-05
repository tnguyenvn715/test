CREATE OR REPLACE PACKAGE import_slate_data_2_pkg
IS
  FUNCTION f_check_sbgi(f_sbgi_code VARCHAR2) RETURN stvsbgi.stvsbgi_code%TYPE;
  FUNCTION f_string_midpoint(f_string IN VARCHAR2) RETURN NUMBER;
  FUNCTION f_translate_ethn_code(f_ethn_code IN VARCHAR2, f_hispanic_ind IN VARCHAR2) RETURN stvethn.stvethn_code%TYPE;
  FUNCTION f_translate_race_code(f_race_code IN VARCHAR2, f_hispanic_ind IN VARCHAR2) RETURN gorrace.gorrace_race_cde%TYPE;
  FUNCTION f_translate_rtyp_code(f_prospect_type IN VARCHAR2, f_term_code IN VARCHAR2, f_country_code IN VARCHAR2) RETURN srbrecr.srbrecr_rtyp_code%TYPE;
  FUNCTION f_translate_styp_code(f_prospect_type IN VARCHAR2) RETURN srbrecr.srbrecr_styp_code%TYPE;
  FUNCTION f_check_goradid_guid(f_additional_id IN goradid.goradid_additional_id%TYPE, f_adid_code IN goradid.goradid_adid_code%TYPE) RETURN NUMBER;
  FUNCTION f_check_goradid_pidm(f_goradid_pidm IN goradid.goradid_pidm%TYPE, f_adid_code IN goradid.goradid_adid_code%TYPE) RETURN NUMBER;
  FUNCTION f_get_primid_from_guid(f_additional_id IN goradid.goradid_additional_id%TYPE, f_adid_code IN goradid.goradid_adid_code%TYPE) RETURN spriden.spriden_id%TYPE;
  FUNCTION f_get_pidm_from_guid(f_additional_id IN goradid.goradid_additional_id%TYPE, f_adid_code IN goradid.goradid_adid_code%TYPE) RETURN goradid.goradid_pidm%TYPE;
  FUNCTION f_get_valid_natn(f_nation IN VARCHAR2) RETURN stvnatn.stvnatn_code%TYPE;
  FUNCTION f_check_date_format(f_date_str IN VARCHAR2, f_date_format IN VARCHAR2) RETURN VARCHAR2;
  FUNCTION f_check_valid_majr(f_majr_code IN VARCHAR2) RETURN VARCHAR2;
  PROCEDURE p_import_pers_to_temp; 
  PROCEDURE p_import_applicants;
  PROCEDURE p_insert_appl_cur_fos(
    p_guid          goradid.goradid_additional_id%TYPE DEFAULT NULL,
    p_pidm          sorlcur.sorlcur_pidm%TYPE,
    p_term_code     sorlcur.sorlcur_term_code%TYPE,
    p_levl_code     sorlcur.sorlcur_levl_code%TYPE,
    p_degree_code   sorlcur.sorlcur_degc_code%TYPE,
    p_appl_no       sorlcur.sorlcur_key_seqno%TYPE,
    p_majr_code     sorlfos.sorlfos_majr_code%TYPE,
    p_admt_code     sorlcur.sorlcur_admt_code%TYPE,
    p_data_origin   sorlcur.sorlcur_data_origin%TYPE);
  PROCEDURE  p_load_percent_4yr(
    p_pidm          sprcmnt.sprcmnt_pidm%TYPE,
    p_percent_4yr   sprcmnt.sprcmnt_text%TYPE,
    p_orig_code     sprcmnt.sprcmnt_orig_code%TYPE,
    p_data_origin   sprcmnt.sprcmnt_data_origin%TYPE,
    p_error_out   OUT  VARCHAR2);
  PROCEDURE p_import_appl_schools;
  PROCEDURE p_insert_srtaddr ( 
    p_guid      VARCHAR2,
    p_ridm      srtaddr.srtaddr_ridm%TYPE,
    p_street1   VARCHAR2,
    p_street2   VARCHAR2,
    p_street3   VARCHAR2,
    p_city      VARCHAR2,
    p_region    VARCHAR2,
    p_postal    VARCHAR2,
    p_natn_code stvnatn.stvnatn_code%TYPE,
    p_atyp_code stvatyp.stvatyp_code%TYPE,
    p_from_date  DATE,
    p_to_date    DATE,
    p_data_origin VARCHAR2,
    p_err_out   OUT VARCHAR2);
    
  PROCEDURE p_insert_srttele(
    p_guid          VARCHAR2,
    p_ridm          srttele.srttele_ridm%TYPE,
    p_phone_number  VARCHAR2,
    p_tele_code     stvtele.stvtele_code%TYPE,
    p_data_origin   VARCHAR2,
    p_err_out   OUT VARCHAR2);
  
  PROCEDURE p_insert_srtemal(
    p_guid          VARCHAR2,
    p_ridm          srtemal.srtemal_ridm%TYPE,
    p_email_address srtemal.srtemal_email_address%TYPE,
    p_emal_code     srtemal.srtemal_emal_code%TYPE,
    p_data_origin   VARCHAR2,
    p_err_out   OUT VARCHAR2);

  PROCEDURE p_insert_srtprel(
    p_guid           VARCHAR2,
    p_ridm           srtprel.srtprel_ridm%TYPE,
    p_prel_code      srtprel.srtprel_prel_code%TYPE,
    p_term_code      srtprel.srtprel_term_code%TYPE,
    p_rtyp_code      srtprel.srtprel_rtyp_code%TYPE,
    p_styp_code      srtprel.srtprel_styp_code%TYPE,
    p_data_origin    srtprel.srtprel_data_origin%TYPE,
    p_err_out   OUT VARCHAR2);
  
  PROCEDURE p_insert_srthsch(
    p_guid              VARCHAR2,
    p_ridm              srthsch.srthsch_ridm%TYPE,
    p_school_code       VARCHAR2,
    p_graduation_date   DATE,
    p_term_code         VARCHAR2,
    p_data_origin       VARCHAR2,
    p_err_out   OUT     VARCHAR2); 
  
  PROCEDURE p_insert_srtpcol(
    p_guid              VARCHAR2,
    p_ridm              srtpcol.srtpcol_ridm%TYPE,
    p_school_code       VARCHAR2,
    p_graduation_date   DATE,
    p_term_code         VARCHAR2,
    p_data_origin       VARCHAR2,
    p_err_out   OUT     VARCHAR2) ; 
  PROCEDURE p_handle_err(
    p_guid          sdlperr.sdlperr_guid%TYPE,
    p_err_mesg      sdlperr.sdlperr_error%TYPE,
    p_data_origin   sdlperr.sdlperr_data_origin%TYPE);   

  PROCEDURE p_srrsrin_in_gjbprun(
    p_prospect_code  IN    gjbprun.gjbprun_value%TYPE,
    p_outfile_name   IN    VARCHAR2 );
      
  PROCEDURE p_srrprel_in_gjbprun(
    p_prospect_code  IN    gjbprun.gjbprun_value%TYPE,
    p_match_status   IN     gjbprun.gjbprun_value%TYPE,
    p_outfile_name   IN    VARCHAR2);
  PROCEDURE p_srtpurg_in_gjbprun(
    p_prospect_code  IN    gjbprun.gjbprun_value%TYPE,
    p_match_status   IN    gjbprun.gjbprun_value%TYPE,
    p_load_status    IN    gjbprun.gjbprun_value%TYPE,
    p_audit_or_update IN   gjbprun.gjbprun_value%TYPE,
    p_outfile_name   IN    VARCHAR2);
  PROCEDURE p_load_sorcont(
    p_pidm            sorcont.sorcont_pidm%TYPE,
    p_ctyp_code       sorcont.sorcont_ctyp_code%TYPE,
    p_ctyp_date       sorcont.sorcont_contact_date%TYPE,
    p_data_origin     VARCHAR2,
    p_error_out   OUT VARCHAR2);
  PROCEDURE p_load_sorcont(
    p_pidm           sorcont.sorcont_pidm%TYPE,
    p_ctyp_code      sorcont.sorcont_ctyp_code%TYPE,
    p_data_origin    VARCHAR2,
    p_error_out  OUT VARCHAR2);
  PROCEDURE p_insert_srrrsrc(
    p_pidm           srrrsrc.srrrsrc_pidm%TYPE,
    p_sbgi_code      srrrsrc.srrrsrc_sbgi_code%TYPE,
    p_term           srbrecr.srbrecr_term_code%TYPE,
    p_seqno          srbrecr.srbrecr_admin_seqno%TYPE,
    p_data_origin     VARCHAR2) ;
  
  PROCEDURE p_load_sorints(
    p_pidm           sorints.sorints_pidm%TYPE,
    p_ints_code      sorints.sorints_ints_code%TYPE,
    p_data_origin    VARCHAR2,
    p_error_out  OUT VARCHAR2);
  PROCEDURE p_load_saraatt(
    p_pidm           saraatt.saraatt_pidm%TYPE,
    p_term_code      saraatt.saraatt_term_code%TYPE,
    p_appl_no        saraatt.saraatt_appl_no%TYPE,
    p_atts_code      saraatt.saraatt_atts_code%TYPE,
    p_data_origin    VARCHAR2,
    p_error_out  OUT VARCHAR2);
  PROCEDURE p_load_adm_rating(
    p_pidm           saradap.saradap_pidm%TYPE,
    p_term_code      saradap.saradap_term_code_entry%TYPE,
    p_appl_no        saradap.saradap_appl_no%TYPE,
    p_ratp_code      stvratp.stvratp_code%TYPE,
    p_rating         VARCHAR2,
    p_username       VARCHAR2,
    p_data_origin    VARCHAR2,
    p_err_out    OUT VARCHAR2);
  PROCEDURE p_load_adm_decision(
    p_pidm           saradap.saradap_pidm%TYPE,
    p_term_code      saradap.saradap_term_code_entry%TYPE,
    p_appl_no        saradap.saradap_appl_no%TYPE,
    p_apdc_code      stvapdc.stvapdc_code%TYPE,
    p_data_origin    VARCHAR2,
    p_err_out    OUT VARCHAR2);
  PROCEDURE p_load_adm_response(
    p_pidm           saradap.saradap_pidm%TYPE,
    p_term_code      saradap.saradap_term_code_entry%TYPE,
    p_appl_no        saradap.saradap_appl_no%TYPE,
    p_apdc_code      stvapdc.stvapdc_code%TYPE,
    p_data_origin    VARCHAR2,
    p_err_out    OUT VARCHAR2);
  PROCEDURE p_load_adm_deposit(
    p_pidm           saradap.saradap_pidm%TYPE,
    p_term_code      saradap.saradap_term_code_entry%TYPE,
    p_appl_no        saradap.saradap_appl_no%TYPE,
    p_apdc_code      stvapdc.stvapdc_code%TYPE,
    p_data_origin    VARCHAR2,
    p_err_out    OUT VARCHAR2);
  PROCEDURE p_import_pers_contacts;
  PROCEDURE p_import_appl_interests;
  PROCEDURE p_import_applications_schools;
  PROCEDURE p_email_error;
  PROCEDURE p_email_error(
    p_proc_name      VARCHAR2);
  PROCEDURE p_write_error_log;
  PROCEDURE p_write_error_log(
    p_proc_name      VARCHAR2);
  PROCEDURE p_get_srrprel_err(
    p_file_loc VARCHAR2,
    p_file_name VARCHAR2
    );
  PROCEDURE p_email_multi_guids_err(
    p_adid_code     VARCHAR2);
  PROCEDURE p_email_multi_pidms_err(
    p_adid_code     VARCHAR2);
  PROCEDURE p_run_email_procedures;
END import_slate_data_2_pkg;
    
/

create or replace PACKAGE BODY import_slate_data_2_pkg
IS

  FUNCTION f_check_sbgi (
    f_sbgi_code IN VARCHAR2) RETURN stvsbgi.stvsbgi_code%TYPE 
  IS 
    valid_sbgi_cde  stvsbgi.stvsbgi_code%TYPE;  
  BEGIN
    BEGIN
      SELECT stvsbgi_code INTO valid_sbgi_cde 
      FROM stvsbgi
      WHERE stvsbgi_code = f_sbgi_code
      AND stvsbgi_type_ind IN ('H', 'C');
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        valid_sbgi_cde := '000000'; -- unknown school
    END;
    RETURN valid_sbgi_cde;
   
  END f_check_sbgi;
  --------
  
  
  FUNCTION f_string_midpoint(f_string  VARCHAR2) RETURN NUMBER
  IS 
    v_mid_len NUMBER;
    v_index NUMBER;
    v_mid_pos NUMBER;
  BEGIN
    v_mid_len := LENGTH(f_string)/2;
    v_index := 1;
    v_mid_pos := 0;
    WHILE INSTR(f_string, ' ', 1, v_index) <= v_mid_len AND INSTR(f_string, ' ', 1, v_index) != 0
    LOOP
      v_mid_pos := INSTR(f_string, ' ', 1, v_index);
      v_index := v_index + 1;
    END LOOP;

    RETURN v_mid_pos;
    
  END f_string_midpoint;

  ---------
  FUNCTION f_check_goradid_guid(f_additional_id IN goradid.goradid_additional_id%TYPE, f_adid_code IN goradid.goradid_adid_code%TYPE) RETURN NUMBER
  IS
    v_cnt NUMBER; 
  BEGIN
    SELECT COUNT(*)
      INTO v_cnt
      FROM goradid
      WHERE goradid_adid_code = f_adid_code
      AND goradid_additional_id = f_additional_id;

    RETURN v_cnt;
  END f_check_goradid_guid;
  ----------
  FUNCTION f_check_goradid_pidm(f_goradid_pidm IN goradid.goradid_pidm%TYPE, f_adid_code IN goradid.goradid_adid_code%TYPE) RETURN NUMBER
  IS
    v_cnt NUMBER; 
  BEGIN
    SELECT COUNT(*)
      INTO v_cnt
      FROM goradid
      WHERE goradid_pidm = f_goradid_pidm
      AND goradid_adid_code = f_adid_code;

    RETURN v_cnt;
  END f_check_goradid_pidm;
  ----------
  FUNCTION f_translate_ethn_code(f_ethn_code IN VARCHAR2, f_hispanic_ind IN VARCHAR2) 
    RETURN stvethn.stvethn_code%TYPE
  IS
    v_ethn_code    stvethn.stvethn_code%TYPE := NULL;
  BEGIN
    /*--------------------------------------------------------------------------
                WHEN HISPANIC IS NOT YES
    --------------------------------------------------------------------------*/
    IF f_hispanic_ind = 'No' OR f_hispanic_ind IS NULL
    THEN
      --ONE ETHNIC CODE
      IF length(f_ethn_code) = 2
      THEN
        v_ethn_code := f_ethn_code;        
      END IF;
      --THREE OR MORE ETHNIC CODES
      IF length(f_ethn_code) >= 6
      THEN
        v_ethn_code := 'MR';    
      END IF;
      --TWO ETHNIC CODES
      IF length(f_ethn_code) = 4
      THEN
        --BL and (AS or AP)	=BS
        IF (SUBSTR(f_ethn_code,1, 2) = 'BL' AND SUBSTR(f_ethn_code,3, 1) = 'A') 
        OR (SUBSTR(f_ethn_code,1, 1) = 'A' AND SUBSTR(f_ethn_code,3, 2) = 'BL')
        THEN
          v_ethn_code := 'BS';
        END IF;
          
        --BL and NA	=BN
        IF (SUBSTR(f_ethn_code,1, 2) = 'BL' AND SUBSTR(f_ethn_code,3, 2) = 'NA')
        OR (SUBSTR(f_ethn_code,1, 2) = 'NA' AND SUBSTR(f_ethn_code,3, 2) = 'BL')
        THEN
          v_ethn_code := 'BN';
        END IF;
            
        --BL and WH	=BW
        IF (SUBSTR(f_ethn_code,1, 2) = 'BL' AND SUBSTR(f_ethn_code,3, 2) = 'WH')
        OR (SUBSTR(f_ethn_code,1, 2) = 'WH' AND SUBSTR(f_ethn_code,3, 2) = 'BL')
        THEN
          v_ethn_code := 'BW';
        END IF;
            
        --NA and (AS or AP)	=NS
        IF (SUBSTR(f_ethn_code,1, 2) = 'NA' AND SUBSTR(f_ethn_code,3, 1) = 'A')
        OR (SUBSTR(f_ethn_code,1, 1) = 'A' AND SUBSTR(f_ethn_code,3, 2) = 'NA')
        THEN
          v_ethn_code := 'NS';
        END IF;
          
        --NA and WH	=NW
        IF (SUBSTR(f_ethn_code,1, 2) = 'NA' AND SUBSTR(f_ethn_code,3, 2) = 'WH')
        OR (SUBSTR(f_ethn_code,1, 2) = 'WH' AND SUBSTR(f_ethn_code,3, 2) = 'NA')
        THEN
          v_ethn_code := 'NW';
        END IF;
            
        --(AS or AP) and WH	=AH
        IF (SUBSTR(f_ethn_code,1, 1) = 'A' AND SUBSTR(f_ethn_code,3, 2) = 'WH')
        OR (SUBSTR(f_ethn_code,1, 2) = 'WH' AND SUBSTR(f_ethn_code,3, 1) = 'A')
        THEN
          v_ethn_code := 'AH';
        END IF;  
      END IF;
    END IF;
    
    
    /*--------------------------------------------------------------------------
                   WHEN HISPANIC IS YES
    --------------------------------------------------------------------------*/     
    IF f_hispanic_ind = 'Yes'
    THEN
      --HISPANIC AND NO OTHER ETHNIC CODE       
      IF  f_ethn_code IS NULL AND f_hispanic_ind = 'Yes'
      THEN
        v_ethn_code := 'HI';  
      END IF;
        
      --HISPANIC AND MORE THAN ONE OTHER ETHNIC CODE
      IF length(f_ethn_code) >= 2 
      THEN
        v_ethn_code := 'HM';
      END IF;
        
      --HISPANIC AND ONLY ONE OTHER ETHNIC CODE
      IF length(f_ethn_code) = 2
      THEN
        --the other ethnicity code is WH	=HW
        IF f_ethn_code = 'WH'
        THEN
          v_ethn_code := 'HW';
        END IF;
            
        --the other ethnicity code is NA	=HN
        IF f_ethn_code = 'NA'
        THEN
          v_ethn_code := 'HN';
        END IF;
            
        --the other ethnicity code is (AS or AP)	=HS
        IF SUBSTR(f_ethn_code,1, 1) = 'A'
        THEN
          v_ethn_code := 'HS';
        END IF;
            
        --the other ethnicity code is BL	=BH
        IF f_ethn_code = 'BL'
        THEN
          v_ethn_code := 'BH';
        END IF;
      END IF;
    END IF;
    
    RETURN v_ethn_code;
  END f_translate_ethn_code;
  ---------
  
  
  FUNCTION f_translate_race_code(f_race_code IN VARCHAR2, f_hispanic_ind IN VARCHAR2) 
    RETURN gorrace.gorrace_race_cde%TYPE
  IS
    v_gorprac_race gorrace.gorrace_race_cde%TYPE := NULL;
  BEGIN   
    -- If hispanic indicator is yes
    IF f_hispanic_ind = 'Yes'
    THEN
      IF length(f_race_code) >= 1
      THEN
        v_gorprac_race := 'M';
      ELSE
        v_gorprac_race := 'H';
      END IF;
    END IF;
    -- If hispanic indicator is not yes
    IF f_hispanic_ind = 'No' OR f_hispanic_ind IS NULL
    THEN
      IF length(f_race_code) > 1
      THEN
        v_gorprac_race := 'M';
      ELSE
        v_gorprac_race := f_race_code;
      END IF;
    END IF;
    RETURN v_gorprac_race;
  END f_translate_race_code;
  -------
FUNCTION f_translate_rtyp_code(f_prospect_type IN VARCHAR2, f_term_code IN VARCHAR2 , f_country_code IN VARCHAR2) RETURN srbrecr.srbrecr_rtyp_code%TYPE
  IS
    v_rtyp_code   srbrecr.srbrecr_rtyp_code%TYPE := NULL;
  BEGIN
    /** Translate RTYP_CODE **/
    IF f_prospect_type = 'FY'
    THEN
      IF f_country_code IS NULL OR f_country_code = 'US'
      THEN
        v_rtyp_code := 'F1';
      ELSE
        v_rtyp_code := 'F2';
      END IF;
    END IF;
      
    IF f_prospect_type = 'AC'
    THEN
      IF f_country_code IS NULL OR f_country_code = 'US'
      THEN
        v_rtyp_code := 'A1';
      ELSE
        v_rtyp_code := 'A2';
      END IF;
    END IF;
    
    IF f_prospect_type = 'TR'
    THEN
      IF f_country_code IS NULL OR f_country_code = 'US'
      THEN
        v_rtyp_code := 'T1';
      ELSE
        v_rtyp_code := 'T2';
      END IF;     
    END IF;    
    --visiting students
    IF f_prospect_type = 'VS'
    THEN
      --September Visiting
      IF SUBSTR(f_term_code, length(f_term_code)-1, 2) = '01'
      THEN
        IF f_country_code IS NULL OR f_country_code = 'US'
        THEN
          v_rtyp_code := 'V1';
        ELSE
          v_rtyp_code := 'V2';
        END IF;  
      --January Visiting
      ELSIF SUBSTR(f_term_code, length(f_term_code)-1, 2) = '03'
      THEN
        IF f_country_code IS NULL OR f_country_code = 'US'
        THEN
          v_rtyp_code := 'V3';
        ELSE
          v_rtyp_code := 'V4';
        END IF;   
      END IF;
    END IF; 
    RETURN v_rtyp_code;
  END f_translate_rtyp_code;
  -------
  FUNCTION f_translate_styp_code(f_prospect_type IN VARCHAR2) RETURN srbrecr.srbrecr_styp_code%TYPE
  IS
    v_styp_code   srbrecr.srbrecr_styp_code%TYPE := NULL;
  BEGIN
    /** Translate styp_code **/
    IF f_prospect_type = 'FY'
    THEN
      v_styp_code := 'M';
    END IF;
      
    IF f_prospect_type = 'AC'
    THEN
      v_styp_code := 'A';
    END IF;
    
    IF f_prospect_type = 'TR'
    THEN
      v_styp_code := 'B'; 
    END IF;
    
    IF f_prospect_type = 'VS'
    THEN
      v_styp_code := 'K'; 
    END IF;
    return v_styp_code;
  END f_translate_styp_code;
  --------
  FUNCTION f_get_primid_from_guid(f_additional_id IN goradid.goradid_additional_id%TYPE, f_adid_code IN goradid.goradid_adid_code%TYPE) RETURN spriden.spriden_id%TYPE
  IS
    v_spriden_primary_id  spriden.spriden_id%TYPE := NULL;
    CURSOR spriden_primid_c 
    IS
      SELECT spriden_id 
      FROM spriden
      WHERE spriden_pidm = 
            (SELECT goradid_pidm 
               FROM goradid 
              WHERE goradid_additional_id = f_additional_id 
                AND goradid_adid_code = f_adid_code)
      AND spriden_change_ind IS NULL;
  BEGIN
    IF spriden_primid_c%ISOPEN THEN
      CLOSE spriden_primid_c;
    END IF;
    
    OPEN spriden_primid_c;
    FETCH spriden_primid_c INTO v_spriden_primary_id;
    CLOSE spriden_primid_c;
    RETURN v_spriden_primary_id;
  EXCEPTION 
    WHEN TOO_MANY_ROWS
    THEN
      v_spriden_primary_id := NULL;
      RETURN v_spriden_primary_id;
  END f_get_primid_from_guid;
  
  --------
  FUNCTION f_get_pidm_from_guid(f_additional_id IN goradid.goradid_additional_id%TYPE, f_adid_code IN goradid.goradid_adid_code%TYPE) RETURN goradid.goradid_pidm%TYPE
  IS
    v_pidm        goradid.goradid_pidm%TYPE := NULL; 
    CURSOR goradid_pidm_c
    IS
      SELECT goradid_pidm FROM goradid
      WHERE goradid_additional_id = f_additional_id
      AND goradid_adid_code = f_adid_code;

  BEGIN
    IF goradid_pidm_c%ISOPEN 
    THEN 
     CLOSE goradid_pidm_c;
    END IF;
    OPEN goradid_pidm_c;
    FETCH goradid_pidm_c INTO v_pidm;
    CLOSE goradid_pidm_c;
    RETURN v_pidm;
  EXCEPTION
    WHEN TOO_MANY_ROWS
    THEN
      v_pidm := NULL;
      RETURN v_pidm;
  END f_get_pidm_from_guid;
  --------
  FUNCTION f_get_valid_natn(f_nation IN VARCHAR2) RETURN stvnatn.stvnatn_code%TYPE
  IS
    v_natn_code stvnatn.stvnatn_code%TYPE := NULL;
    v_is_natn_code VARCHAR2(1) := 'N';
    CURSOR stvnatn_code_exist_c
    IS
      SELECT 'Y' FROM stvnatn
      WHERE stvnatn_code =  f_nation;
    CURSOR stvnatn_get_code_c
    IS
      SELECT stvnatn_code FROM stvnatn
      WHERE stvnatn_nation =  f_nation;
  BEGIN
    IF stvnatn_code_exist_c%ISOPEN 
    THEN 
     CLOSE stvnatn_code_exist_c;
    END IF;
    OPEN stvnatn_code_exist_c;
    FETCH stvnatn_code_exist_c INTO v_is_natn_code;
    CLOSE stvnatn_code_exist_c;
    
    IF v_is_natn_code = 'Y'
    THEN
      v_natn_code :=  f_nation;
    ELSE
      IF stvnatn_get_code_c%ISOPEN THEN 
        CLOSE stvnatn_get_code_c;
      END IF;
      
      OPEN stvnatn_get_code_c;
      FETCH stvnatn_get_code_c INTO v_natn_code;
      CLOSE stvnatn_get_code_c;
    END IF;
    RETURN v_natn_code;
  END f_get_valid_natn;
  --------
  FUNCTION f_check_date_format(f_date_str IN VARCHAR2, f_date_format IN VARCHAR2) RETURN VARCHAR2
  IS 
    v_date DATE;
    v_is_valid VARCHAR2(1);
  BEGIN
    v_date := TO_DATE( f_date_str, f_date_format);
    v_is_valid := 'Y';
    RETURN v_is_valid;
  EXCEPTION
    WHEN OTHERS THEN
      v_is_valid := 'N';
      RETURN v_is_valid;
  END f_check_date_format;
  --------
  FUNCTION f_check_valid_majr(f_majr_code IN VARCHAR2) RETURN VARCHAR2
  IS
    v_is_valid      VARCHAR2(1);
  BEGIN
    BEGIN
      SELECT 'Y' 
      INTO v_is_valid
      FROM stvmajr
      WHERE stvmajr_code = f_majr_code;
     
      RETURN v_is_valid;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        v_is_valid := 'N';
        RETURN v_is_valid;
    END; 
  END f_check_valid_majr;
  ------
  PROCEDURE p_import_pers_to_temp
  IS  
    v_pros_pidm           szspros.szspros_pidm%TYPE;
    v_pros_type           szspros.szspros_pros_type%TYPE;
    v_term_code           szspros.szspros_term_code%TYPE;
    v_school_code         szspros.szspros_school_code%TYPE;
    v_school_type         szspros.szspros_school_type%TYPE;
    v_school_grad         szspros.szspros_school_grad%TYPE;
    v_rtyp_code           stvrtyp.stvrtyp_code%TYPE;
    v_key_seqno           srbrecr.srbrecr_admin_seqno%TYPE;
    v_error_mesg          VARCHAR2(200);
    v_szsiden_rec         szsiden%ROWTYPE;
    v_ridm                srtiden.srtiden_ridm%TYPE;
    v_srtiden_id          srtiden.srtiden_id%TYPE;
    v_srtiden_pidm        srtiden.srtiden_pidm%TYPE;
    v_srtiden_match_status srtiden.srtiden_match_status%TYPE;
    v_proc_name         VARCHAR2(60 CHAR) := 'p_import_pers_to_temp';
    v_goradid_guid_exist    NUMBER;
    v_goradid_pidm_exist    NUMBER;
    srbrecr_cnt           NUMBER;
    ident_rtyp_cnt        NUMBER;
    v_goradid_pidm        goradid.goradid_pidm%TYPE;
    srtprel_failed        EXCEPTION;
    no_szspros            EXCEPTION;
    guid_to_many_pidms    EXCEPTION;
    pidm_to_many_guids    EXCEPTION;
    diff_pidm_in_goradid  EXCEPTION;
    
    --Records with PIDM created in system
    CURSOR szsiden_c 
    IS
      SELECT * FROM szsiden
       WHERE guid NOT IN (SELECT srtiden_additional_id 
                           FROM srtiden 
                          WHERE srtiden_adid_code = 'SL');

  BEGIN    
    IF szsiden_c%ISOPEN THEN 
      CLOSE szsiden_c;
    END IF;
    OPEN szsiden_c;
    LOOP
      FETCH szsiden_c INTO v_szsiden_rec;
      EXIT WHEN szsiden_c%NOTFOUND;
      BEGIN 
        SAVEPOINT startpoint;
        --Get RIDM sequence--
        SELECT sobseqn_maxseqno + 1
          INTO v_ridm
          FROM saturn.sobseqn
         WHERE sobseqn_function = 'RIDM';
  
        UPDATE saturn.sobseqn
           SET sobseqn_maxseqno = v_ridm,
               sobseqn_activity_date = SYSDATE
         WHERE sobseqn_function = 'RIDM';
         
        --Check if guid already exist in goradid table
        v_goradid_guid_exist :=  f_check_goradid_guid(v_szsiden_rec.guid, 'SL');
        v_goradid_pidm_exist :=  f_check_goradid_pidm(v_szsiden_rec.pidm, 'SL');
        IF v_goradid_guid_exist > 1
        THEN
          RAISE guid_to_many_pidms; 
        END IF;
        IF v_goradid_pidm_exist > 1
        THEN
          RAISE pidm_to_many_guids;
        END IF;
        ------
        IF v_goradid_guid_exist = 1  AND v_szsiden_rec.pidm IS NULL--pidm not exist but alternate id is in spriden
        THEN
          v_srtiden_match_status := 'M';
          v_srtiden_id := f_get_primid_from_guid(v_szsiden_rec.guid, 'SL'); 
          v_srtiden_pidm := f_get_pidm_from_guid(v_szsiden_rec.guid, 'SL'); 
        END IF;
        
        IF v_szsiden_rec.pidm IS NOT NULL --pidm exist in general
        THEN
          /*IF v_goradid_pidm_exist = 1
          THEN
            v_goradid_pidm := f_get_pidm_from_guid(v_szsiden_rec.guid, 'SL');
          END IF;
          IF v_goradid_pidm <>  v_szsiden_rec.pidm
          THEN
            RAISE diff_pidm_in_goradid;
          END IF;*/
          v_srtiden_match_status := 'M';
          v_srtiden_id := v_szsiden_rec.id;
          v_srtiden_pidm := v_szsiden_rec.pidm;    

        END IF;
        
        IF v_goradid_guid_exist = 0 AND v_szsiden_rec.pidm IS NULL --pidm not exist and alternate id not found in spriden -- new
        THEN
          v_srtiden_match_status := NULL;
          v_srtiden_id := srkprel.f_get_prospect_sobseqn ('PROSPECT_ID');
          v_srtiden_pidm := NULL; 
        END IF; 
        
        --SRTIDEN: Ellucian staging table for SPRIDEN
        --------------------------------------------------
        BEGIN
          INSERT INTO saturn.srtiden
                  (srtiden_ridm, 
                  srtiden_id,
                  srtiden_pidm,
                  srtiden_last_name,
                  srtiden_first_name,
                  srtiden_mi,
                  srtiden_additional_id,
                  srtiden_adid_code,
                  srtiden_match_status,
                  srtiden_activity_date,
                  srtiden_data_origin,
                  srtiden_user_id
                  )
          VALUES 
                  (v_ridm, 
                  v_srtiden_id,
                  v_srtiden_pidm,
                  v_szsiden_rec.last_name,
                  v_szsiden_rec.first_name,
                  v_szsiden_rec.middle_name,
                  v_szsiden_rec.guid,
                  'SL',
                  v_srtiden_match_status,
                  SYSDATE,
                  'SLATE',
                  gb_common.f_sct_user    
                  );

        EXCEPTION
          WHEN OTHERS THEN
            RAISE;
        END;
        --Retrieve prospect related data from staging table SZSPROS--
        BEGIN
          SELECT szspros_pidm, szspros_pros_type, szspros_term_code, szspros_school_code, szspros_school_type,  szspros_school_grad
          INTO v_pros_pidm, v_pros_type, v_term_code, v_school_code, v_school_type, v_school_grad
          FROM szspros 
          WHERE szspros_guid = v_szsiden_rec.guid; 
        EXCEPTION
          WHEN NO_DATA_FOUND THEN
            RAISE no_szspros;
        END;
        --If term code is null and prospect type is FY, refer to grad date for term code
        IF v_pros_type = 'FY' AND v_term_code IS NULL 
        THEN 
          v_term_code := TO_CHAR(TO_NUMBER(SUBSTR(v_school_grad, 1, 4)) + 1)|| '01';
        END IF;
        
        v_rtyp_code := f_translate_rtyp_code(v_pros_type, v_term_code, v_szsiden_rec.ma_natn_code);
        
        --======================= JAN 3, 2018 CHANGES============ 
        --USE API TO UPDATE RECRUIT TYPE DIRECTLY IN SRBRECR
        --ONLY IF RECRUIT TYPE IN STAGE TABLE NOT MATCH WHAT'S CURRENTLY
        --IN BANNER
          
        IF v_pros_pidm IS NOT NULL
        THEN
          SELECT COUNT(*)
            INTO srbrecr_cnt
            FROM srbrecr
            WHERE srbrecr_pidm = v_pros_pidm
            AND srbrecr_term_code = v_term_code;
          SELECT COUNT(*)
            INTO ident_rtyp_cnt
            FROM srbrecr
            WHERE srbrecr_pidm = v_pros_pidm
            AND srbrecr_term_code = v_term_code
            AND srbrecr_rtyp_code = v_rtyp_code;
          IF srbrecr_cnt = 1 AND ident_rtyp_cnt = 0
          THEN-- has srbrecr record, but diff rtyp_code, then update
            v_key_seqno := baninst1.sb_recruit.F_GetSrbrecrSeqno(v_pros_pidm,  v_term_code, 'C');

            BEGIN
              BANINST1.SB_RECRUIT.P_UPDATE(
                p_pidm  => v_pros_pidm,
                p_term_code => v_term_code,
                p_admin_seqno => v_key_seqno,
                p_rtyp_code => v_rtyp_code,
                p_data_origin => 'SLATE',
                p_user_id => gb_common.f_sct_user);
              COMMIT; 
            
            EXCEPTION
              WHEN OTHERS THEN
                v_error_mesg :=  'Failed to update to new recruit type for prospect record. ' || SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300); 
                p_handle_err(v_szsiden_rec.guid, v_error_mesg, v_proc_name);        
            END;
          END IF;
        END IF;
        --=======================END JAN 3, 2018 CHANGES============ 
        
        
        --------------------------------------
        --SRTPREL: Ellucian Staging Table for Prospect Data
        --------------------------------------------------
        v_error_mesg := NULL;
        
        p_insert_srtprel( v_szsiden_rec.guid, v_ridm, 'SDLP', v_term_code,
                          v_rtyp_code,
                          f_translate_styp_code(v_pros_type),
                          'SLATE', v_error_mesg);
        IF v_error_mesg IS NOT NULL
        THEN
          RAISE srtprel_failed;
        END IF;
        
        --SRTPERS: Ellucian staging table for SPBPERS
        --------------------------------------------------
        BEGIN
          SAVEPOINT pre_srtpers;
          INSERT INTO saturn.srtpers
                  (srtpers_ridm, 
                  srtpers_birth_date, 
                  srtpers_ssn, 
                  srtpers_sex,
                  srtpers_pref_first_name,
                  srtpers_citz_code, 
                  srtpers_ethn_code,
                  srtpers_birth_day,
                  srtpers_birth_mon,
                  srtpers_birth_year,
                  srtpers_activity_date, 
                  srtpers_data_origin,
                  srtpers_user_id
                  )
          VALUES
                  (v_ridm, 
                  TO_DATE(v_szsiden_rec.dob, 'YYYY-MM-DD'),
                  v_szsiden_rec.ssn,
                  'F',  
                  v_szsiden_rec.pref_first,
                  v_szsiden_rec.citz_code,
                  f_translate_ethn_code(v_szsiden_rec.ethn_code, v_szsiden_rec.hispanic),
                  to_char(TO_DATE(v_szsiden_rec.dob, 'YYYY-MM-DD'), 'DD'), 
                  to_char(TO_DATE(v_szsiden_rec.dob, 'YYYY-MM-DD'), 'MM'), 
                  to_char(TO_DATE(v_szsiden_rec.dob, 'YYYY-MM-DD'), 'YY'), 
                  SYSDATE,
                  'SLATE',
                  gb_common.f_sct_user 
                  );
        EXCEPTION
          WHEN OTHERS THEN
            ROLLBACK TO pre_srtpers;
            v_error_mesg :=  'Failed to load bio/demo data for record. ' || SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300); 
            p_handle_err(v_szsiden_rec.guid, v_error_mesg, v_proc_name);
        END;    
        
        --SRTPRAC: Ellucian staging table for GORPRAC
        --------------------------------------------------
        IF f_translate_race_code(v_szsiden_rec.race_code, v_szsiden_rec.hispanic) IS NOT NULL
        THEN
          BEGIN
            SAVEPOINT pre_srtprac;
            INSERT INTO saturn.srtprac
                    ( srtprac_ridm,
                    srtprac_race_cde,
                    srtprac_activity_date,
                    srtprac_data_origin,
                    srtprac_user_id
                    )
            VALUES 
                    (v_ridm, 
                    f_translate_race_code(v_szsiden_rec.race_code, v_szsiden_rec.hispanic),
                    SYSDATE,
                    'SLATE',
                    gb_common.f_sct_user  
                    );    
          EXCEPTION
            WHEN OTHERS THEN
              ROLLBACK TO pre_srtprac;
              v_error_mesg :=  'Failed to load race data for record. ' || SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300); 
              p_handle_err(v_szsiden_rec.guid, v_error_mesg, v_proc_name);
          END;
        END IF;
        
        --SRTINTL: Ellucian staging table for GOBINTL
        -------------------------------------------------- 
        IF (v_szsiden_rec.citz_natn IS NOT NULL )
        THEN
          BEGIN
            SAVEPOINT pre_srtintl;
            INSERT INTO SRTINTL (SRTINTL_RIDM, SRTINTL_ACTIVITY_DATE, SRTINTL_NATN_CODE_LEGAL)
            VALUES (v_ridm, SYSDATE, v_szsiden_rec.citz_natn);   
          EXCEPTION
            WHEN OTHERS THEN
              ROLLBACK TO pre_srtintl;
              v_error_mesg :=  'Failed to load international nation data for record. ' || SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300); 
              p_handle_err(v_szsiden_rec.guid, v_error_mesg, v_proc_name);
          END;
        END IF;
        
        --SRTADDR: Ellucian staging table for SPRADDR
        -------------------------------------------------- 
        --mailing address
        IF ( v_szsiden_rec.ma_street1 IS NOT NULL) 
        THEN
          v_error_mesg := NULL;
          p_insert_srtaddr( v_szsiden_rec.guid,
                            v_ridm,
                            v_szsiden_rec.ma_street1,
                            v_szsiden_rec.ma_street2,
                            v_szsiden_rec.ma_street3,
                            v_szsiden_rec.ma_city,
                            v_szsiden_rec.ma_region,
                            v_szsiden_rec.ma_postal,
                            v_szsiden_rec.ma_natn_code,
                            'MA',
                            TO_DATE(v_szsiden_rec.ma_from_date, 'YYYY-MM-DD'),
                            TO_DATE(v_szsiden_rec.ma_to_date, 'YYYY-MM-DD'),
                            'SLATE',
                            v_error_mesg);
          IF v_error_mesg IS NOT NULL
          THEN
            p_handle_err(v_szsiden_rec.guid, v_error_mesg, v_proc_name);
          END IF;
        END IF;
      
        --permanent address
        IF ( v_szsiden_rec.cur_street1 IS NOT NULL) 
        THEN
          v_error_mesg := NULL;
          p_insert_srtaddr( v_szsiden_rec.guid,
                            v_ridm,
                            v_szsiden_rec.cur_street1,
                            v_szsiden_rec.cur_street2,
                            v_szsiden_rec.cur_street3,
                            v_szsiden_rec.cur_city,
                            v_szsiden_rec.cur_region,
                            v_szsiden_rec.cur_postal,
                            v_szsiden_rec.cur_natn_code,
                            'PR',
                            NULL,
                            NULL,
                            'SLATE',
                            v_error_mesg);
          IF v_error_mesg IS NOT NULL
          THEN
            p_handle_err(v_szsiden_rec.guid, v_error_mesg, v_proc_name);
          END IF;
        END IF; 
        
        --SRTTELE: Ellucian staging table for SPRTELE
        --------------------------------------------------
        --if cellphone is not null
        IF ( v_szsiden_rec.mobile_phone IS NOT NULL) 
        THEN
          v_error_mesg := NULL;
          p_insert_srttele(v_szsiden_rec.guid, v_ridm, v_szsiden_rec.mobile_phone, 'CL', 'SLATE', v_error_mesg);
          IF v_error_mesg IS NOT NULL
          THEN
            p_handle_err(v_szsiden_rec.guid, v_error_mesg, v_proc_name);
          END IF;
        END IF;
        
        --if daytime phone is not null
        IF ( v_szsiden_rec.day_phone IS NOT NULL) 
        THEN
          v_error_mesg := NULL;
          p_insert_srttele(v_szsiden_rec.guid, v_ridm, v_szsiden_rec.day_phone, 'PH', 'SLATE', v_error_mesg);
          IF v_error_mesg IS NOT NULL
          THEN
            p_handle_err(v_szsiden_rec.guid, v_error_mesg, v_proc_name);
          END IF;
        END IF;
        
        --SRTEMAL: Ellucian staging table for GOREMAL
        --------------------------------------------------
        IF v_szsiden_rec.email IS NOT NULL
        THEN
          v_error_mesg := NULL;
          p_insert_srtemal(v_szsiden_rec.guid, v_ridm, v_szsiden_rec.email, 'EH', 'SLATE', v_error_mesg);
          IF v_error_mesg IS NOT NULL
          THEN
            p_handle_err(v_szsiden_rec.guid, v_error_mesg, v_proc_name);
          END IF;
        END IF;
        
        --SRTHSCH: Ellucian staging table for SORHSCH
        --------------------------------------------------
        IF v_school_type = 'High School'
        THEN
          v_error_mesg := NULL;
          p_insert_srthsch( v_szsiden_rec.guid, v_ridm, v_school_code, TO_DATE(v_school_grad, 'YYYY-MM-DD'), v_term_code, 'SLATE', v_error_mesg);
          IF v_error_mesg IS NOT NULL
          THEN
            p_handle_err(v_szsiden_rec.guid, v_error_mesg, v_proc_name);
          END IF;
        --SRTPCOL: Ellucian staging table for SORPCOL
        --------------------------------------------------
        ELSIF v_school_type = 'College'
        THEN
          v_error_mesg := NULL;
          p_insert_srtpcol( v_szsiden_rec.guid, v_ridm, 'CC' || v_school_code, TO_DATE(v_school_grad, 'YYYY-MM-DD'), v_term_code, 'SLATE', v_error_mesg);   
          IF v_error_mesg IS NOT NULL
          THEN
            p_handle_err(v_szsiden_rec.guid, v_error_mesg, v_proc_name);
          END IF;
        END IF;
        --Commit all changes to SRT tables
        COMMIT;
      EXCEPTION
        WHEN no_szspros
        THEN
          ROLLBACK TO startpoint;
          v_error_mesg :=  'No corresponding data in temporary table SZSPROS. Cannot load to Ellucian temporary tables for prospect/person.';
          p_handle_err(v_szsiden_rec.guid, v_error_mesg, v_proc_name);
        WHEN srtprel_failed
        THEN
          ROLLBACK TO startpoint;
          v_error_mesg :=  'Error loading to temporary prospect info table SRTPREL. Cannot load to Ellucian temporary tables for prospect/person.';
          p_handle_err(v_szsiden_rec.guid, v_error_mesg, v_proc_name);
        WHEN guid_to_many_pidms
        THEN
          ROLLBACK TO startpoint;
          v_error_mesg :=  'Slate Ref ID has more than one Banner IDs. Cannot load to Ellucian temporary tables for prospect/person.';
          p_handle_err(v_szsiden_rec.guid, v_error_mesg, v_proc_name);       
        
        WHEN pidm_to_many_guids
        THEN  
          ROLLBACK TO startpoint;
          v_error_mesg :=  'Banner ID ' || v_szsiden_rec.id || ' has more than one Slate Ref IDs. Cannot load to Ellucian temporary tables for prospect/person.';
          p_handle_err(v_szsiden_rec.guid, v_error_mesg, v_proc_name);
          
        /*WHEN diff_pidm_in_goradid
        THEN
          ROLLBACK TO startpoint;
          v_error_mesg :=  'Slate Ref ID is mapped to another ID in Banner. Cannot load to Ellucian temporary tables for prospect/person.';
          p_handle_err(v_szsiden_rec.guid, v_error_mesg, v_proc_name);*/
        
        WHEN OTHERS THEN
          ROLLBACK TO startpoint;
          v_error_mesg :=  'Error loading to Ellucian temporary tables for prospect/person.' || SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300); 
          p_handle_err(v_szsiden_rec.guid, v_error_mesg, v_proc_name);
      END;
    END LOOP; 
    CLOSE szsiden_c;
  END p_import_pers_to_temp ;
  ---------
  
  PROCEDURE p_import_applicants
  IS
    v_row_id_in           gb_common.internal_record_id_type;
    v_row_id_out          gb_common.internal_record_id_type;
    v_pidm                szsappl.szsappl_pidm%TYPE;
    v_guid                szsappl.szsappl_guid%TYPE;
    v_term_code           szsappl.szsappl_term_code_entry%TYPE;
    v_key_seqno           srbrecr.srbrecr_admin_seqno%TYPE;
    v_pros_type           srbrecr.srbrecr_rtyp_code%TYPE;
    v_admt_code           saradap.saradap_admt_code%TYPE;
    v_styp_code           saradap.saradap_styp_code%TYPE;
    v_resd_code           saradap.saradap_resd_code%TYPE;
    v_appl_date           saradap.saradap_appl_date%TYPE;
    old_levl_code         saradap.saradap_levl_code%TYPE;
    old_appl_date         saradap.saradap_appl_date%TYPE;
    old_styp_code         saradap.saradap_styp_code%TYPE;
    old_admt_code         saradap.saradap_admt_code%TYPE;
    old_resd_code         saradap.saradap_resd_code%TYPE;
    v_appl_no_out         saradap.saradap_appl_no%TYPE;
    v_appl_no_in          saradap.saradap_appl_no%TYPE;
    v_majr_code           sorlfos.sorlfos_majr_code%TYPE; 
    v_appl_src            sarrsrc.sarrsrc_sbgi_code%TYPE;
    v_saradap_exist       VARCHAR2(1);
    szsappl_rec           szsappl%ROWTYPE;
    v_recommend_dec       szsappl.szsappl_rec_dec%TYPE;
    v_confirmed_dec       szsappl.szsappl_fin_dec%TYPE;
    v_dec_history         szsappl_rec.szsappl_dec_hist%TYPE;
    v_sarappd_appl_no     sarappd.sarappd_appl_no%TYPE;
    v_error_mesg          VARCHAR2(200);
    v_proc_name         VARCHAR2(60 CHAR) := 'p_import_applicants';
    no_srbrecr_row      EXCEPTION;

  
    --Records with PIDM created in system but must be one-to-one
    CURSOR szsappl_c 
    IS
      SELECT szsappl.* FROM szsappl
      JOIN goradid ON szsappl_guid = goradid_additional_id
      WHERE goradid_adid_code  = 'SL'    
      AND NOT EXISTS 
        (SELECT goradid_additional_id 
        FROM goradid 
        WHERE goradid_additional_id = szsappl_guid
        AND goradid_adid_code = 'SL'
        GROUP BY goradid_additional_id HAVING COUNT(*) >1)
      AND NOT EXISTS
        (SELECT goradid_pidm
        FROM goradid
        WHERE goradid_pidm = szsappl_pidm
        AND goradid_adid_code = 'SL' 
        GROUP BY goradid_pidm HAVING COUNT(*) >1);
        
  BEGIN    
    IF szsappl_c%ISOPEN
    THEN
      CLOSE szsappl_c;
    END IF;
    OPEN szsappl_c;
    LOOP
      FETCH szsappl_c INTO szsappl_rec;
      EXIT WHEN szsappl_c%NOTFOUND;
      v_pidm := f_get_pidm_from_guid(szsappl_rec.szsappl_guid, 'SL');
      v_guid  := szsappl_rec.szsappl_guid;
      v_term_code := szsappl_rec.szsappl_term_code_entry;
      v_key_seqno := baninst1.sb_recruit.F_GetSrbrecrSeqno(v_pidm,  v_term_code, 'C');
      v_error_mesg := NULL;
      BEGIN 
        IF v_key_seqno = 0
        THEN
          RAISE no_srbrecr_row; --no matching recruit record in SRBRECR
        END IF;  
      
        --Get admit code-- need to look at prospect type for visiting cases
        SELECT srbrecr_rtyp_code 
        INTO v_pros_type
        FROM srbrecr 
        WHERE srbrecr_pidm = v_pidm 
        AND srbrecr_term_code = v_term_code
        AND srbrecr_admin_seqno = v_key_seqno;
        
        --Get admit code
          --January Visiting:   Slate Prospect Type = VS, Round = TJ
          --September Visiting: Slate Prospect Type = VS, Round = TS
          --January Transfer:   Slate Prospect Type = TJ, Round = TJ
          --September Transfer: Slate Prospect Type = TS, Round = TS
          --Others inferred by Round
        IF szsappl_rec.szsappl_admt_code = 'TJ'
        THEN
          IF NVL(v_pros_type, 'NULL') LIKE 'V%'
          THEN 
            --January Visiting
            v_admt_code := 'VJ';
          ELSE 
            --January Transfer
            v_admt_code := szsappl_rec.szsappl_admt_code;
          END IF;
        ELSIF szsappl_rec.szsappl_admt_code = 'TS'
        THEN
          IF NVL(v_pros_type, 'NULL') LIKE 'V%'
          THEN 
            --September Visiting
            v_admt_code := 'VS';
          ELSE 
            --September Transfer
            v_admt_code := szsappl_rec.szsappl_admt_code;
          END IF;    
        ELSE 
          --Other round codes
          v_admt_code := szsappl_rec.szsappl_admt_code;
        END IF;
        
        --Get residence code
        IF SUBSTR(szsappl_rec.szsappl_admt_code, 1 , 1) = 'A'
        THEN
          v_resd_code := szsappl_rec.szsappl_ada_resd;    
        ELSIF SUBSTR(szsappl_rec.szsappl_admt_code, 1 , 1) = 'T'
        THEN
          v_resd_code := szsappl_rec.szsappl_tr_resd;        
        ELSE
          v_resd_code := szsappl_rec.szsappl_fy_resd;
        END IF; 
        --Translate student type
        IF szsappl_rec.szsappl_school1_ctry <> 'US'  
        THEN
          v_styp_code := 'Z';     
        ELSE
          v_styp_code := 'Y';
        END IF;
        --Get submit date
        v_appl_date := TO_DATE(szsappl_rec.szsappl_appl_date, 'YYYY-MM-DD');
        --Get substring for major
        IF szsappl_rec.szsappl_acad_ints IS NOT NULL
        THEN
          --Major = first 4 letter of academic interest
          v_majr_code := SUBSTR(szsappl_rec.szsappl_acad_ints,1, 4);
          --Check if major code is in validation table
          IF f_check_valid_majr(v_majr_code) = 'N'
          THEN
            v_error_mesg := 'Major code: ' || v_majr_code || ' is not found in STVMAJR validation table. Changed to UNKNOWN major.';
            p_handle_err(v_guid, v_error_mesg, v_proc_name);
            v_majr_code := '0000';
          END IF;
        ELSE
          v_majr_code := '0000';
        END IF;
      
        -- EXISTENCE CHECK ON SARADAP          - 1 TERM = 1 APPL PER PERSON      
        -------------------------------------------------------------------------
        v_saradap_exist := NULL;   
        BEGIN
          SELECT DISTINCT 'Y', saradap_appl_no, rowid
          INTO v_saradap_exist, v_appl_no_in, v_row_id_in 
          FROM saradap 
          WHERE saradap_pidm = v_pidm
          AND saradap_term_code_entry = v_term_code
          AND saradap_levl_code = 'UG';
        EXCEPTION
          WHEN NO_DATA_FOUND THEN
            v_saradap_exist := 'N';       
        END;
        -- Create or update application record in SARADAP
        IF v_saradap_exist = 'N' 
        THEN
          v_appl_no_out := NULL;
          v_error_mesg := NULL;
          
          -- Create appplication if none exist that match
          -------------------------------------------------------------------------
          baninst1.sakmods.p_create_application(
                               commplan_ind_in       => 'N',
                               source_ind_in         => NULL,
                               admr_code_cc_in       => NULL,
                               chkl_comment_cc_in    => NULL,
                               complete_date_in      => NULL,
                               processing_date_in    => SYSDATE,
                               wadf_auto_appl_ind_in => 'N',
                               pidm_in               => v_pidm,
                               term_in               => v_term_code,
                               levl_in               => 'UG',
                               appl_date_in          => v_appl_date,
                               apst_in               => 'A',
                               apst_date_in          => SYSDATE,
                               maint_ind_in          => 'S',
                               admt_in               => v_admt_code,
                               styp_in               => v_styp_code,
                               camp_in               => '0',
                               site_in	             => NULL,
                               coll1_in              => 'SM',
                               degc1_in              => 'AB',
                               majr1_in              => v_majr_code,
                               resd_in     	         => v_resd_code,
                               full_part_ind_in      => NULL,             
                               activity_date_in      => SYSDATE,
                               web_acct_misc_ind_in  => NULL,
                               web_cashier_user_in   => NULL,
                               web_trans_no_in       => NULL,
                               web_amount_in         => NULL,
                               web_receipt_number_in => NULL,
                               waiv_code_in          => NULL,
                               err_msg               => v_error_mesg,
                               adap_applno           => v_appl_no_out,
                               sbgi_srce_code_in     =>  NULL,
                               csts_code_in	         => 'INPROGRESS',
                               cact_code_in	         => 'ACTIVE',
                               checklist_ind_in      => 'N',
                               override_severity_in  => 'N',
                               /* CC 7.3: Add application preference */
                               appl_preference_in    => NULL,
                               priority_in           => 1);
          IF v_error_mesg IS NOT NULL
          THEN
            ROLLBACK;
            v_error_mesg :=  'Unable to create application, field of study, curriculum for Banner pidm '|| v_pidm || '. ' || v_error_mesg; 
            p_handle_err(v_guid, v_error_mesg, v_proc_name); 
          ELSE
            COMMIT;  
            v_sarappd_appl_no := v_appl_no_out;
          END IF;
        ELSE
         v_sarappd_appl_no := v_appl_no_in;
        
          SELECT saradap_levl_code, saradap_appl_date, saradap_styp_code, saradap_admt_code, saradap_resd_code 
            INTO old_levl_code, old_appl_date, old_styp_code, old_admt_code, old_resd_code
            FROM saradap
            WHERE saradap_pidm = v_pidm 
            AND saradap_appl_no = v_appl_no_in 
            AND saradap_term_code_entry = v_term_code;
       
          IF ('UG' <> NVL(old_levl_code, 'NULL') )--need to by dynamic in future
          OR (NVL(to_char(v_appl_date,'DD-MON-YYYY'), 'NULL') <> NVL(to_char(old_appl_date,'DD-MON-YYYY'), 'NULL') )
          OR (NVL(v_styp_code, 'NULL') <> NVL(old_styp_code, 'NULL') )
          OR (NVL(v_admt_code, 'NULL') <> NVL(old_admt_code, 'NULL') )
          OR (NVL(v_resd_code, 'NULL') <> NVL(old_resd_code, 'NULL') )
          THEN
            BEGIN
              -- Update SARADAP here when associated row is already there
              -------------------------------------------------------------------------
              baninst1.sb_admissionsapplication.p_update(
                      p_pidm              =>  v_pidm,
                      p_term_code_entry   =>  v_term_code,
                      p_appl_no           =>  v_appl_no_in,
                      p_appl_date         =>  v_appl_date,
                      p_apst_code         =>  'A',
                      p_apst_date         =>  SYSDATE,
                      p_maint_ind         =>  'S',
                      p_admt_code         =>  v_admt_code,
                      p_styp_code         =>  v_styp_code,
                      p_resd_code         =>  v_resd_code,
                      p_data_origin       =>  'SLATE',
                      p_user_id           =>  gb_common.f_sct_user,
                      p_rowid             =>  v_row_id_in);
              gb_common.p_commit;
            EXCEPTION
              WHEN OTHERS THEN --record the Oracle error in the track error table
                v_error_mesg := 'Unable to use API to update application data in SARADAP for Banner pidm '|| v_pidm || '. ' || SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300);
                p_handle_err(v_guid, v_error_mesg, v_proc_name);
            END; 
          END IF; 
          --Insert into SORLCUR and SORLFOS/keep this out of IF condition above because major may change?
          p_insert_appl_cur_fos(v_guid, v_pidm, v_term_code, 'UG', 'AB', v_appl_no_in, v_majr_code, v_admt_code, 'SLATE');
        END IF;
        IF v_sarappd_appl_no IS NOT NULL
        THEN
          -------------------------PRELIMINARY RATING-------------------------
          IF szsappl_rec.szsappl_pr_rating IS NOT NULL
          THEN
            v_error_mesg := NULL;
            p_load_adm_rating(v_pidm, v_term_code, v_sarappd_appl_no, 'PR',   szsappl_rec.szsappl_pr_rating, 'ATOWNER', 'SLATE', v_error_mesg);
            IF v_error_mesg IS NOT NULL THEN
              p_handle_err(v_guid, v_error_mesg, 'p_load_adm_rating');
            END IF;
          END IF;
          ----------------------------READER RATING----------------------------
          IF szsappl_rec.szsappl_rr_rating IS NOT NULL
          THEN
            v_error_mesg := NULL;
            p_load_adm_rating(v_pidm, v_term_code, v_sarappd_appl_no, 'RR',   szsappl_rec.szsappl_rr_rating, 'ATOWNER', 'SLATE', v_error_mesg);
            IF v_error_mesg IS NOT NULL THEN
              p_handle_err(v_guid, v_error_mesg, 'p_load_adm_rating');
            END IF;
          END IF;
          ----------------------------DECISIONS---------------------------------
          v_recommend_dec := NVL(szsappl_rec.szsappl_rec_dec, '00');
          v_confirmed_dec := NVL(szsappl_rec.szsappl_fin_dec, '00');
          v_dec_history := NVL(szsappl_rec.szsappl_dec_hist, 'NULL');
          --obsolete decision codes: 14, 16, 17, 19
          -- 18 (waitlist admit) still relevant but can only be inferred from confirmed decision history
          
          IF  v_recommend_dec IN ('11', '12', '13','15')
          AND v_confirmed_dec NOT IN ('11', '12', '13','15')
          THEN
            v_error_mesg := NULL;       
            p_load_adm_decision(v_pidm, v_term_code, v_sarappd_appl_no, v_recommend_dec, 'SLATE', v_error_mesg);
            
            IF v_error_mesg IS NOT NULL THEN
              p_handle_err(v_guid, v_error_mesg, 'p_load_adm_decision');
            END IF;   
          END IF;

          -------------------------CONFIRMED DECISION (FINAL)-------------------------
          --Slate Decision- Wait List, Denied, Defered (12, 13, 15)
          IF v_confirmed_dec IN ('12', '13', '15')
          THEN
            v_error_mesg := NULL;       
            p_load_adm_decision(v_pidm, v_term_code, v_sarappd_appl_no, v_confirmed_dec, 'SLATE', v_error_mesg);
            IF v_error_mesg IS NOT NULL 
            THEN
              p_handle_err(v_guid, v_error_mesg, 'p_load_adm_decision');
            END IF;  
          END IF;

          --Slate Decision- Admit (11)
          IF v_confirmed_dec = '11'
          THEN
            --Regular Admit (Admit 11)
            IF LOWER(v_dec_history) NOT LIKE '%wait list%' 
            THEN
              v_error_mesg := NULL;       
              p_load_adm_decision(v_pidm, v_term_code, v_sarappd_appl_no, v_confirmed_dec, 'SLATE', v_error_mesg);
                
              IF v_error_mesg IS NOT NULL 
              THEN
                p_handle_err(v_guid, v_error_mesg, 'p_load_adm_response');
              END IF;
            --Admitted after Waitlist (Waitlist Admit 18)
            ELSE 
              v_error_mesg := NULL;       
              p_load_adm_decision(v_pidm, v_term_code, v_sarappd_appl_no, '18', 'SLATE', v_error_mesg);
                
              IF v_error_mesg IS NOT NULL 
              THEN
                p_handle_err(v_guid, v_error_mesg, 'p_load_adm_response');
              END IF;
            END IF;
          END IF;
          --Slate Decision that are Banner responses- 20, 21, 22, 40, 41, 43, 44, 46
          IF v_confirmed_dec IN ('20', '21', '22') 
          OR (SUBSTR(v_confirmed_dec, 1, 1) = '4' AND v_confirmed_dec <> '45')
          THEN
            v_error_mesg := NULL;       
            p_load_adm_response(v_pidm, v_term_code, v_sarappd_appl_no, v_confirmed_dec, 'SLATE', v_error_mesg);
            
            IF v_error_mesg IS NOT NULL 
            THEN
               p_handle_err(v_guid, v_error_mesg, 'p_load_adm_response');
            END IF;
          END IF;
          --Slate Decision- Withdraw(45)
          IF v_confirmed_dec = '45'
          THEN
            --Withdraw before deposit (Withdraw Before Decision 45)
            IF LOWER(v_dec_history) NOT LIKE '%admit/matric%'
            THEN
              v_error_mesg := NULL;       
              p_load_adm_response(v_pidm, v_term_code, v_sarappd_appl_no, v_confirmed_dec, 'SLATE', v_error_mesg);
                
              IF v_error_mesg IS NOT NULL 
              THEN
                p_handle_err(v_guid, v_error_mesg, 'p_load_adm_response');
              END IF;
            --Withdraw after Deposit (Applicant Cancels 42)
            ELSE
              v_error_mesg := NULL;       
              p_load_adm_response(v_pidm, v_term_code, v_sarappd_appl_no, '42', 'SLATE', v_error_mesg);
                
              IF v_error_mesg IS NOT NULL 
              THEN
                p_handle_err(v_guid, v_error_mesg, 'p_load_adm_response');
              END IF;
            END IF;
          END IF;
        END IF; 
      EXCEPTION
        WHEN no_srbrecr_row THEN --record the Oracle error in the track error table
          v_error_mesg :=  'Unable to start processing applicant records for Banner pidm '|| v_pidm || ' because SRBRECR record for associated term ' || v_term_code || ' is not found.' ; 
          p_handle_err(v_guid, v_error_mesg, v_proc_name);    
        WHEN OTHERS THEN
          v_error_mesg :=  'Error processing applicant data for Banner pidm '|| v_pidm|| '. ' || SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300); 
          p_handle_err(v_guid, v_error_mesg, v_proc_name);
      END;     
    END LOOP;
    CLOSE szsappl_c;

  END p_import_applicants;
  ------------
  PROCEDURE p_insert_appl_cur_fos(
    p_guid          goradid.goradid_additional_id%TYPE DEFAULT NULL,
    p_pidm          sorlcur.sorlcur_pidm%TYPE,
    p_term_code     sorlcur.sorlcur_term_code%TYPE,
    p_levl_code     sorlcur.sorlcur_levl_code%TYPE,
    p_degree_code   sorlcur.sorlcur_degc_code%TYPE,
    p_appl_no       sorlcur.sorlcur_key_seqno%TYPE,
    p_majr_code     sorlfos.sorlfos_majr_code%TYPE,
    p_admt_code     sorlcur.sorlcur_admt_code%TYPE,
    p_data_origin   sorlcur.sorlcur_data_origin%TYPE)
    
  IS
    v_row_id_in           gb_common.internal_record_id_type;
    v_row_id_out          gb_common.internal_record_id_type;
    v_lcur_lmod_code      sorlcur.sorlcur_lmod_code%TYPE := sb_curriculum_str.f_ADMISSIONS;
    v_lcur_seq            NUMBER;
    v_lcur_err_out        NUMBER;
    v_lcur_severe_out     VARCHAR2(200);
    v_lcur_seqno_out      sorlcur.sorlcur_seqno%TYPE;
    v_lfos_err_out        NUMBER;
    v_lfos_severe_out     VARCHAR2(200);
    v_lfos_seqno_out      sorlfos.sorlfos_seqno%TYPE;
    v_old_lfos_majr       sorlfos.sorlfos_majr_code%TYPE;
    v_new_lfos_majr       sorlfos.sorlfos_majr_code%TYPE;
    v_recr_lcur_exist     VARCHAR2(1);
    v_adm_lcur_exist      VARCHAR2(1);
    v_adm_lfos_exist      VARCHAR2(1);
    no_recruit_in_sorlcur EXCEPTION;
    v_error_mesg          VARCHAR2(200);
    v_proc_name           VARCHAR2(60 CHAR) := 'p_insert_appl_cur_fos';
    v_user_id             VARCHAR2(30 CHAR) := gb_common.f_sct_user;
  BEGIN
    v_new_lfos_majr  := p_majr_code;

    --Check if recruit record is in curriculum table
    BEGIN
      SELECT DISTINCT 'Y'
      INTO v_recr_lcur_exist
      FROM sorlcur
      WHERE sorlcur_pidm = p_pidm
      AND sorlcur_term_code = p_term_code
      AND sorlcur_lmod_code = sb_curriculum_str.f_RECRUIT;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        v_recr_lcur_exist := 'N';
    END; 
    --Raise exception if Recruit record hasn't been created in curriculum table
    IF v_recr_lcur_exist = 'N'
    THEN
      RAISE no_recruit_in_sorlcur;
    END IF;
    --Move forward.. 
    
 
    
    ------------------------------------------------------------------------
    --Get max curriculum sequence for curriculumn
    -------------------------------------------------------------------------     
    BEGIN
      SELECT MAX(sorlcur_seqno)
      INTO v_lcur_seq
      FROM sorlcur
      WHERE sorlcur_pidm = p_pidm;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        v_lcur_seq := 0;
    END; 
    ------------------------------------------------------------------------
    --EXISTENCE CHECK for application record is already in curriculumn for the same term
    -------------------------------------------------------------------------
    BEGIN
      SELECT DISTINCT 'Y' 
      INTO v_adm_lcur_exist
      FROM sorlcur
      WHERE sorlcur_pidm = p_pidm 
      AND sorlcur_term_code = p_term_code
      AND sorlcur_lmod_code = v_lcur_lmod_code
      AND sorlcur_key_seqno = p_appl_no;
      -- do I need to do more check? like admit code, level code, cact code?
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        v_adm_lcur_exist := 'N';
    END;      
    ------------------------------------------------------------------------
    --1) IF NO application record  already in curriculumn for the same term
    -------------------------------------------------------------------------   
    IF v_adm_lcur_exist = 'N'
    THEN
      v_lcur_seq := v_lcur_seq + 1;
      BEGIN
        ------------------------------------------------------------------------
        --Create new row for applicant in curriculum table
        -------------------------------------------------------------------------
        baninst1.sb_curriculum.p_create(
                   p_pidm              => p_pidm,
                   p_seqno             => v_lcur_seq,
                   p_lmod_code         => v_lcur_lmod_code,
                   p_term_code         => p_term_code,
                   p_key_seqno         => p_appl_no,
                   p_priority_no       => 1,
                   p_roll_ind          => 'N',
                   p_cact_code         => 'ACTIVE',
                   p_user_id           => v_user_id,
                   p_data_origin       => p_data_origin,
                   p_levl_code         => p_levl_code,
                   p_coll_code         => 'SM',
                   p_degc_code         => p_degree_code,
                   p_camp_code         => '0',
                   p_admt_code         => p_admt_code,
                   p_term_code_ctlg    => p_term_code,
                   p_rowid_out         => v_row_id_out,
                   p_seqno_out         => v_lcur_seqno_out,
                   p_curr_error_out    => v_lcur_err_out,
                   p_severity_out      => v_lcur_severe_out
                   );
                   
        gb_common.p_commit;
      EXCEPTION
        WHEN OTHERS THEN 
          v_error_mesg := 'Unable to create curriculum record in SORLCUR. ' || SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300);
          p_handle_err(p_guid, v_error_mesg, v_proc_name);
      END;
 
      --Create New row for applicant in field of study table
      ------------------------------------------------------------------------
      IF v_lcur_seqno_out IS NOT NULL   
      THEN    
        BEGIN
          baninst1.sb_fieldofstudy.p_create(
                     p_pidm              => p_pidm,
                     p_lcur_seqno        => v_lcur_seq,
                     p_seqno             => 1,
                     p_lfst_code         => 'MAJOR',
                     p_term_code         => p_term_code,
                     p_priority_no       => 1,
                     p_csts_code         => 'INPROGRESS',
                     p_cact_code         => 'ACTIVE',
                     p_data_origin       => p_data_origin,
                     p_user_id           => v_user_id,
                     p_majr_code         => v_new_lfos_majr,
                     p_term_code_ctlg    => p_term_code,               
                     p_rowid_out         => v_row_id_out,
                     p_curr_error_out    => v_lfos_err_out,
                     p_severity_out      => v_lfos_severe_out,
                     p_lfos_seqno_out    => v_lfos_seqno_out 
              );
          gb_common.p_commit;
        EXCEPTION
          WHEN OTHERS THEN 
            v_error_mesg := 'Unable to create field of study record in SORLFOS. ' || SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300);
            p_handle_err(p_guid, v_error_mesg, v_proc_name);
        END;
      END IF;
    --2) IF THERE IS AN application record already in curriculumn for the same term
    -------------------------------------------------------------------------       
    ELSE       
      v_lcur_seq := baninst1.sb_curriculum.f_query_max_seq(p_pidm, v_lcur_lmod_code, p_term_code, p_appl_no, 1);
      --Check if SORLFOS record is already there for the same term
      BEGIN
        SELECT 'Y' , sorlfos_majr_code
        INTO v_adm_lfos_exist, v_old_lfos_majr
        FROM sorlfos
        WHERE sorlfos_pidm = p_pidm 
        AND sorlfos_term_code = p_term_code
        AND sorlfos_lcur_seqno = v_lcur_seq
        AND sorlfos_seqno = 1;
        --AND sorlfos_majr_code = v_new_lfos_majr;
      EXCEPTION
        WHEN NO_DATA_FOUND THEN
          v_adm_lfos_exist := 'N';
          v_old_lfos_majr := NULL;
      END;

      
      --If SORLFOS row for applicant already exist, straight update
      IF v_adm_lfos_exist = 'Y' THEN 
        --Only update if majr code is different
        IF v_old_lfos_majr != v_new_lfos_majr THEN
          UPDATE sorlfos 
              SET sorlfos_majr_code = v_new_lfos_majr,
                  sorlfos_activity_date = SYSDATE,
                  sorlfos_data_origin = p_data_origin,
                  sorlfos_user_id = v_user_id
            WHERE sorlfos_pidm = p_pidm 
              AND sorlfos_term_code = p_term_code
              AND sorlfos_lcur_seqno = v_lcur_seq
              AND sorlfos_seqno = 1;
          
          COMMIT;
        END IF;
   
      --If SORLFOS row for applicant doesn't exist yet for this term and lcur seqno, use API to create
      ELSE
        BEGIN
          baninst1.sb_fieldofstudy.p_create(
                       p_pidm              => p_pidm,
                       p_lcur_seqno        => v_lcur_seq,
                       p_seqno             => 1,
                       p_lfst_code         => 'MAJOR',
                       p_term_code         => p_term_code,
                       p_priority_no       => 1,
                       p_csts_code         => 'INPROGRESS',
                       p_cact_code         => 'ACTIVE',
                       p_data_origin       => p_data_origin,
                       p_user_id           => v_user_id,
                       p_majr_code         => v_new_lfos_majr,
                       p_term_code_ctlg    => p_term_code,               
                       p_rowid_out         => v_row_id_out,
                       p_curr_error_out    => v_lfos_err_out,
                       p_severity_out      => v_lfos_severe_out,
                       p_lfos_seqno_out    => v_lfos_seqno_out 
                );
          gb_common.p_commit;
        EXCEPTION
          WHEN OTHERS THEN 
            v_error_mesg := 'Unable to create field of study record in SORLFOS.' || SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300);
            p_handle_err(p_guid, v_error_mesg, v_proc_name);
        END;
      END IF;    
    END IF;
    --Backfill SARADAP from the LCUR and LFOS rows
    baninst1.soklcur.p_backload_curr(
                p_lmod => sb_curriculum_str.f_ADMISSIONS,
                p_term_code => p_term_code,
                p_keyseqno => p_appl_no,
                p_pidm => p_pidm);
    COMMIT;
  EXCEPTION
    WHEN OTHERS THEN
      v_error_mesg := SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300);
      p_handle_err(p_guid, v_error_mesg, v_proc_name);
  END p_insert_appl_cur_fos;
  ---------
  PROCEDURE  p_load_percent_4yr(
    p_pidm          sprcmnt.sprcmnt_pidm%TYPE,
    p_percent_4yr   sprcmnt.sprcmnt_text%TYPE,
    p_orig_code     sprcmnt.sprcmnt_orig_code%TYPE,
    p_data_origin   sprcmnt.sprcmnt_data_origin%TYPE,
    p_error_out   OUT  VARCHAR2)
  IS     
    v_4yr_cnt           NUMBER := 0;
    v_percent_4yr_old   sprcmnt.sprcmnt_text%TYPE := NULL;
    v_orig_code         sprcmnt.sprcmnt_orig_code%TYPE := NULL;
    v_orig_cnt          NUMBER := 0;
    v_user_id           sprcmnt.sprcmnt_user_id%TYPE := gb_common.f_sct_user;
    
  BEGIN
    SELECT COUNT(*) 
    INTO v_orig_cnt
    FROM stvorig 
    WHERE stvorig_code = p_orig_code;
    
    IF v_orig_cnt > 0
    THEN
      v_orig_code := p_orig_code;
    END IF; 
    
    SELECT COUNT(*)
      INTO v_4yr_cnt
      FROM sprcmnt
      WHERE sprcmnt_pidm = p_pidm
      AND sprcmnt_cmtt_code = 'PSC';
       
    IF v_4yr_cnt IS NOT NULL AND v_4yr_cnt = 0
    THEN
      BEGIN
        INSERT INTO sprcmnt( 
                  sprcmnt_pidm,
                  sprcmnt_cmtt_code,
                  sprcmnt_text,
                  sprcmnt_date,
                  sprcmnt_orig_code, 
                  sprcmnt_activity_date,
                  sprcmnt_contact_date,--do we leave null for this or sysdate?
                  sprcmnt_user_id,
                  sprcmnt_data_origin)
  
        VALUES (
                  p_pidm, 
                  'PSC',
                  p_percent_4yr, 
                  SYSDATE, 
                  v_orig_code,
                  SYSDATE,  
                  SYSDATE, 
                  v_user_id, 
                  p_data_origin); 
      EXCEPTION
        WHEN OTHERS
        THEN
          p_error_out := 'Error inserting 4yr college percent for Banner PIDM ' || p_pidm || '. ' || SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300);
      END;
    END IF;
          
    IF v_4yr_cnt > 0
    THEN
      SELECT sprcmnt_text
      INTO v_percent_4yr_old
      FROM sprcmnt
      WHERE sprcmnt_pidm = p_pidm
      AND sprcmnt_cmtt_code = 'PSC';
            
      IF (p_percent_4yr <> NVL(v_percent_4yr_old,'NULL_VAL'))
      THEN
        BEGIN
          UPDATE sprcmnt
          SET sprcmnt_text = p_percent_4yr,
              sprcmnt_activity_date = SYSDATE,
              sprcmnt_user_id = v_user_id
          WHERE sprcmnt_pidm = p_pidm 
          AND sprcmnt_cmtt_code = 'PSC'; 
        EXCEPTION
          WHEN OTHERS
          THEN
            p_error_out := 'Error updating 4yr college percent for Banner PIDM ' || p_pidm || '. ' || SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300);
        END;
      END IF;     
    END IF;
    
    COMMIT;
  EXCEPTION
    WHEN OTHERS THEN
      p_error_out := 'Error processing for Banner PIDM ' || p_pidm || '. ' || SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300);
      ROLLBACK;
  END p_load_percent_4yr;

  ---------
 
  PROCEDURE p_import_appl_schools
  IS
    szsappl_rec           szsappl%ROWTYPE;
    v_row_id_in           gb_common.internal_record_id_type;
    v_row_id_out          gb_common.internal_record_id_type;
    v_pidm                szsappl.szsappl_pidm%TYPE;
    v_guid                szsappl.szsappl_guid%TYPE;
    v_term_code           szsappl.szsappl_term_code_entry%TYPE;
    v_key_seqno           srbrecr.srbrecr_admin_seqno%TYPE;  
    v_school1_code        szsappl.szsappl_school1_code%TYPE;
    v_school1_type        szsappl.szsappl_school1_type%TYPE;
    v_school1_grad        DATE;
    v_school2_code        szsappl.szsappl_school2_code%TYPE;
    v_school2_type        szsappl.szsappl_school2_type%TYPE;
    v_school2_grad        DATE;
    v_gpa                 szsappl.szsappl_gpa%TYPE;
    v_rank                NUMBER;--szsappl_class_rank is varchar2
    v_class_size          szsappl.szsappl_class_size%TYPE; 
    v_percentile          sorhsch.sorhsch_percentile%TYPE;
    v_sorhsch_sbgi        sorhsch.sorhsch_sbgi_code%TYPE;
    v_sorhsch_grad        sorhsch.sorhsch_graduation_date%TYPE;
    v_sorhsch_exist       VARCHAR2(1);    
    v_sorpcol_sbgi        sorpcol.sorpcol_sbgi_code%TYPE;
    v_sorpcol_exist       VARCHAR2(1); 
    v_4yr_cnt             szsappl.szsappl_col_percent%TYPE;
    v_percent_4yr         sprcmnt.sprcmnt_text%TYPE;
    v_percent_4yr_old     sprcmnt.sprcmnt_text%TYPE;
    v_sordegr_seq_out     sordegr.sordegr_degr_seq_no%TYPE;
    v_sordegr_exist       VARCHAR2(1);
    v_error_mesg          VARCHAR2(200);
    v_proc_name           VARCHAR2(60 CHAR) := 'p_import_appl_schools';
    v_user_id             VARCHAR2(30 CHAR) := gb_common.f_sct_user;
    v_data_origin         VARCHAR2(30 CHAR) := 'SLATE';
    no_srbrecr_row        EXCEPTION;
    invalid_sbgi_code     EXCEPTION;
    --Records with PIDM created in system
    CURSOR szsappl_c 
    IS
      SELECT * FROM szsappl
      WHERE szsappl_guid IN 
          (SELECT goradid_additional_id 
             FROM goradid 
            WHERE goradid_adid_code = 'SL')
      AND szsappl_guid NOT IN (SELECT goradid_additional_id 
                                FROM goradid 
                               WHERE goradid_adid_code = 'SL'
                               GROUP BY goradid_additional_id HAVING COUNT(*) >1)
      AND NVL(szsappl_pidm, '00000000') NOT IN (SELECT goradid_pidm 
                                FROM goradid 
                               WHERE goradid_adid_code = 'SL'
                               GROUP BY goradid_pidm HAVING COUNT(*) >1); 
        
  
  BEGIN    
    IF szsappl_c%ISOPEN
    THEN
      CLOSE szsappl_c;
    END IF;
    OPEN szsappl_c;
    LOOP
      FETCH szsappl_c INTO szsappl_rec;
      EXIT WHEN szsappl_c%NOTFOUND;
      v_pidm := f_get_pidm_from_guid(szsappl_rec.szsappl_guid, 'SL');
      v_guid  := szsappl_rec.szsappl_guid;
      v_term_code := szsappl_rec.szsappl_term_code_entry;
      v_key_seqno := baninst1.sb_recruit.F_GetSrbrecrSeqno(v_pidm,  v_term_code, 'C');
      BEGIN
        IF v_key_seqno = 0
        THEN
          RAISE no_srbrecr_row; --no matching recruit record in SRBRECR
        END IF;  
        v_school1_code := szsappl_rec.szsappl_school1_code;
        v_school1_type := szsappl_rec.szsappl_school1_type;
        v_school1_grad := TO_DATE(szsappl_rec.szsappl_school1_grad, 'YYYY-MM-DD');
        v_school2_code := szsappl_rec.szsappl_school2_code;
        v_school2_type := szsappl_rec.szsappl_school2_type;
        v_school2_grad := TO_DATE(szsappl_rec.szsappl_school2_grad, 'YYYY-MM-DD');
        v_gpa          := szsappl_rec.szsappl_gpa;
        BEGIN 
          v_rank         := to_number(szsappl_rec.szsappl_class_rank);
        EXCEPTION
          WHEN VALUE_ERROR THEN  
            v_error_mesg := 'Class rank ''' || szsappl_rec.szsappl_class_rank || ''' is not a valid number. Rank is now set as NULL.';
            p_handle_err(v_guid, v_error_mesg, v_proc_name);
            v_rank := NULL;
        END;
        v_class_size   := to_number(szsappl_rec.szsappl_class_size);
        v_percent_4yr  := szsappl_rec.szsappl_col_percent;
        IF v_rank IS NULL 
        THEN
          v_class_size := NULL;
        END IF;
       
        IF v_rank IS NOT NULL and v_class_size IS NOT NULL
        THEN
          BEGIN
            v_percentile  := ROUND((v_class_size - v_rank)/v_class_size*100);
          EXCEPTION
            WHEN ZERO_DIVIDE THEN
              v_error_mesg := 'Class size should not be 0. Cannot calculate percentile due to division by 0 error. Percentile is now set as NULL.';
              p_handle_err(v_guid, v_error_mesg, v_proc_name);
              v_percentile := NULL;
          END;
        ELSE 
          v_percentile := NULL;
        END IF;
        v_sorhsch_sbgi := NULL;
        v_sorhsch_grad := NULL;  
        v_sorpcol_sbgi := NULL; 
        
        --If grad date not there but school exist, 
        --take termcode year for year and concat 6/1 in front
        IF v_school1_type = 'High School' AND v_school1_grad IS NULL
        THEN
          v_school1_grad := TO_DATE('06/01/' || SUBSTR(v_term_code, 3, 2), 'MM/DD/RRRR'); 
        END IF;
        IF v_school2_type = 'High School' AND v_school2_grad IS NULL
        THEN
          v_school2_grad := TO_DATE('06/01/' || SUBSTR(v_term_code, 3, 2), 'MM/DD/RRRR'); 
        END IF;
        --Decide which school to insert where
        ------------------------------------------------------------------------
        --1) Case when only 1 school exist 
        ------------------------------------------------------------------------
        IF v_school1_type = 'High School' AND v_school2_type IS NULL
        THEN
          v_sorhsch_sbgi := v_school1_code;
          v_sorhsch_grad := v_school1_grad;
        
        ELSIF v_school1_type = 'College' AND v_school2_type IS NULL
        THEN
          v_sorpcol_sbgi := v_school1_code; 
        
        ELSIF v_school1_type IS NULL AND v_school2_type = 'High School'
        THEN
          v_sorhsch_sbgi := v_school2_code;
          v_sorhsch_grad := v_school2_grad;
        
        ELSIF v_school1_type IS NULL AND v_school2_type = 'College'
        THEN
          v_sorpcol_sbgi := v_school2_code;
        ------------------------------------------------------------------------  
        --2) Case when both exist but thankfully different
        ------------------------------------------------------------------------
        ELSIF v_school1_type = 'High School' AND v_school2_type = 'College'
        THEN
          v_sorhsch_sbgi := v_school1_code;
          v_sorhsch_grad := v_school1_grad;
          v_sorpcol_sbgi := v_school2_code;  
       
        ELSIF v_school1_type = 'College' AND v_school2_type = 'High School'
        THEN
          v_sorhsch_sbgi := v_school2_code;
          v_sorhsch_grad := v_school2_grad;
          v_sorpcol_sbgi := v_school1_code;  
        ------------------------------------------------------------------------  
        --3) Case when both exist but the same type :(
        ------------------------------------------------------------------------
  
        ELSIF v_school1_type = 'High School' AND v_school2_type = 'High School'
        THEN
          --if school 2 has most recent grad, then use school 2
          IF v_school1_grad < v_school2_grad
          THEN
            v_sorhsch_sbgi := v_school2_code;
            v_sorhsch_grad := v_school2_grad;
          --if school 1 has most recent grad, OR both has same dates, then use school 1
          ELSE  
            v_sorhsch_sbgi := v_school1_code;
            v_sorhsch_grad := v_school1_grad;
          END IF;
          
        ELSIF v_school1_type = 'College' AND v_school2_type = 'College'
        THEN
          --if school 2 has most recent grad, then use school 2
          IF v_school1_grad < v_school2_grad
          THEN
            v_sorpcol_sbgi := v_school2_code;
          --if school 1 has most recent grad, OR both has same dates, then use school 1
          ELSE  
            v_sorpcol_sbgi := v_school1_code;
          END IF;
        END IF;       
        ------------------------------------------------------------------------  
        --INSERT/UPDATE SPRCMNT
        v_error_mesg := NULL;
        IF v_percent_4yr IS NOT NULL
        THEN
           p_load_percent_4yr(v_pidm, v_percent_4yr, 'ADM', 'SLATE', v_error_mesg);
        END IF;
        IF v_error_mesg IS NOT NULL 
        THEN
          p_handle_err(v_guid, v_error_mesg, 'p_load_percent_4yr');
        END IF;
      
        ------------------------------------------------------------------------  
        --INSERT/UPDATE SORHSCH
        ------------------------------------------------------------------------
        IF v_sorhsch_sbgi IS NOT NULL AND f_check_sbgi(v_sorhsch_sbgi) = '000000'
        THEN
          v_error_mesg := 'Skipped processing high school due to SBGI code ' || v_sorhsch_sbgi || ' being invalid.' ;
          p_handle_err(v_guid, v_error_mesg, v_proc_name);
        
        ELSIF v_sorhsch_sbgi IS NOT NULL AND f_check_sbgi(v_sorhsch_sbgi) <> '000000' 
        THEN
          v_row_id_in := NULL;
          v_sorhsch_exist := NULL;     
          v_error_mesg := NULL;
          BEGIN
            SELECT 'Y' , rowid
            INTO v_sorhsch_exist, v_row_id_in 
            FROM sorhsch
            WHERE sorhsch_pidm = v_pidm
            AND sorhsch_sbgi_code = v_sorhsch_sbgi;
            
          EXCEPTION
            WHEN NO_DATA_FOUND THEN
              v_sorhsch_exist := 'N';
          END;
          ---If H.S already exist, update, else create
          IF v_sorhsch_exist = 'Y'
          THEN
            BEGIN     
              baninst1.sb_highschool.p_update(
                p_pidm            => v_pidm,
                p_sbgi_code       => v_sorhsch_sbgi,
                p_graduation_date => v_sorhsch_grad,
                p_gpa             => v_gpa,
                p_class_rank      => v_rank,
                p_class_size      => v_class_size,
                p_percentile      => v_percentile,
                p_user_id         => v_user_id,
                p_data_origin     => v_data_origin,
                p_rowid           => v_row_id_in);
              gb_common.p_commit;
              
            EXCEPTION
              WHEN OTHERS
              THEN
                v_error_mesg := 'Unable to update highschool record in SORHSCH. '|| SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300);
                p_handle_err(v_guid, v_error_mesg, v_proc_name);
            END;
          ELSE
        
            BEGIN  
              baninst1.sb_highschool.p_create(
                p_pidm            => v_pidm,
                p_sbgi_code       => v_sorhsch_sbgi,
                p_graduation_date => v_sorhsch_grad,
                p_gpa             => v_gpa,
                p_class_rank      => v_rank,
                p_class_size      => v_class_size,
                p_percentile      => v_percentile,
                p_user_id         => v_user_id,
                p_data_origin     => v_data_origin,
                p_rowid_out       => v_row_id_out);
              gb_common.p_commit;

            EXCEPTION            
              WHEN OTHERS
              THEN
                v_error_mesg := 'Unable to create highschool record in SORHSCH. '|| SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300);
                p_handle_err(v_guid, v_error_mesg, v_proc_name);
            END;      
          END IF;
          
        END IF;
        ------------------------------------------------------------------------  
        --INSERT/UPDATE SORPCOL
        --College CEEB from Slate needs prepended with 'CC' to be valid in Banner
        ------------------------------------------------------------------------    
        IF v_sorpcol_sbgi IS NOT NULL AND f_check_sbgi('CC' || v_sorpcol_sbgi) = '000000'
        THEN
          v_sorpcol_sbgi := 'CC' || v_sorpcol_sbgi;
          v_error_mesg := 'Skipped processing college due to SBGI code ' || v_sorpcol_sbgi || ' being invalid.' ;
          p_handle_err(v_guid, v_error_mesg, v_proc_name);
        ELSIF v_sorpcol_sbgi IS NOT NULL AND f_check_sbgi('CC' || v_sorpcol_sbgi) <> '000000' 
        THEN
          v_sorpcol_sbgi := 'CC' || v_sorpcol_sbgi;
          v_row_id_in := NULL;
          v_sorpcol_exist := NULL; 
          v_error_mesg := NULL;
          BEGIN
            SELECT 'Y' , rowid
            INTO v_sorpcol_exist, v_row_id_in 
            FROM sorpcol
            WHERE sorpcol_pidm = v_pidm
            AND sorpcol_sbgi_code = v_sorpcol_sbgi;    
          EXCEPTION
            WHEN NO_DATA_FOUND THEN
              v_sorpcol_exist := 'N';
          END;

          IF v_sorpcol_exist = 'N'
          THEN
            SAVEPOINT create_col;
            BEGIN
              baninst1.gb_prior_college.p_create(
                p_pidm            => v_pidm,
                p_sbgi_code       => v_sorpcol_sbgi,
                p_trans_recv_date => NULL,
                p_trans_rev_date  => NULL,
                p_official_trans  => NULL,
                p_admr_code       => NULL,         
                p_user_id         => v_user_id,
                p_data_origin     => v_data_origin,
                p_rowid_out       => v_row_id_out);
                           
            EXCEPTION
              WHEN OTHERS
              THEN
                v_error_mesg := 'Unable to create college record in SORPCOL. ' || SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300);
                p_handle_err(v_guid, v_error_mesg, v_proc_name);
            END; 

            BEGIN
              baninst1.gb_pcol_degree.p_create(
                   p_pidm              => v_pidm,
                   p_sbgi_code         => v_sorpcol_sbgi,
                   p_degc_code         => 'AB',
                   p_degr_seq_no_inout => v_sordegr_seq_out,
                   p_data_origin       => v_data_origin,
                   p_user_id           => v_user_id,
                   p_rowid_out         => v_row_id_out);
              --gb_common.p_commit;
            EXCEPTION
              WHEN OTHERS THEN
                ROLLBACK TO SAVEPOINT create_col;
                v_error_mesg := 'Unable to create degree record in SORDEGR. ' || SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300);
                p_handle_err(v_guid, v_error_mesg, v_proc_name);
            END;
            gb_common.p_commit;
          END IF;
          IF v_sorpcol_exist = 'Y'
          THEN
            --EXISTENCE CHECK IN SORDEGR
            BEGIN
              SELECT DISTINCT 'Y' 
              INTO v_sordegr_exist 
              FROM sordegr
              WHERE sordegr_pidm = v_pidm
              AND sordegr_sbgi_code = v_sorpcol_sbgi
              AND sordegr_degc_code = 'AB';
            
            EXCEPTION
              WHEN NO_DATA_FOUND THEN
                v_sordegr_exist := 'N';
            END;
            IF v_sordegr_exist = 'N'
            THEN
              BEGIN
                baninst1.gb_pcol_degree.p_create(
                     p_pidm              => v_pidm,
                     p_sbgi_code         => v_sorpcol_sbgi,
                     p_degc_code         => 'AB',
                     p_degr_seq_no_inout => v_sordegr_seq_out,
                     p_data_origin       => v_data_origin,
                     p_user_id           => v_user_id,
                     p_rowid_out         => v_row_id_out);
                  gb_common.p_commit;
              EXCEPTION
                WHEN OTHERS THEN
                  v_error_mesg := 'Unable to create degree record in SORDEGR for Banner pidm '|| v_pidm || '. ' || SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300);
                  p_handle_err(v_guid, v_error_mesg, v_proc_name);
      
              END;
            END IF;
          END IF;
        END IF;
      EXCEPTION
        WHEN no_srbrecr_row THEN --record the Oracle error in the track error table
          v_error_mesg :=  'Unable to start processing school data for Banner pidm '|| v_pidm || ' because SRBRECR record for associated term ' || v_term_code || ' is not found.' ; 
          p_handle_err(v_guid, v_error_mesg, v_proc_name);
        WHEN OTHERS THEN
          v_error_mesg :=  'Error processing school data for Banner pidm '|| v_pidm|| '. ' || SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300); 
          p_handle_err(v_guid, v_error_mesg, v_proc_name);
      END;
    END LOOP; 
    CLOSE szsappl_c;
  END p_import_appl_schools;
  ------
  PROCEDURE p_insert_srtprel(
    p_guid           VARCHAR2,
    p_ridm           srtprel.srtprel_ridm%TYPE,
    p_prel_code      srtprel.srtprel_prel_code%TYPE,
    p_term_code      srtprel.srtprel_term_code%TYPE,
    p_rtyp_code      srtprel.srtprel_rtyp_code%TYPE,
    p_styp_code      srtprel.srtprel_styp_code%TYPE,
    p_data_origin    srtprel.srtprel_data_origin%TYPE,
    p_err_out   OUT VARCHAR2)
  IS
  BEGIN
    SAVEPOINT pre_srtprel;
    INSERT INTO saturn.srtprel
                    (srtprel_ridm, 
                    srtprel_prel_code, 
                    srtprel_user,
                    srtprel_add_date, 
                    srtprel_term_code, 
                    srtprel_admin_seqno,
                    srtprel_levl_code,
                    srtprel_dept_code,
                    srtprel_degc_code, 
                    srtprel_majr_code,
                    srtprel_camp_code,
                    srtprel_coll_code,
                    srtprel_rtyp_code,
                    srtprel_styp_code,
                    srtprel_data_origin,
                    srtprel_activity_date,
                    srtprel_user_id)                          
    VALUES(
                    p_ridm, 
                    p_prel_code,
                    gb_common.f_sct_user, 
                    SYSDATE,
                    p_term_code,
                    1,
                    'UG',
                    NULL,
                    'AB', 
                    '0000', 
                    '0',
                    'SM',
                    p_rtyp_code,
                    p_styp_code,
                    p_data_origin,
                    SYSDATE,
                    gb_common.f_sct_user);
  EXCEPTION
    WHEN OTHERS THEN
      ROLLBACK TO pre_srtprel;
      p_err_out :=  'Failed to load curriculum/field study for record.' || SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300); 
  END p_insert_srtprel;
  -------
  
  PROCEDURE p_insert_srthsch(
    p_guid              VARCHAR2,
    p_ridm              srthsch.srthsch_ridm%TYPE,
    p_school_code       VARCHAR2,
    p_graduation_date   DATE,
    p_term_code         VARCHAR2,
    p_data_origin       VARCHAR2,
    p_err_out   OUT     VARCHAR2)
  IS
    v_graduation_date   DATE;
    v_sbgi_code         srthsch.srthsch_sbgi_code%TYPE;
  BEGIN
    --Check sgbi code-- 
    v_sbgi_code := f_check_sbgi(p_school_code);
    IF v_sbgi_code <> '000000' --only insert if is not null or unknown school
    THEN
      IF p_graduation_date IS NULL
      THEN
        v_graduation_date := TO_DATE('06/01/' || SUBSTR(p_term_code, 3, 2), 'MM/DD/RRRR'); 
      ELSE
        v_graduation_date := p_graduation_date;
      END IF;
      BEGIN
        SAVEPOINT pre_srthsch;
        INSERT INTO saturn.srthsch
            (srthsch_ridm,
             srthsch_sbgi_code,
             srthsch_activity_date,
             srthsch_graduation_date,
             srthsch_user_id,
             srthsch_data_origin)    
        VALUES (
             p_ridm, 
             v_sbgi_code,
             sysdate,
             v_graduation_date,
             gb_common.f_sct_user,
             p_data_origin);

      EXCEPTION
        WHEN OTHERS THEN
          ROLLBACK TO pre_srthsch;
          p_err_out := 'Failed to load high school info for record. ' || SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300); 
      END;              
    END IF;
  END p_insert_srthsch;
  ---------
  PROCEDURE p_insert_srtpcol(
    p_guid              VARCHAR2,
    p_ridm              srtpcol.srtpcol_ridm%TYPE,
    p_school_code       VARCHAR2,
    p_graduation_date   DATE,
    p_term_code         VARCHAR2,
    p_data_origin       VARCHAR2,
    p_err_out   OUT     VARCHAR2)
  IS
    v_graduation_date   DATE;
    v_sbgi_code         srtpcol.srtpcol_sbgi_code%TYPE;
  BEGIN
    /** Check sgbi code **/  
    v_sbgi_code := f_check_sbgi(p_school_code);
    IF v_sbgi_code <> '000000' --is not null or unknown school
    THEN
      IF p_graduation_date IS NULL
      THEN
        v_graduation_date := TO_DATE('06/01/' || SUBSTR(p_term_code, 3, 2), 'MM/DD/RRRR'); 
      ELSE
        v_graduation_date := p_graduation_date;
      END IF;
      BEGIN
        SAVEPOINT pre_srtpcol;
        INSERT INTO saturn.srtpcol
            (srtpcol_ridm,
             srtpcol_sbgi_code,
             srtpcol_activity_date,
             srtpcol_graduation_date,
             srtpcol_user_id,
             srtpcol_data_origin)   
        VALUES (
             p_ridm, 
             v_sbgi_code,
             sysdate,
             v_graduation_date,
             gb_common.f_sct_user,
             p_data_origin);
      EXCEPTION
        WHEN OTHERS THEN
          ROLLBACK TO pre_srtpcol;
          p_err_out :=  'Failed to load prior college info for record.' || SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300); 
      END;              
    END IF;
  END p_insert_srtpcol;  
  --------
  PROCEDURE p_insert_srtaddr ( 
    p_guid      VARCHAR2,
    p_ridm      srtaddr.srtaddr_ridm%TYPE,
    p_street1   VARCHAR2,
    p_street2   VARCHAR2,
    p_street3   VARCHAR2,
    p_city      VARCHAR2,
    p_region    VARCHAR2,
    p_postal    VARCHAR2,
    p_natn_code stvnatn.stvnatn_code%TYPE,
    p_atyp_code stvatyp.stvatyp_code%TYPE,
    p_from_date  DATE,
    p_to_date    DATE,
    p_data_origin VARCHAR2,
    p_err_out   OUT VARCHAR2)
    
  IS  
    v_spraddr_street1      spraddr.spraddr_street_line1%TYPE := NULL;
    v_spraddr_street2      spraddr.spraddr_street_line2%TYPE := NULL;
    v_spraddr_street3      spraddr.spraddr_street_line3%TYPE := NULL;
    v_spraddr_street4     spraddr.spraddr_street_line4%TYPE := NULL;
    v_spraddr_region      spraddr.spraddr_stat_code%TYPE := NULL;
    v_spraddr_city        spraddr.spraddr_city%TYPE := NULL;
    v_spraddr_postal      spraddr.spraddr_zip%TYPE := NULL;
    v_spraddr_natn        spraddr.spraddr_natn_code%TYPE := NULL;
  BEGIN
   --if street line 1 overflow, split text in half and move second part to street line 2
   IF LENGTH(p_street1) > 75 
    THEN
      IF p_street2 IS NOT NULL
      THEN
        v_spraddr_street3 := p_street2 || ' ' || p_street3; -- if street line 2 is not null push the text down to concatenate before line 3
      ELSE
        v_spraddr_street3 := p_street3;
      END IF;
      v_spraddr_street1 := SUBSTR(p_street1, 1, f_string_midpoint(p_street1));
      v_spraddr_street2 := SUBSTR(p_street1, f_string_midpoint(p_street1)+1);

    ELSE
      --keep same (street line 4 is never used for domestic)
      v_spraddr_street1 := p_street1;
      v_spraddr_street2 := p_street2;
      v_spraddr_street3 := p_street3;
    END IF;
  
    --Format Nation Code, Region, and Postal Code   
    IF p_natn_code = 'US' OR p_natn_code IS NULL   
    THEN       
      --Leave postal, region, city the way they are but do not load SPRADDR_NATN_CODE IF = US or null
      v_spraddr_postal := p_postal;
      v_spraddr_region := p_region;
      v_spraddr_city := p_city;
      v_spraddr_natn := NULL; 
    ELSE
      v_spraddr_natn := p_natn_code;
      IF length(p_city || ' ' || p_region || ' ' || p_postal) >  50 -- if exceed City line max char limit
      THEN
          --Concatenate region and postal codes into City and put city into Street Line 4
          v_spraddr_city := p_region || ' ' || p_postal;
          v_spraddr_street4 := p_city;
      ELSE
          --Concatenate city, region, and postal into City, leave Street Line 4 blank
          v_spraddr_city := p_city || ' ' || p_region || ' ' || p_postal;
      END IF;    
    END IF;

    BEGIN
      SAVEPOINT pre_srtaddr;
      INSERT INTO saturn.srtaddr
      (
        srtaddr_ridm,
        srtaddr_atyp_code,
        srtaddr_seqno,
        srtaddr_street_line1,
        srtaddr_street_line2,
        srtaddr_street_line3,
        srtaddr_street_line4,
        srtaddr_city,
        srtaddr_stat_code,
        srtaddr_zip,
        srtaddr_natn_code,
        srtaddr_from_date,
        srtaddr_to_date,
        srtaddr_activity_date,
        srtaddr_data_origin,
        srtaddr_user_id 
      )
      VALUES
      (
        p_ridm, 
        p_atyp_code,
        '1',
        v_spraddr_street1,
        v_spraddr_street2,
        v_spraddr_street3,
        v_spraddr_street4,
        v_spraddr_city,
        v_spraddr_region,
        v_spraddr_postal,
        v_spraddr_natn,
        p_from_date,
        p_to_date,
        SYSDATE,
        p_data_origin,
        gb_common.f_sct_user
      );
        
    EXCEPTION
      WHEN OTHERS THEN
        ROLLBACK TO pre_srtaddr;
        p_err_out := 'Failed to load address info for record. ' ||  SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300); 
    END;       
  END p_insert_srtaddr;
  --------
  PROCEDURE p_insert_srttele(
    p_guid          VARCHAR2,
    p_ridm          srttele.srttele_ridm%TYPE,
    p_phone_number  VARCHAR2,
    p_tele_code     stvtele.stvtele_code%TYPE,
    p_data_origin   VARCHAR2,
    p_err_out   OUT VARCHAR2)
  IS
    v_phone_area      VARCHAR2(20) := null;
    v_phone_number    VARCHAR2(20) := null;
    v_phone_ctry_code VARCHAR2(20) := null;
  BEGIN
    v_phone_number := REPLACE(p_phone_number, '-', ' ');

    -- Country Code --
    v_phone_ctry_code := SUBSTR(v_phone_number, 1, instr(v_phone_number,' ')-1) ;
    
    -- Area Code and Number --
    v_phone_number := SUBSTR(v_phone_number, instr(v_phone_number,' ')+1);

    --If there is space separating area code from the rest of the phone digits
    --take digits before the space as area code
    IF instr(v_phone_number,' ') <> 0 
    THEN
      v_phone_area := SUBSTR(v_phone_number, 1, instr(v_phone_number,' ')-1) ;
      v_phone_number := REPLACE(SUBSTR(v_phone_number, instr(v_phone_number,' ')), ' ' , '');
    --Otherwise take first three digits as area code and the rest is phone number
    ELSE
      v_phone_area := SUBSTR(v_phone_number,1, 3) ;
      v_phone_number := SUBSTR(v_phone_number, 4) ;
    END IF;   
    
    BEGIN
      SAVEPOINT pre_srttele;
      INSERT INTO saturn.srttele(
          srttele_ridm,
          srttele_seqno,
          srttele_tele_code,
          srttele_activity_date,
          srttele_phone_area,
          srttele_phone_number,
          srttele_ctry_code_phone,
          srttele_user_id,
          srttele_data_origin)
      VALUES(
          p_ridm,
          '1',
          p_tele_code,
          SYSDATE,
          v_phone_area,
          v_phone_number,
          v_phone_ctry_code,
          gb_common.f_sct_user,
          p_data_origin
      );
    EXCEPTION
      WHEN OTHERS THEN
        ROLLBACK TO srttele;
        p_err_out := 'Failed to load phone info for record. ' || SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300); 
    END;   
  END p_insert_srttele;
  --------
  PROCEDURE p_insert_srtemal(
    p_guid          VARCHAR2,
    p_ridm          srtemal.srtemal_ridm%TYPE,
    p_email_address srtemal.srtemal_email_address%TYPE,
    p_emal_code     srtemal.srtemal_emal_code%TYPE,
    p_data_origin   VARCHAR2,
    p_err_out   OUT VARCHAR2)
  IS
  BEGIN
    SAVEPOINT pre_srtemal;
    INSERT INTO saturn.srtemal(
                SRTEMAL_RIDM,   
                SRTEMAL_STATUS_IND,
                SRTEMAL_PREFERRED_IND,
                SRTEMAL_DISP_WEB_IND,
                SRTEMAL_EMAL_CODE,   
                SRTEMAL_EMAIL_ADDRESS,
                SRTEMAL_ACTIVITY_DATE,          
                SRTEMAL_USER_ID,
                SRTEMAL_VERSION,        
                SRTEMAL_DATA_ORIGIN)
    VALUES(     p_ridm, 
                'A',
                'Y',
                'Y',
                p_emal_code,
                p_email_address,
                SYSDATE,
                gb_common.f_sct_user,
                1,
                p_data_origin);
 
  EXCEPTION
    WHEN OTHERS THEN
      ROLLBACK TO pre_srtemal;
      p_err_out :=  'Failed to load email info for record. ' || SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300); 
  END p_insert_srtemal;
  --------
  PROCEDURE p_handle_err(
      p_guid          sdlperr.sdlperr_guid%TYPE,
      p_err_mesg      sdlperr.sdlperr_error%TYPE,
      p_data_origin   sdlperr.sdlperr_data_origin%TYPE)    
  IS
  BEGIN
    INSERT INTO sdlperr(
        sdlperr_guid,
        sdlperr_error,
        sdlperr_activity_date,
        sdlperr_data_origin)
    VALUES(
        p_guid,
        p_err_mesg,
        SYSDATE,
        p_data_origin); 
END p_handle_err;
------
PROCEDURE p_srrsrin_in_gjbprun(
  p_prospect_code  IN    gjbprun.gjbprun_value%TYPE,
  p_outfile_name   IN    VARCHAR2)
  AS
    v_banner_job    VARCHAR2(100) := 'SRRSRIN';
    v_one_up_no     gjbprun.gjbprun_one_up_no%TYPE;
    --outfile_loc   VARCHAR2 (40)       :='BAN_GUAUPLP_STU_DIR'; -- /u03/app/upl/stu -- Works in PPOD, not devx
    outfile_loc     VARCHAR2 (40)       :='BANJOBS_DIR'; -- /u03/app/banjobs/cronfiles -- Works in DEVX not ppod
    output_file     UTL_FILE.file_type;
    v_error_mesg    VARCHAR2(200) := null;   
    v_proc_name     VARCHAR2(60 CHAR) := 'p_srrsrin_in_gjbprun';
  BEGIN
    BEGIN
      SELECT general.gjbpseq.nextval 
        INTO v_one_up_no
        FROM DUAL;

      ---refresh the table
      DELETE general.gjbprun
      WHERE gjbprun_job = v_banner_job;
      COMMIT;
        
      INSERT INTO general.gjbprun
      (
          gjbprun_job,
          gjbprun_one_up_no,
          gjbprun_number,
          gjbprun_activity_date,
          gjbprun_value
      )
      VALUES
      (
          v_banner_job,
          v_one_up_no,
          '01',
          SYSDATE,
          p_prospect_code
      );
          
      INSERT INTO general.gjbprun
      (
          gjbprun_job,
          gjbprun_one_up_no,
          gjbprun_number,
          gjbprun_activity_date,
          gjbprun_value
      )
      VALUES
      (
          v_banner_job,
          v_one_up_no,
          '02',
          SYSDATE,
          NULL
      );
          
      INSERT INTO general.gjbprun
      (
          gjbprun_job,
          gjbprun_one_up_no,
          gjbprun_number,
          gjbprun_activity_date,
          gjbprun_value
      )
      VALUES
      (
          v_banner_job,
          v_one_up_no,
          '03',
          SYSDATE,
          'A'
      );
          
      INSERT INTO general.gjbprun
      (
          gjbprun_job,
          gjbprun_one_up_no,
          gjbprun_number,
          gjbprun_activity_date,
          gjbprun_value
      )
      VALUES
      (
          v_banner_job,
          v_one_up_no,
          '04',
          SYSDATE,
          'N'
      );
      
      INSERT INTO general.gjbprun
      (
          gjbprun_job,
          gjbprun_one_up_no,
          gjbprun_number,
          gjbprun_activity_date,
          gjbprun_value
      )
      VALUES
      (
          v_banner_job,
          v_one_up_no,
          '05',
          SYSDATE,
          NULL
      );
                   

      INSERT INTO general.gjbprun
      (
          gjbprun_job,
          gjbprun_one_up_no,
          gjbprun_number,
          gjbprun_activity_date,
          gjbprun_value
      )
      VALUES
      (
          v_banner_job,
          v_one_up_no,
          '99',
          SYSDATE,
          '55'
      );
            
      COMMIT; 
         
      output_file :=
      UTL_FILE.fopen (outfile_loc,
                            p_outfile_name, 
                           'w'
                          );
  
      
          
      UTL_FILE.put_line (output_file, v_one_up_no);
      UTL_FILE.fclose_all;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        v_error_mesg :=  'Unable to run SRRPREL because of one-up number.' || SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300); 
        p_handle_err('000000000', v_error_mesg, v_proc_name);
    END;   
END p_srrsrin_in_gjbprun;



PROCEDURE p_srrprel_in_gjbprun(
  p_prospect_code  IN    gjbprun.gjbprun_value%TYPE,
  p_match_status   IN     gjbprun.gjbprun_value%TYPE,
  p_outfile_name   IN    VARCHAR2)
  AS
    v_banner_job VARCHAR2(100) := 'SRRPREL';
    v_one_up_no            gjbprun.gjbprun_one_up_no%TYPE;
    --outfile_loc            VARCHAR2 (40)       :='BAN_GUAUPLP_STU_DIR'; -- /u03/app/upl/stu -- Works in PPOD, not devx
    outfile_loc            VARCHAR2 (40)       :='BANJOBS_DIR'; -- /u03/app/banjobs/cronfiles -- Works in DEVX not ppod
    output_file            UTL_FILE.file_type;
    v_error_mesg    VARCHAR2(200) := null;   
    v_proc_name     VARCHAR2(60 CHAR) := 'p_srrprel_in_gjbprun';

  BEGIN
    BEGIN
      SELECT general.gjbpseq.nextval 
        INTO v_one_up_no
        FROM DUAL;

      ---refresh the table
      DELETE general.gjbprun
      WHERE gjbprun_job = v_banner_job;
      COMMIT;
        
      INSERT INTO general.gjbprun
      (
          gjbprun_job,
          gjbprun_one_up_no,
          gjbprun_number,
          gjbprun_activity_date,
          gjbprun_value
      )
      VALUES
      (
          v_banner_job,
          v_one_up_no,
          '01',
          SYSDATE,
          p_prospect_code
      );
          
      INSERT INTO general.gjbprun
      (
          gjbprun_job,
          gjbprun_one_up_no,
          gjbprun_number,
          gjbprun_activity_date,
          gjbprun_value
      )
      VALUES
      (
          v_banner_job,
          v_one_up_no,
          '02',
          SYSDATE,
          NULL
      );
          
      INSERT INTO general.gjbprun
      (
          gjbprun_job,
          gjbprun_one_up_no,
          gjbprun_number,
          gjbprun_activity_date,
          gjbprun_value
      )
      VALUES
      (
          v_banner_job,
          v_one_up_no,
          '03',
          SYSDATE,
          p_match_status
      );
          
      INSERT INTO general.gjbprun
      (
          gjbprun_job,
          gjbprun_one_up_no,
          gjbprun_number,
          gjbprun_activity_date,
          gjbprun_value
      )
      VALUES
      (
          v_banner_job,
          v_one_up_no,
          '04',
          SYSDATE,
          NULL
      );
          

      INSERT INTO general.gjbprun
      (
          gjbprun_job,
          gjbprun_one_up_no,
          gjbprun_number,
          gjbprun_activity_date,
          gjbprun_value
      )
      VALUES
      (
          v_banner_job,
          v_one_up_no,
          '99',
          SYSDATE,
          '55'
      );
            
      COMMIT; 
         
      output_file :=
      UTL_FILE.fopen (outfile_loc,
                            p_outfile_name, 
                           'w'
                          );
  
      
          
      UTL_FILE.put_line (output_file, v_one_up_no);
      UTL_FILE.fclose_all;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        v_error_mesg :=  'Unable to run SRRPREL because of one-up number.' || SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300); 
        p_handle_err('000000000', v_error_mesg, v_proc_name);
    END;   
  END p_srrprel_in_gjbprun;
  ---------
  PROCEDURE p_srtpurg_in_gjbprun(
    p_prospect_code  IN    gjbprun.gjbprun_value%TYPE,
    p_match_status   IN    gjbprun.gjbprun_value%TYPE,
    p_load_status    IN    gjbprun.gjbprun_value%TYPE,
    p_audit_or_update IN   gjbprun.gjbprun_value%TYPE,
    p_outfile_name   IN    VARCHAR2)
  IS  
    v_banner_job    VARCHAR2(100) := 'SRTPURG';
    v_one_up_no     gjbprun.gjbprun_one_up_no%TYPE;
    no_one_up       EXCEPTION;
    outfile_loc     VARCHAR2 (40)       :='BANJOBS_DIR';
    output_file     UTL_FILE.file_type;
    v_error_mesg    VARCHAR2(200) := null;   
    v_proc_name     VARCHAR2(60 CHAR) := 'p_srtpurg_in_gjbprun';

  BEGIN
    BEGIN
      SELECT general.gjbpseq.nextval 
        INTO v_one_up_no
        FROM DUAL;
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        RAISE no_one_up;
    END;
    ---refresh the table
    DELETE general.gjbprun
    WHERE gjbprun_job = v_banner_job;
    COMMIT;
    SAVEPOINT pre_gjbprun_insert;    
    INSERT INTO general.gjbprun(
          gjbprun_job,
          gjbprun_one_up_no,
          gjbprun_number,
          gjbprun_activity_date,
          gjbprun_value
    )
    VALUES(
          v_banner_job,
          v_one_up_no,
          '01',
          SYSDATE,
          p_prospect_code
    );
          
    INSERT INTO general.gjbprun(
          gjbprun_job,
          gjbprun_one_up_no,
          gjbprun_number,
          gjbprun_activity_date,
          gjbprun_value
    )
    VALUES(
          v_banner_job,
          v_one_up_no,
          '02',
          SYSDATE,
          NULL
    );
          
    INSERT INTO general.gjbprun(
          gjbprun_job,
          gjbprun_one_up_no,
          gjbprun_number,
          gjbprun_activity_date,
          gjbprun_value
    )
    VALUES(
          v_banner_job,
          v_one_up_no,
          '03',
          SYSDATE,
          NULL
    );
          
     INSERT INTO general.gjbprun(
          gjbprun_job,
          gjbprun_one_up_no,
          gjbprun_number,
          gjbprun_activity_date,
          gjbprun_value
    )
    VALUES(
          v_banner_job,
          v_one_up_no,
          '04',
          SYSDATE,
          NULL
    );
          
    INSERT INTO general.gjbprun(
          gjbprun_job,
          gjbprun_one_up_no,
          gjbprun_number,
          gjbprun_activity_date,
          gjbprun_value
    )
    VALUES(
          v_banner_job,
          v_one_up_no,
          '05',
          SYSDATE,
          NULL
    );
    
    INSERT INTO general.gjbprun(
          gjbprun_job,
          gjbprun_one_up_no,
          gjbprun_number,
          gjbprun_activity_date,
          gjbprun_value
    )
    VALUES(
          v_banner_job,
          v_one_up_no,
          '06',
          SYSDATE,
          p_match_status
    );
    
    INSERT INTO general.gjbprun(
          gjbprun_job,
          gjbprun_one_up_no,
          gjbprun_number,
          gjbprun_activity_date,
          gjbprun_value
    )
    VALUES(
          v_banner_job,
          v_one_up_no,
          '07',
          SYSDATE,
          p_load_status
    );
    
    INSERT INTO general.gjbprun(
          gjbprun_job,
          gjbprun_one_up_no,
          gjbprun_number,
          gjbprun_activity_date,
          gjbprun_value
    )
    VALUES(
          v_banner_job,
          v_one_up_no,
          '08',
          SYSDATE,
          NULL
    );
    
    INSERT INTO general.gjbprun(
          gjbprun_job,
          gjbprun_one_up_no,
          gjbprun_number,
          gjbprun_activity_date,
          gjbprun_value
    )
    VALUES(
          v_banner_job,
          v_one_up_no,
          '09',
          SYSDATE,
          'Y'
    );
    
    INSERT INTO general.gjbprun(
          gjbprun_job,
          gjbprun_one_up_no,
          gjbprun_number,
          gjbprun_activity_date,
          gjbprun_value
    )
    VALUES(
          v_banner_job,
          v_one_up_no,
          '10',
          SYSDATE,
          p_audit_or_update
    );
    
    INSERT INTO general.gjbprun(
          gjbprun_job,
          gjbprun_one_up_no,
          gjbprun_number,
          gjbprun_activity_date,
          gjbprun_value
    )
    VALUES(
          v_banner_job,
          v_one_up_no,
          '10',
          SYSDATE,
          NULL
    );
    
    INSERT INTO general.gjbprun(
          gjbprun_job,
          gjbprun_one_up_no,
          gjbprun_number,
          gjbprun_activity_date,
          gjbprun_value
    )
    VALUES(
          v_banner_job,
          v_one_up_no,
          '99',
          SYSDATE,
          '55'
    );
            
    COMMIT; 
         
    output_file := UTL_FILE.fopen (outfile_loc, p_outfile_name, 'w');  
    UTL_FILE.put_line (output_file, v_one_up_no);
    UTL_FILE.fclose_all;
  EXCEPTION
    WHEN NO_DATA_FOUND THEN
      v_error_mesg :=  'Unable to run SRRPREL because of one-up number.' || SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300); 
      p_handle_err('UNKNOWN', v_error_mesg, v_proc_name);
    WHEN OTHERS THEN
      ROLLBACK TO SAVEPOINT pre_gjbprun_insert;
      v_error_mesg :=  SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300); 
      p_handle_err('UNKNOWN', v_error_mesg, v_proc_name);
  END p_srtpurg_in_gjbprun;
  ---------
  PROCEDURE p_load_sorcont(
    p_pidm            sorcont.sorcont_pidm%TYPE,
    p_ctyp_code       sorcont.sorcont_ctyp_code%TYPE,
    p_ctyp_date       sorcont.sorcont_contact_date%TYPE,
    p_data_origin     VARCHAR2,
    p_error_out   OUT VARCHAR2)
  IS  
    v_record_exists   VARCHAR2(1);

  BEGIN
    v_record_exists := '';
    BEGIN
      SELECT DISTINCT 'Y' 
      INTO v_record_exists
      FROM sorcont WHERE sorcont_pidm = p_pidm
      AND sorcont_ctyp_code = p_ctyp_code
      AND trunc(sorcont_contact_date) = trunc(p_ctyp_date);
    EXCEPTION
      WHEN NO_DATA_FOUND THEN
        v_record_exists := 'N';
    END;
   
    --If no duplicate exist in SORCONT, insert into sorcont
    IF v_record_exists = 'N'
    THEN 
      INSERT INTO sorcont(
                    sorcont_pidm, 
                    sorcont_ctyp_code, 
                    sorcont_contact_date,
                    sorcont_user_id,
                    sorcont_activity_date, 
                    sorcont_data_origin)
            VALUES (p_pidm, 
                    p_ctyp_code, 
                    p_ctyp_date,
                    gb_common.f_sct_user,
                    sysdate, 
                    p_data_origin);
      COMMIT;
    END IF;
  EXCEPTION
    WHEN OTHERS THEN
      ROLLBACK;
      p_error_out :='Error inserting contact codes into SORCONT for Banner PIDM: ' || p_pidm || '. ' || SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300);
  END p_load_sorcont;
  ----------
  --OVERLOAD PROCEDURE
  --no date input
  --sorcont_contact_date hard coded as sysdate
   PROCEDURE p_load_sorcont(
    p_pidm            sorcont.sorcont_pidm%TYPE,
    p_ctyp_code       sorcont.sorcont_ctyp_code%TYPE,
    p_data_origin     VARCHAR2,
    p_error_out   OUT VARCHAR2)
  IS  
    v_sorcont_cnt     NUMBER := 0;
  BEGIN
    SELECT COUNT(*)
      INTO v_sorcont_cnt
      FROM sorcont 
     WHERE sorcont_pidm = p_pidm
       AND sorcont_ctyp_code = p_ctyp_code;
       --AND trunc(sorcont_contact_date) = trunc(p_ctyp_date);--remove to fix error of multiple instances when contact_date = sysdate
   
    --If no duplicate exist in SORCONT, insert into sorcont
    IF v_sorcont_cnt = 0
    THEN 
      INSERT INTO sorcont(
                      sorcont_pidm, 
                      sorcont_ctyp_code, 
                      sorcont_contact_date,
                      sorcont_user_id,
                      sorcont_activity_date, 
                      sorcont_data_origin)
              VALUES (p_pidm, 
                      p_ctyp_code, 
                      sysdate,
                      gb_common.f_sct_user,
                      sysdate, 
                      p_data_origin);
                      
      COMMIT;
    END IF;
  EXCEPTION
    WHEN OTHERS THEN
      ROLLBACK;
      p_error_out :='Error inserting contact codes into SORCONT for Banner PIDM: ' || p_pidm || '. ' || SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300);
  END p_load_sorcont;
  ----------
  PROCEDURE p_insert_srrrsrc(
    p_pidm           srrrsrc.srrrsrc_pidm%TYPE,
    p_sbgi_code      srrrsrc.srrrsrc_sbgi_code%TYPE,
    p_term           srbrecr.srbrecr_term_code%TYPE,
    p_seqno          srbrecr.srbrecr_admin_seqno%TYPE,
    p_data_origin    VARCHAR2) 
  IS
    v_record_exists   VARCHAR2(1);
    v_primsrce_exists VARCHAR2(1);
    v_primsrce  VARCHAR2(1);
  BEGIN
  
    IF p_sbgi_code IS NULL THEN
      RETURN;
    END IF;
    v_record_exists   := '';
    v_primsrce_exists := '' ;
    v_primsrce  := '';
    BEGIN
      SELECT DISTINCT 'Y'
        INTO v_primsrce_exists
        FROM srrrsrc
       WHERE srrrsrc_pidm = p_pidm AND srrrsrc_term_code = p_term AND
             srrrsrc_admin_seqno = p_seqno AND srrrsrc_primary_srce_ind = 'Y';
    EXCEPTION
      WHEN no_data_found THEN
        v_primsrce_exists := 'N';
    END;
  
    BEGIN
      SELECT DISTINCT 'Y'
        INTO v_record_exists
        FROM srrrsrc
       WHERE srrrsrc_pidm = p_pidm AND srrrsrc_term_code = p_term AND
             srrrsrc_admin_seqno = p_seqno AND srrrsrc_sbgi_code = p_sbgi_code;
    EXCEPTION
      WHEN no_data_found THEN
        v_record_exists := 'N';
    END;
  
    IF v_record_exists = 'N' THEN    
      IF v_primsrce_exists = 'Y' THEN
        v_primsrce := '';
      ELSE
        v_primsrce := 'Y';
      END IF;
  
      INSERT INTO srrrsrc
        (srrrsrc_admin_seqno,
         srrrsrc_pidm,
         srrrsrc_term_code,
         srrrsrc_sbgi_code,
         srrrsrc_primary_srce_ind,
         srrrsrc_user_id,
         srrrsrc_activity_date,
         srrrsrc_data_origin)
      VALUES
        (p_seqno,
         p_pidm,
         p_term,
         p_sbgi_code,
         v_primsrce,
         gb_common.f_sct_user, 
         SYSDATE,
         p_data_origin);
      COMMIT;
    END IF;
  END p_insert_srrrsrc; 

  ----------
  PROCEDURE p_load_sorints(
    p_pidm           sorints.sorints_pidm%TYPE,
    p_ints_code      sorints.sorints_ints_code%TYPE,
    p_data_origin    VARCHAR2,
    p_error_out  OUT VARCHAR2)
  IS
    v_sorints_cnt       NUMBER := 0;
    v_old_ints_date     DATE;
    v_appl_min_yr       VARCHAR2(4);
    v_appl_term         stvterm.stvterm_code%TYPE;
  BEGIN
    IF p_ints_code IS NULL THEN
      RETURN;
    END IF;
    
    SELECT COUNT(*)
      INTO v_sorints_cnt
      FROM sorints
     WHERE sorints_pidm = p_pidm 
       AND sorints_ints_code = p_ints_code;
    
    IF v_sorints_cnt = 0
    THEN
      BEGIN
       INSERT INTO sorints(
              sorints_pidm,
              sorints_ints_code,
              sorints_activity_date,
              sorints_user_id,
              sorints_data_origin)
       VALUES(
              p_pidm,
              p_ints_code,
              SYSDATE,
              gb_common.f_sct_user,
              p_data_origin);
       COMMIT;
      EXCEPTION
        WHEN OTHERS THEN
          p_error_out :='Error inserting interest code ' || p_ints_code || ' into SORINTS for Banner PIDM ' || p_pidm || '. ' || SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300);
      END;
      
    END IF;
    IF v_sorints_cnt = 1
    THEN
      SELECT sorints_activity_date
        INTO v_old_ints_date
        FROM sorints
       WHERE sorints_pidm = p_pidm 
         AND sorints_ints_code = p_ints_code;
      
      v_appl_term := atowner.f_term_on_date('SM', sysdate); --note: for summer this function returned the year rounded down
     
      IF SUBSTR(v_appl_term, 5, 2) = '04' THEN 
        --if currently it's summer term we round up the years
        v_appl_min_yr := SUBSTR(v_appl_term, 1, 4);
      ELSE 
        v_appl_min_yr := TO_CHAR(TO_NUMBER(SUBSTR(v_appl_term, 1, 4)) - 1);
      END IF;
       
      --If the SORINTS_ACTIVITY_DATE of existing record
      --is before July 1st beginning the academic year
      --update SORINTS_ACTIVITY_DATE to SYSDATE
      --else leave it alone
      IF v_old_ints_date < to_date('07/01/'|| v_appl_min_yr, 'MM/DD/YYYY')
      THEN
          BEGIN
           UPDATE sorints
              SET sorints_activity_date = SYSDATE,
                  sorints_user_id = gb_common.f_sct_user,
                  sorints_data_origin = p_data_origin
            WHERE sorints_pidm = p_pidm 
              AND sorints_ints_code = p_ints_code;
          
           COMMIT;
          EXCEPTION
            WHEN OTHERS THEN
              p_error_out :='Error updating activity date for existing interest code ' || p_ints_code || ' in SORINTS for Banner PIDM ' || p_pidm || '. ' || SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300);
          END;
        END IF; 
    END IF;
  END p_load_sorints;
  -----------
  PROCEDURE p_load_saraatt(
    p_pidm           saraatt.saraatt_pidm%TYPE,
    p_term_code      saraatt.saraatt_term_code%TYPE,
    p_appl_no        saraatt.saraatt_appl_no%TYPE,
    p_atts_code      saraatt.saraatt_atts_code%TYPE,
    p_data_origin    VARCHAR2,
    p_error_out  OUT VARCHAR2)
  IS 
    v_saraatt_cnt   NUMBER := 0;
  BEGIN
    IF p_atts_code IS NULL THEN
      RETURN;
    END IF;
    SELECT COUNT(*)
      INTO v_saraatt_cnt
      FROM saraatt
     WHERE saraatt_pidm = p_pidm
       AND saraatt_term_code = p_term_code
       AND saraatt_appl_no = p_appl_no
       AND saraatt_atts_code = p_atts_code;
    
    IF v_saraatt_cnt = 0
    THEN
      BEGIN
       INSERT INTO saraatt(
              saraatt_pidm,
              saraatt_term_code,
              saraatt_appl_no,
              saraatt_atts_code,
              saraatt_activity_date,
              saraatt_user_id,
              saraatt_data_origin)
       VALUES(
              p_pidm,
              p_term_code,
              p_appl_no,
              p_atts_code,
              sysdate,
              gb_common.f_sct_user,
              p_data_origin);
       COMMIT;
      EXCEPTION
        WHEN OTHERS THEN
          p_error_out :='Error inserting code ' || p_atts_code || ' into SARAATT for Banner PIDM: ' || p_pidm || '. ' || SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300);
      END;
    END IF;
  
  END p_load_saraatt;
  -----------
  PROCEDURE p_load_adm_rating(
    p_pidm           saradap.saradap_pidm%TYPE,
    p_term_code      saradap.saradap_term_code_entry%TYPE,
    p_appl_no        saradap.saradap_appl_no%TYPE,
    p_ratp_code      stvratp.stvratp_code%TYPE,
    p_rating         VARCHAR2,
    p_username       VARCHAR2,
    p_data_origin    VARCHAR2,
    p_err_out    OUT VARCHAR2)
  IS
    v_rating         VARCHAR2(2 CHAR);
    v_old_pr         saradap.saradap_rtyp_code%TYPE;
    v_old_rr         sarappd.sarappd_apdc_code%TYPE;
    v_rating_cnt     NUMBER;
    v_decision_cnt   NUMBER;
    v_row_id_in      gb_common.internal_record_id_type;
    v_row_id_out     gb_common.internal_record_id_type;
    v_sarappd_seqno  NUMBER;
    invalid_reader_rating   EXCEPTION;
    invalid_reader_type     EXCEPTION;
  BEGIN
    IF p_rating < 10 AND p_rating > 0
    THEN
      v_rating :='0'||TO_CHAR(p_rating,'TM9');
    ELSIF p_rating = 10
    THEN 
      v_rating := TO_CHAR(p_rating,'TM9');
    ELSE 
      RAISE invalid_reader_rating;
    END IF;
    
    IF p_ratp_code NOT IN ('PR', 'RR')
    THEN
      RAISE invalid_reader_type;
    END IF;
 
    IF p_ratp_code = 'PR' 
    THEN
      SELECT saradap_rtyp_code, rowid
        INTO v_old_pr, v_row_id_in 
        FROM saradap 
        WHERE saradap_pidm = p_pidm
        AND saradap_term_code_entry = p_term_code
        AND saradap_appl_no = p_appl_no;
  
      IF NVL(v_old_pr, '00') != v_rating  
      THEN      
        BEGIN
          baninst1.sb_admissionsapplication.p_update(
                    p_pidm              =>  p_pidm,
                    p_term_code_entry   =>  p_term_code,
                    p_appl_no           =>  p_appl_no,
                    p_rtyp_code         =>  v_rating,
                    --p_intv_code         =>  v_intv_code,
                    p_data_origin       =>  p_data_origin,
                    p_user_id           =>  gb_common.f_sct_user,
                    p_rowid             =>  v_row_id_in);
          gb_common.p_commit;       
        EXCEPTION
          WHEN OTHERS THEN --record the Oracle error in the track error table
            p_err_out := 'Unable to update PR data in SARADAP for '|| p_pidm || ' for term ' || p_term_code || '. '|| SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300);
        END; 
      END IF;
    END IF;

    SELECT COUNT(*) 
      INTO v_rating_cnt
      FROM sarappd 
     WHERE sarappd_apdc_code <= '10'--rating--greater equal is better in performance
       AND sarappd_pidm = p_pidm
       AND sarappd_term_code_entry= p_term_code
       AND sarappd_appl_no = p_appl_no;

    SELECT COUNT(*)
      INTO v_decision_cnt
      FROM sarappd 
     WHERE (sarappd_apdc_code > '10' AND sarappd_apdc_code < '20')--decisions------greater equal is better in performance
       AND sarappd_pidm = p_pidm
       AND sarappd_term_code_entry = p_term_code
       AND sarappd_appl_no = p_appl_no;
    
    IF p_ratp_code = 'RR' 
    THEN
      IF v_decision_cnt = 0 --ok to insert/update the RR if no decision yet
      THEN
        IF v_rating_cnt = 0 --reader rating not exist yet 
        THEN
          v_sarappd_seqno := 1;
          BEGIN
            sb_application_decision.p_create(
                p_pidm              => p_pidm,
                p_term_code_entry   => p_term_code,
                p_appl_no           => p_appl_no,
                p_seq_no_inout      => v_sarappd_seqno,
                p_apdc_date         => SYSDATE,
                p_apdc_code         => v_rating,
                p_maint_ind         => 'U',
                p_user              => UPPER(p_username),
                p_data_origin       => p_data_origin,
                p_rowid_out         => v_row_id_out
            );
            gb_common.p_commit;
          EXCEPTION
            WHEN OTHERS THEN
              p_err_out := 'Unable to insert reader rating in SARAPPD for '|| p_pidm || ' for term ' || p_term_code || '. '|| SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300);    
          END;    
        ELSE                --reader rating already exist, update         
          SELECT MAX(sarappd_seq_no)
            INTO v_sarappd_seqno
            FROM sarappd 
           WHERE sarappd_apdc_code <= '10'
             AND sarappd_pidm = p_pidm
             AND sarappd_term_code_entry= p_term_code
             AND sarappd_appl_no = p_appl_no;
             
          SELECT sarappd_apdc_code
            INTO v_old_rr
            FROM sarappd
           WHERE sarappd_pidm = p_pidm
             AND sarappd_term_code_entry= p_term_code
             AND sarappd_appl_no = p_appl_no
             AND sarappd_seq_no = v_sarappd_seqno;
          IF v_old_rr <> v_rating
          THEN
            BEGIN
                sb_application_decision.p_update(
                      p_pidm              => p_pidm,
                      p_term_code_entry   => p_term_code,
                      p_appl_no           => p_appl_no,
                      p_seq_no            => v_sarappd_seqno,
                      p_apdc_date         => SYSDATE,
                      p_apdc_code         => v_rating,
                      p_maint_ind         => 'U',
                      p_user              => UPPER(p_username),
                      p_data_origin       => p_data_origin         
                );
                  
                gb_common.p_commit;
              EXCEPTION
                WHEN OTHERS THEN
                  p_err_out := 'Unable to update reader rating in SARAPPD for '|| p_pidm || ' for term ' || p_term_code || '. '|| SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300);
              END;
          END IF;
        END IF;
      ELSE --decision already exist
        IF v_rating_cnt = 0 --reader rating not exist yet 
        THEN 
          p_err_out := 'Cannot insert new reader rating after decision has already been added for pidm ' ||p_pidm|| ' for term ' || p_term_code || '.';
        END IF;
      END IF;
    END IF;
  EXCEPTION
    WHEN invalid_reader_rating THEN
      p_err_out := 'Reader rating ' || p_rating || ' is an invalid rating for Banner pidm ' || p_pidm || ' for term ' || p_term_code || '.';
    WHEN invalid_reader_type THEN
      p_err_out := 'Reader type ' || p_ratp_code || ' is an invalid code for Banner pidm ' || p_pidm || ' for term ' || p_term_code || '. Expect only PR or RR.';
  END p_load_adm_rating;
  ----------
  PROCEDURE p_load_adm_decision(
    p_pidm           saradap.saradap_pidm%TYPE,
    p_term_code      saradap.saradap_term_code_entry%TYPE,
    p_appl_no        saradap.saradap_appl_no%TYPE,
    p_apdc_code      stvapdc.stvapdc_code%TYPE,
    p_data_origin    VARCHAR2,
    p_err_out    OUT VARCHAR2)
  IS 
    v_decision_cnt   NUMBER;
    v_old_decision   sarappd.sarappd_apdc_code%TYPE;
    v_row_id_in      gb_common.internal_record_id_type;
    v_row_id_out     gb_common.internal_record_id_type;
    v_sarappd_seqno  NUMBER;
    v_iden_resp_exist NUMBER;
    v_iden_depo_exist NUMBER;
  BEGIN
    IF p_apdc_code IN ('11','12','13','15')--decisions that would need to overwrite each others
    THEN
      SELECT COUNT(*)
        INTO v_decision_cnt
        FROM sarappd 
       WHERE sarappd_apdc_code in ('11','12','13','15')
         AND sarappd_pidm = p_pidm
         AND sarappd_term_code_entry = p_term_code
         AND sarappd_appl_no = p_appl_no;
      --insert if missing and there are no decisions yet
      IF v_decision_cnt = 0 
      THEN
        BEGIN
          SELECT MAX(sarappd_seq_no)
          INTO v_sarappd_seqno
          FROM sarappd
          WHERE sarappd_pidm = p_pidm
          AND sarappd_term_code_entry = p_term_code
          AND sarappd_appl_no = p_appl_no;
        EXCEPTION
          WHEN no_data_found THEN
            v_sarappd_seqno := 0;
        END;
        v_sarappd_seqno := v_sarappd_seqno + 1;
     
        BEGIN     
          sb_application_decision.p_create(
                  p_pidm              => p_pidm,
                  p_term_code_entry   => p_term_code,
                  p_appl_no           => p_appl_no,
                  p_seq_no_inout      => v_sarappd_seqno,
                  p_apdc_date         => SYSDATE,
                  p_apdc_code         => p_apdc_code,
                  p_maint_ind         => 'U',
                  p_user              => gb_common.f_sct_user,
                  p_data_origin       => p_data_origin,
                  p_rowid_out         => v_row_id_out);     
            gb_common.p_commit;
        EXCEPTION
          WHEN OTHERS THEN
            p_err_out := 'Unable to insert decision in SARAPPD for '|| p_pidm || ' for term ' || p_term_code || '. '|| SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300);
        END;
      ELSE 
        --update otherwise
        SELECT MAX(sarappd_seq_no)
              INTO v_sarappd_seqno
              FROM sarappd 
             WHERE sarappd_apdc_code in ('11','12','13','15')
               AND sarappd_pidm = p_pidm
               AND sarappd_term_code_entry= p_term_code
               AND sarappd_appl_no = p_appl_no;
               
            SELECT sarappd_apdc_code
              INTO v_old_decision
              FROM sarappd
             WHERE sarappd_pidm = p_pidm
               AND sarappd_term_code_entry= p_term_code
               AND sarappd_appl_no = p_appl_no
               AND sarappd_seq_no = v_sarappd_seqno;
        IF v_old_decision <> p_apdc_code
        THEN
          BEGIN   
            sb_application_decision.p_update(
                  p_pidm              => p_pidm,
                  p_term_code_entry   => p_term_code,
                  p_appl_no           => p_appl_no,
                  p_seq_no            => v_sarappd_seqno,
                  p_apdc_date         => SYSDATE,
                  p_apdc_code         => p_apdc_code,
                  p_maint_ind         => 'U',
                  p_user              => gb_common.f_sct_user,
                  p_data_origin       => p_data_origin );   
            gb_common.p_commit;
          EXCEPTION
            WHEN OTHERS THEN
              p_err_out := 'Unable to update decision in SARAPPD for '|| p_pidm || ' for term ' || p_term_code || '. '|| SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300);
        
          END;
        END IF;
      END IF;
    END IF;
    IF p_apdc_code = '18'--decisions that don't need overwrite--for waitlist admit
    THEN
      --check if identical code already there
      SELECT COUNT(*)
        INTO v_decision_cnt
        FROM sarappd 
       WHERE sarappd_apdc_code = '18'
         AND sarappd_pidm = p_pidm
         AND sarappd_term_code_entry = p_term_code
         AND sarappd_appl_no = p_appl_no;
      IF v_decision_cnt = 0
      THEN
        BEGIN
          sb_application_decision.p_create(
                      p_pidm              => p_pidm,
                      p_term_code_entry   => p_term_code,
                      p_appl_no           => p_appl_no,
                      p_seq_no_inout      => v_sarappd_seqno,
                      p_apdc_date         => SYSDATE,
                      p_apdc_code         => p_apdc_code,
                      p_maint_ind         => 'U',
                      p_user              => gb_common.f_sct_user,
                      p_data_origin       => p_data_origin,
                      p_rowid_out         => v_row_id_out);     
          gb_common.p_commit;
        EXCEPTION
          WHEN OTHERS THEN
            p_err_out := 'Unable to insert student waitlist decision in SARAPPD for '|| p_pidm || ' for term ' || p_term_code || '. '|| SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300);
        END;     
      END IF;
    END IF;
  END p_load_adm_decision;
  ---------
  PROCEDURE p_load_adm_response(
    p_pidm           saradap.saradap_pidm%TYPE,
    p_term_code      saradap.saradap_term_code_entry%TYPE,
    p_appl_no        saradap.saradap_appl_no%TYPE,
    p_apdc_code      stvapdc.stvapdc_code%TYPE,
    p_data_origin    VARCHAR2,
    p_err_out    OUT VARCHAR2)
  IS 
    v_iden_resp_exist     NUMBER;
    v_row_id_out          gb_common.internal_record_id_type;
    v_sarappd_seqno       NUMBER;
    invalid_adm_response  EXCEPTION;
  BEGIN
    IF p_apdc_code NOT IN ('20', '21', '22') AND SUBSTR(p_apdc_code, 1, 1) <> '4'
    THEN 
      raise invalid_adm_response;
    END IF;
    
    SELECT COUNT(*) 
      INTO v_iden_resp_exist
      FROM sarappd 
      WHERE sarappd_apdc_code = p_apdc_code
      AND sarappd_pidm = p_pidm
      AND sarappd_term_code_entry= p_term_code
      AND sarappd_appl_no = p_appl_no;
    IF v_iden_resp_exist = 0
    THEN
      BEGIN
        SAVEPOINT pre_response;
        sb_application_decision.p_create(
                      p_pidm              => p_pidm,
                      p_term_code_entry   => p_term_code,
                      p_appl_no           => p_appl_no,
                      p_seq_no_inout      => v_sarappd_seqno,
                      p_apdc_date         => SYSDATE,
                      p_apdc_code         => p_apdc_code,
                      p_maint_ind         => 'U',
                      p_user              => gb_common.f_sct_user,
                      p_data_origin       => p_data_origin,
                      p_rowid_out         => v_row_id_out);     
        gb_common.p_commit;
      EXCEPTION
        WHEN OTHERS THEN
          ROLLBACK TO pre_response;
          p_err_out := 'Unable to insert student response in SARAPPD for '|| p_pidm || ' for term ' || p_term_code || '. '|| SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300);
      END;
        
      IF p_apdc_code = '45' AND p_err_out IS NULL 
      THEN
        BEGIN
          SAVEPOINT pre_wrsn_code;
          baninst1.sb_admissionsapplication.p_update(
                  p_pidm              =>  p_pidm,
                  p_term_code_entry   =>  p_term_code,
                  p_appl_no           =>  p_appl_no,
                  p_wrsn_code         => 'WB',
                  p_data_origin       => 'SLATE',
                  p_user_id           =>  gb_common.f_sct_user);
          COMMIT;
        EXCEPTION
          WHEN OTHERS THEN 
            ROLLBACK TO pre_wrsn_code;
            p_err_out := 'Unable to update SARADAP_WRSN_CODE = ''WB'' for '|| p_pidm || ' for term ' || p_term_code || '. '|| SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300);
        END;
      END IF;
    END IF;
  EXCEPTION
    WHEN invalid_adm_response THEN
      p_err_out := 'Student response code ' || p_apdc_code || ' is not a valid reponse code for Banner pidm ' || p_pidm || ' for term ' || p_term_code || '. ';  
  
  END p_load_adm_response;
  ---------
  PROCEDURE p_load_adm_deposit(
    p_pidm           saradap.saradap_pidm%TYPE,
    p_term_code      saradap.saradap_term_code_entry%TYPE,
    p_appl_no        saradap.saradap_appl_no%TYPE,
    p_apdc_code      stvapdc.stvapdc_code%TYPE,
    p_data_origin    VARCHAR2,
    p_err_out    OUT VARCHAR2)
  IS 
    v_iden_depo_exist   NUMBER;
    v_row_id_out     gb_common.internal_record_id_type;
    v_sarappd_seqno  NUMBER;
    invalid_adm_deposit EXCEPTION;
  BEGIN
    --update/insert for deposit
    IF SUBSTR(p_apdc_code, 1, 1) <> 'D' AND SUBSTR(p_apdc_code, 1, 1) <> 'P'
    THEN
      RAISE invalid_adm_deposit;
    END IF;
    
     --check if identical code already there
    SELECT COUNT(*) 
      INTO v_iden_depo_exist
      FROM sarappd 
     WHERE sarappd_apdc_code = p_apdc_code
       AND sarappd_pidm = p_pidm
       AND sarappd_term_code_entry= p_term_code
       AND sarappd_appl_no = p_appl_no;
    IF v_iden_depo_exist = 0
    THEN
      BEGIN
        sb_application_decision.p_create(
                    p_pidm              => p_pidm,
                    p_term_code_entry   => p_term_code,
                    p_appl_no           => p_appl_no,
                    p_seq_no_inout      => v_sarappd_seqno,
                    p_apdc_date         => SYSDATE,
                    p_apdc_code         => p_apdc_code,
                    p_maint_ind         => 'U',
                    p_user              => gb_common.f_sct_user,
                    p_data_origin       => p_data_origin,
                    p_rowid_out         => v_row_id_out);     
        gb_common.p_commit;
      EXCEPTION
        WHEN OTHERS THEN
          p_err_out := 'Unable to insert student deposit in SARAPPD for '|| p_pidm || ' for term ' || p_term_code || '. '|| SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300);
      END;
    END IF;
  EXCEPTION
    WHEN invalid_adm_deposit THEN
      p_err_out := 'Student deposit code ' || p_apdc_code || ' is not a valid deposit code for Banner pidm ' || p_pidm || ' for term ' || p_term_code || '.';  
  END p_load_adm_deposit;
  ---------
  
  PROCEDURE p_import_pers_contacts
  IS
    v_pidm            goradid.goradid_pidm%TYPE;
    v_szscont_rec     szscont%ROWTYPE;
    v_ctyp_code       sorcont.sorcont_ctyp_code%TYPE;
    v_ctyp_date       DATE;
    v_term_code       stvterm.stvterm_code%TYPE;
    no_srbrecr_row     EXCEPTION;
    v_srbrecr_seqno   NUMBER;
    v_error_mesg      VARCHAR2(200 CHAR);
    v_proc_name       VARCHAR2(30 CHAR) := 'p_import_pers_contacts';
    --get only id with guid in GORADID or a pidm already populated
    CURSOR szscont_c
    IS 
      SELECT szscont.* FROM szscont
      JOIN goradid on szscont_guid = goradid_additional_id
      WHERE goradid_adid_code  = 'SL'    
      AND NOT EXISTS 
        (SELECT goradid_additional_id 
        FROM goradid 
        WHERE goradid_additional_id = szscont_guid
        AND goradid_adid_code = 'SL'
        GROUP BY goradid_additional_id HAVING COUNT(*) >1)
      AND NOT EXISTS
        (SELECT goradid_pidm
        FROM goradid
        WHERE goradid_pidm = szscont_pidm
        AND goradid_adid_code = 'SL' 
        GROUP BY goradid_pidm HAVING COUNT(*) >1);
  ----look at goradid pidm
  BEGIN
    IF szscont_c%ISOPEN
    THEN
      CLOSE szscont_c;
    END IF;
    OPEN szscont_c;
    LOOP
      FETCH szscont_c INTO v_szscont_rec;
      EXIT WHEN szscont_c%NOTFOUND;
      v_term_code := v_szscont_rec.szscont_entry_term;
      v_pidm := f_get_pidm_from_guid(v_szscont_rec.szscont_guid, 'SL');

      BEGIN
        v_srbrecr_seqno := baninst1.sb_recruit.F_GetSrbrecrSeqno(v_pidm,  v_term_code, 'C');--get current seqno, return 0 if null
        IF v_srbrecr_seqno = 0
        THEN
  
          RAISE no_srbrecr_row; --no matching recruit record in SRBRECR
  
        END IF;  
        
        v_srbrecr_seqno := baninst1.sb_recruit.F_GetSrbrecrSeqno(v_pidm,  v_term_code, 'C');--get current seqno, return 0 if null
        
        --------------------------------------------
        ----------------Campus Tour-----------------  
        IF v_szscont_rec.szscont_atr_date IS NOT NULL
        THEN 
          v_ctyp_code := 'ATR';
          v_ctyp_date := TO_DATE(v_szscont_rec.szscont_atr_date, 'YYYY-MM-DD');
          --Insert into sorcont and srrrsrc when ok
          v_error_mesg := NULL;
          p_load_sorcont(v_pidm, v_ctyp_code, v_ctyp_date, 'SLATE', v_error_mesg);
          IF v_error_mesg <> NULL
          THEN
            p_handle_err(v_szscont_rec.szscont_guid, v_error_mesg, v_proc_name);
          END IF;
          p_insert_srrrsrc(v_pidm, v_ctyp_code, v_term_code, v_srbrecr_seqno, 'SLATE');
          
        END IF;
        --------------------------------------------  
        ----------------Info Session----------------  
        IF v_szscont_rec.szscont_ais_date IS NOT NULL
        THEN 
          v_ctyp_code := 'AIS';
          v_ctyp_date := TO_DATE(v_szscont_rec.szscont_ais_date, 'YYYY-MM-DD');
           --Insert into sorcont and srrrsrc when ok
          v_error_mesg := NULL;
          p_load_sorcont(v_pidm, v_ctyp_code, v_ctyp_date, 'SLATE', v_error_mesg);
          IF v_error_mesg <> NULL
          THEN
            p_handle_err(v_szscont_rec.szscont_guid, v_error_mesg, v_proc_name);
          END IF;
          p_insert_srrrsrc(v_pidm, v_ctyp_code, v_term_code, v_srbrecr_seqno, 'SLATE');
        END IF;  
        --------------------------------------------
        ------------On Campus Interview-------------
        IF v_szscont_rec.szscont_aiv_date IS NOT NULL
        THEN 
          v_ctyp_code := 'AIV';
          v_ctyp_date := TO_DATE(v_szscont_rec.szscont_aiv_date, 'YYYY-MM-DD');
          --Insert into sorcont and srrrsrc when ok
          v_error_mesg := NULL;
          p_load_sorcont(v_pidm, v_ctyp_code, v_ctyp_date, 'SLATE', v_error_mesg);
          IF v_error_mesg <> NULL
          THEN
            p_handle_err(v_szscont_rec.szscont_guid, v_error_mesg, v_proc_name);
          END IF;
          p_insert_srrrsrc(v_pidm, v_ctyp_code, v_term_code, v_srbrecr_seqno, 'SLATE');
        END IF;  
        --------------------------------------------
        -------------Alumnae Interview--------------
        IF v_szscont_rec.szscont_aia_date IS NOT NULL
        THEN 
          v_ctyp_code := 'AIA';
          v_ctyp_date := TO_DATE(v_szscont_rec.szscont_aia_date, 'YYYY-MM-DD');
          --Insert into sorcont and srrrsrc when ok
          v_error_mesg := NULL;
          p_load_sorcont(v_pidm, v_ctyp_code, v_ctyp_date, 'SLATE', v_error_mesg);
          IF v_error_mesg <> NULL
          THEN
            p_handle_err(v_szscont_rec.szscont_guid, v_error_mesg, v_proc_name);
          END IF;
          p_insert_srrrsrc(v_pidm, v_ctyp_code, v_term_code, v_srbrecr_seqno, 'SLATE');
        END IF;  
        
        --------------------------------------------
        ----------------Alumnae Fair----------------
        IF v_szscont_rec.szscont_aaf_date IS NOT NULL
        THEN 
          v_ctyp_code := 'AAF';
          v_ctyp_date := TO_DATE(v_szscont_rec.szscont_aaf_date, 'YYYY-MM-DD');
          --Insert into sorcont and srrrsrc when ok
          v_error_mesg := NULL;
          p_load_sorcont(v_pidm, v_ctyp_code, v_ctyp_date, 'SLATE', v_error_mesg);
          IF v_error_mesg <> NULL
          THEN
            p_handle_err(v_szscont_rec.szscont_guid, v_error_mesg, v_proc_name);
          END IF;
          p_insert_srrrsrc(v_pidm, v_ctyp_code, v_term_code, v_srbrecr_seqno, 'SLATE');
        END IF;  
        
        --------------------------------------------
        --------------Alumnae Referral--------------
        IF v_szscont_rec.szscont_aar_date IS NOT NULL
        THEN 
          v_ctyp_code := 'AAR';
          v_ctyp_date := TO_DATE(v_szscont_rec.szscont_aar_date, 'YYYY-MM-DD');
          --Insert into sorcont and srrrsrc when ok
          v_error_mesg := NULL;
          p_load_sorcont(v_pidm, v_ctyp_code, v_ctyp_date, 'SLATE', v_error_mesg);
          IF v_error_mesg <> NULL
          THEN
            p_handle_err(v_szscont_rec.szscont_guid, v_error_mesg, v_proc_name);
          END IF;
          p_insert_srrrsrc(v_pidm, v_ctyp_code, v_term_code, v_srbrecr_seqno, 'SLATE');
        END IF;                          
        --------------------------------------------
        --------------Book Award Winner-------------     
        IF v_szscont_rec.szscont_aba_date IS NOT NULL
        THEN 
          v_ctyp_code := 'ABA';
          v_ctyp_date := TO_DATE(v_szscont_rec.szscont_aba_date, 'YYYY-MM-DD');
          --Insert into sorcont and srrrsrc when ok
          v_error_mesg := NULL;
          p_load_sorcont(v_pidm, v_ctyp_code, v_ctyp_date, 'SLATE', v_error_mesg);
          IF v_error_mesg <> NULL
          THEN
            p_handle_err(v_szscont_rec.szscont_guid, v_error_mesg, v_proc_name);
          END IF;
          p_insert_srrrsrc(v_pidm, v_ctyp_code, v_term_code, v_srbrecr_seqno, 'SLATE');
        END IF;   
      EXCEPTION
        WHEN no_srbrecr_row THEN
          v_error_mesg :=  'Unable to start processing contact codes for Banner pidm '|| v_pidm || ' because SRBRECR record for associated term is not found.' ; 
          p_handle_err(v_szscont_rec.szscont_guid, v_error_mesg, v_proc_name);
      
      END;
    END LOOP;
    CLOSE szscont_c;
  END p_import_pers_contacts;
  ---------
  PROCEDURE p_import_appl_interests
  IS
    szsints_rec  szsints%ROWTYPE;
    v_szsints_code  VARCHAR2(20);
    v_pidm          spriden.spriden_pidm%TYPE;
    v_guid          szsints.szsints_guid%TYPE;
    v_term_code     szsints.szsints_term_code%TYPE;
    v_ctyp_code     sorcont.sorcont_ctyp_code%TYPE;
    v_ints_code     sorints.sorints_ints_code%TYPE;
    v_appl_no       saradap.saradap_appl_no%TYPE;
    v_appl_date     saradap.saradap_appl_date%TYPE;
    v_admt_code     saradap.saradap_admt_code%TYPE;
    v_atts_code     saraatt.saraatt_atts_code%TYPE;
    v_appl_term     stvterm.stvterm_code%TYPE;
    v_appl_max_yr   VARCHAR2(4);
    v_appl_min_yr   VARCHAR2(4);
    v_error_mesg    VARCHAR2(200);
    v_saradap_cnt   NUMBER;
    v_no_saradap    EXCEPTION;
    v_multiple_saradap    EXCEPTION;
    v_proc_name     VARCHAR2(30) := 'p_import_appl_interests';
    CURSOR szsints_c
    IS 
      SELECT szsints.* FROM szsints 
      JOIN goradid on szsints_guid = goradid_additional_id
      WHERE goradid_adid_code  = 'SL'    
      AND NOT EXISTS 
        (SELECT goradid_additional_id 
        FROM goradid 
        WHERE goradid_additional_id = szsints_guid
        AND goradid_adid_code = 'SL'
        GROUP BY goradid_additional_id HAVING COUNT(*) >1)
      AND NOT EXISTS
        (SELECT goradid_pidm
        FROM goradid
        WHERE goradid_pidm = szsints_pidm
        AND goradid_adid_code = 'SL' 
        GROUP BY goradid_pidm HAVING COUNT(*) >1);
  BEGIN
    IF szsints_c%ISOPEN
    THEN
      CLOSE szsints_c;
    END IF;
    OPEN szsints_c;
    LOOP
      FETCH szsints_c INTO szsints_rec;
      EXIT WHEN szsints_c%NOTFOUND;
      v_pidm := f_get_pidm_from_guid(szsints_rec.szsints_guid, 'SL');
      v_guid := szsints_rec.szsints_guid;
      v_term_code := szsints_rec.szsints_term_code;
      BEGIN
        SELECT COUNT(*) 
          INTO v_saradap_cnt
          FROM saradap
         WHERE saradap_pidm = v_pidm
            AND saradap_term_code_entry = v_term_code;  
        IF v_saradap_cnt > 1 
        THEN 
          raise v_multiple_saradap;
        END IF;
        IF v_saradap_cnt = 0
        THEN
          raise v_no_saradap;
        END IF;
        
        SELECT saradap_appl_no, saradap_appl_date, saradap_admt_code
          INTO v_appl_no, v_appl_date, v_admt_code
          FROM saradap
         WHERE saradap_pidm = v_pidm
           AND saradap_term_code_entry = v_term_code;
        
        ------------------------------------------
        --SORCONT
        ------------------------------------------
        -------- Phi Theta Kappa Select ---------
        IF szsints_rec.szsints_apt IS NOT NULL
        THEN
          v_error_mesg := NULL;
          v_ctyp_code := 'APT';
          p_load_sorcont(v_pidm, v_ctyp_code, 'SLATE', v_error_mesg);
          IF v_error_mesg IS NOT NULL
          THEN
            p_handle_err(v_guid, v_error_mesg, v_proc_name);
          END IF;
        END IF;
        ----------- Phi Theta Kappa -------------
        ---bit wise: so some could be 0
        IF szsints_rec.szsints_apk = '1'
        THEN
          v_error_mesg := NULL;
          v_ctyp_code := 'APK';
          p_load_sorcont(v_pidm, v_ctyp_code, 'SLATE', v_error_mesg);
          IF v_error_mesg IS NOT NULL
          THEN
            p_handle_err(v_guid, v_error_mesg, v_proc_name);
          END IF;      
        END IF;
        ------------- Trustee Grant -------------
        --bit wise
        IF szsints_rec.szsints_atg = '1'
        THEN
          v_error_mesg := NULL;
          v_ctyp_code := 'ATG';
          p_load_sorcont(v_pidm, v_ctyp_code, 'SLATE', v_error_mesg);
          IF v_error_mesg IS NOT NULL
          THEN
            p_handle_err(v_guid, v_error_mesg, v_proc_name);
          END IF;           
        END IF;
        -------------- Posse Admit --------------
        IF szsints_rec.szsints_po2 IS NOT NULL
        THEN
          v_error_mesg := NULL;
          v_ctyp_code := 'PO2';
          p_load_sorcont(v_pidm, v_ctyp_code, 'SLATE', v_error_mesg);
          IF v_error_mesg IS NOT NULL
          THEN
            p_handle_err(v_guid, v_error_mesg, v_proc_name);
          END IF;      
        END IF;

        --------  Smith in Europe Travel Grant  ---------
        IF szsints_rec.szsints_ase IS NOT NULL
        THEN
          v_error_mesg := NULL;
          v_ctyp_code := 'ASE';
          p_load_sorcont(v_pidm, v_ctyp_code, 'SLATE', v_error_mesg);
          IF v_error_mesg IS NOT NULL
          THEN
            p_handle_err(v_guid, v_error_mesg, v_proc_name);
          END IF;
        END IF;
        --------  Local Area Grant  ---------
        IF szsints_rec.szsints_are = '1'
        THEN
          v_error_mesg := NULL;
          v_ctyp_code := 'ARE';
          p_load_sorcont(v_pidm, v_ctyp_code, 'SLATE', v_error_mesg);
          IF v_error_mesg IS NOT NULL
          THEN
            p_handle_err(v_guid, v_error_mesg, v_proc_name);
          END IF;
        END IF;
        ------------------------------------------
        --SORINTS
        ------------------------------------------
        ------------ Coulter Scholar ------------
        IF szsints_rec.szsints_cs IS NOT NULL
        THEN
          v_error_mesg := NULL;
          v_ints_code := 'CS';
          p_load_sorints(v_pidm, v_ints_code, 'SLATE', v_error_mesg);
          IF v_error_mesg IS NOT NULL
          THEN
            p_handle_err(v_guid, v_error_mesg, v_proc_name);
          END IF;           
        END IF;
        ----- Davis Scholar Boarding School -----
        IF szsints_rec.szsints_db IS NOT NULL
        THEN
          v_error_mesg := NULL;
          v_ints_code := 'DB';
          p_load_sorints(v_pidm, v_ints_code, 'SLATE', v_error_mesg);
          IF v_error_mesg IS NOT NULL
          THEN
            p_handle_err(v_guid, v_error_mesg, v_proc_name);
          END IF;      
        END IF;
        -------------- Davis Scholar -------------
        IF szsints_rec.szsints_dv IS NOT NULL
        THEN
          v_error_mesg := NULL;
          v_ints_code := 'DV';
          p_load_sorints(v_pidm, v_ints_code,'SLATE', v_error_mesg);
          IF v_error_mesg IS NOT NULL
          THEN
            p_handle_err(v_guid, v_error_mesg, v_proc_name);
          END IF;      
        END IF; 
      
        -------------- Financial Aid -------------
        --bit wise
        IF szsints_rec.szsints_fa = '1'
        THEN
          v_error_mesg := NULL;
          v_ints_code := 'FA';
          p_load_sorints(v_pidm, v_ints_code,'SLATE', v_error_mesg);
          IF v_error_mesg IS NOT NULL
          THEN
            p_handle_err(v_guid, v_error_mesg, v_proc_name);
          END IF;      
        END IF; 
        
        -------------- First Generation -------------
        --bit wise
        IF szsints_rec.szsints_fg = '1'
        THEN
          v_error_mesg := NULL;
          v_ints_code := 'FG';
          p_load_sorints(v_pidm, v_ints_code,'SLATE', v_error_mesg);
          IF v_error_mesg IS NOT NULL
          THEN
            p_handle_err(v_guid, v_error_mesg, v_proc_name);
          END IF;      
        END IF; 
        -------------- Undocumented -------------
        IF szsints_rec.szsints_ud IS NOT NULL
        THEN
          v_error_mesg := NULL;
          v_ints_code := 'UD';
          p_load_sorints(v_pidm, v_ints_code,'SLATE', v_error_mesg);
          IF v_error_mesg IS NOT NULL
          THEN
            p_handle_err(v_guid, v_error_mesg, v_proc_name);
          END IF;      
        END IF; 
        
        IF UPPER(v_admt_code) = 'TS'
        THEN
          -------------- Round 1: 7/1 - 2/1-------------
          v_appl_term := f_term_on_date('SM', sysdate);
          
          IF SUBSTR(v_appl_term, 5, 2) = '04' THEN 
            --handle case when sysdate is in the summer
            --since for summer f_term_on_date returned the year rounded down
           
            v_appl_min_yr := SUBSTR(v_appl_term, 1, 4);
            v_appl_max_yr := TO_CHAR(TO_NUMBER(v_appl_min_yr) + 1);
            ------------------------------------------------------
          ELSE 
            v_appl_max_yr := SUBSTR(v_appl_term, 1, 4);
            v_appl_min_yr := TO_CHAR(TO_NUMBER(v_appl_max_yr) - 1);
          END IF;
    
          
          IF trunc(v_appl_date) >= to_date('07/01/'|| v_appl_min_yr, 'MM/DD/YYYY') AND
             trunc(v_appl_date) <= to_date('02/01/'|| v_appl_max_yr, 'MM/DD/YYYY') 
          THEN
            v_error_mesg := NULL;
            v_ints_code := 'R1';
            p_load_sorints(v_pidm, v_ints_code,'SLATE', v_error_mesg);
            IF v_error_mesg IS NOT NULL
            THEN
              p_handle_err(v_guid, v_error_mesg, v_proc_name);
            END IF;      
          END IF; 
          -------------- Round 2: 2/2 to 5/15 -------------
          
          IF trunc(v_appl_date) >= to_date('02/02/'|| v_appl_max_yr, 'MM/DD/YYYY') AND
             trunc(v_appl_date) <= to_date('05/15/'|| v_appl_max_yr, 'MM/DD/YYYY')
    
          THEN
            v_error_mesg := NULL;
            v_ints_code := 'R2';
            p_load_sorints(v_pidm, v_ints_code,'SLATE', v_error_mesg);
            IF v_error_mesg IS NOT NULL
            THEN
              p_handle_err(v_guid, v_error_mesg, v_proc_name);
            END IF;      
          END IF;
        END IF;
        ---------------Early Decision-----------------------
      /*  IF szsints_rec.szsints_eb IS NOT NULL
        THEN
          v_error_mesg := NULL;
          v_ints_code := 'EB';
          p_load_sorints(v_pidm, v_ints_code,'SLATE', v_error_mesg);
          IF v_error_mesg IS NOT NULL
          THEN
            p_handle_err(v_guid, v_error_mesg, v_proc_name);
          END IF;      
        END IF; */
        ------------------------------------------
        --SARAATT
        ------------------------------------------
        -------------- AEMES scholar -------------      
        IF szsints_rec.szsints_ames IS NOT NULL
        THEN
          v_error_mesg := NULL;
          v_atts_code := 'AMES';
          p_load_saraatt(v_pidm, v_term_code, v_appl_no, v_atts_code,'SLATE', v_error_mesg);
          IF v_error_mesg IS NOT NULL
          THEN
            p_handle_err(v_guid, v_error_mesg, v_proc_name);
          END IF;      
        END IF;
        -----Greenfield Comm. Col. Scholar-------
        IF szsints_rec.szsints_gccs IS NOT NULL
        THEN
          v_error_mesg := NULL;
          v_atts_code := 'GCCS';
          p_load_saraatt(v_pidm, v_term_code, v_appl_no, v_atts_code,'SLATE', v_error_mesg);
          IF v_error_mesg IS NOT NULL
          THEN
            p_handle_err(v_guid, v_error_mesg, v_proc_name);
          END IF;        
        END IF;
        
        -------Holyoke Comm. Col. Scholar---------
        IF szsints_rec.szsints_hccs IS NOT NULL
        THEN
          v_error_mesg := NULL;
          v_atts_code := 'HCCS';
          p_load_saraatt(v_pidm, v_term_code, v_appl_no, v_atts_code,'SLATE', v_error_mesg);
          IF v_error_mesg IS NOT NULL
          THEN
            p_handle_err(v_guid, v_error_mesg, v_proc_name);
          END IF; 
        END IF;
        
        
        ---------Presidential Scholarship---------
        IF szsints_rec.szsints_prsc IS NOT NULL
        THEN
          v_error_mesg := NULL;
          v_atts_code := 'PRSC';
          p_load_saraatt(v_pidm, v_term_code, v_appl_no, v_atts_code,'SLATE', v_error_mesg);
          IF v_error_mesg IS NOT NULL
          THEN
            p_handle_err(v_guid, v_error_mesg, v_proc_name);
          END IF; 
        END IF;
        
        ---------Springfield Partnership---------
        
        IF szsints_rec.szsints_spfd IS NOT NULL
        THEN
          v_error_mesg := NULL;
          v_atts_code := 'SPFD';
          p_load_saraatt(v_pidm, v_term_code, v_appl_no, v_atts_code,'SLATE', v_error_mesg);
          IF v_error_mesg IS NOT NULL
          THEN
            p_handle_err(v_guid, v_error_mesg, v_proc_name);
          END IF; 
        END IF;
        
        ---------------STRIDE--------------------
        
        IF szsints_rec.szsints_strd IS NOT NULL
        THEN
          v_error_mesg := NULL;
          v_atts_code := 'STRD';
          p_load_saraatt(v_pidm, v_term_code, v_appl_no, v_atts_code,'SLATE', v_error_mesg);
          IF v_error_mesg IS NOT NULL
          THEN
            p_handle_err(v_guid, v_error_mesg, v_proc_name);
          END IF; 
        END IF;
        
        ---------------Zollman--------------------
        
        IF szsints_rec.szsints_zoll IS NOT NULL
        THEN
          v_error_mesg := NULL;
          v_atts_code := 'ZOLL';
          p_load_saraatt(v_pidm, v_term_code, v_appl_no, v_atts_code,'SLATE', v_error_mesg);
          IF v_error_mesg IS NOT NULL
          THEN
            p_handle_err(v_guid, v_error_mesg, v_proc_name);
          END IF; 
        END IF;  
       
      EXCEPTION
        WHEN v_multiple_saradap THEN
          v_error_mesg := 'Unable to start processing interest codes for Banner pidm '|| v_pidm || ' because there are more than one application records found in term ' || v_term_code || '. ' ;     
          p_handle_err(v_guid, v_error_mesg, v_proc_name);
        WHEN v_no_saradap THEN 
          v_error_mesg := 'Unable to start processing interest codes for Banner pidm '|| v_pidm || ' because there are no application record found in term ' || v_term_code || '. ' ;     
          p_handle_err(v_guid, v_error_mesg, v_proc_name);
        WHEN OTHERS THEN
          v_error_mesg := 'Unable to start processing interest codes for Banner pidm ' || v_pidm || '. ' || SUBSTR (TO_CHAR (SQLCODE) || ' ' || SQLERRM, 1, 300);
          p_handle_err(v_guid, v_error_mesg, v_proc_name);
      END;
    END LOOP;
    CLOSE szsints_c;
  END p_import_appl_interests;
  ----------------
  PROCEDURE p_import_applications_schools
  IS
  BEGIN
    p_import_applicants;
    p_import_appl_schools;
  END p_import_applications_schools;
  ----------------
  PROCEDURE p_email_error
  IS 
    l_message        CLOB;
    l_count          NUMBER:=0;      
    v_email          atowner.send_email.email_con;
    v_mailto         atowner.send_email.email_address;
    v_mailfrom       atowner.send_email.email_address;
    v_mailcc         atowner.send_email.email_address;
    v_mailsubject    VARCHAR2(200)   := '';
    v_mailmessage    VARCHAR2(32000)   := '';
    v_db             VARCHAR2(30);
    CURSOR sdlperr_all_c 
    IS 
      SELECT sdlperr_guid,
             sdlperr_error,
             sdlperr_data_origin
        FROM sdlperr
       WHERE TRUNC(sdlperr_activity_date) = TRUNC(SYSDATE) ;

    err_rec    sdlperr_all_c%ROWTYPE; 
   
  BEGIN 
    --Get DB environment
    SELECT ora_database_name 
    INTO v_db
    FROM dual;
    IF v_db = 'PROD.WORLD'
    THEN
      v_db := '**PROD** ';
    ELSIF v_db = 'DEVX.WORLD'
    THEN
      v_db := '**DEVX** ';
    ELSIF v_db = 'PPOD.WORLD'
    THEN
      v_db := '**PPOD** ';
    END IF;
    
    
    /* Check for open cursor */
    IF sdlperr_all_c%ISOPEN
    THEN
      CLOSE sdlperr_all_c;
    END IF;
    
    OPEN sdlperr_all_c;
    LOOP 
      FETCH sdlperr_all_c INTO err_rec; 
      EXIT WHEN sdlperr_all_c%NOTFOUND;
        
      IF sdlperr_all_c%ISOPEN 
      THEN 
        l_message := l_message || ( 
                'Slate Ref ID#: '||err_rec.sdlperr_guid ||chr(10)||chr(13)||
                'Error message: '||err_rec.sdlperr_error ||chr(10)||chr(13)||
                'Process name: '||err_rec.sdlperr_data_origin||chr(10)||chr(13))||       
                '================================================='||chr(10)||chr(13);
        l_count := l_count+1;
      END IF;
    END LOOP; 
    CLOSE sdlperr_all_c;     
      
    IF l_count > 0 
    THEN   
      v_mailfrom.email  :=  'tnguyen@smith.edu';
      v_mailsubject   :=  v_db || 'Error with import Slate data on '||to_char(sysdate, 'MM/DD/YYYY') ;
      
      v_mailto.email  := 'jdrawe@smith.edu';
      v_mailcc.email   := 'kmessier@smith.edu';
      atowner.send_email.MESSAGE(
                v_email
              , v_mailto
              , v_mailfrom
              , v_mailcc 
              , v_mailsubject
              , SUBSTR(l_message, 1, 18000)
              );
      atowner.send_email.message_end(v_email);
      --send to AT too to troubleshoot
      v_mailto.email  := 'tnguyen@smith.edu';
      v_mailcc.email   := 'kkatamay@smith.edu';
      atowner.send_email.MESSAGE(
                v_email
              , v_mailto
              , v_mailfrom
              , v_mailcc 
              , v_mailsubject
              , SUBSTR(l_message, 1, 18000)
              );
      atowner.send_email.message_end(v_email);
    END IF; 
  END p_email_error;
  -----------
  --OVERLOAD 
  PROCEDURE p_email_error(
    p_proc_name      VARCHAR2)
  IS 
    l_message        CLOB;
    l_count          NUMBER:=0;      
    v_email          atowner.send_email.email_con;
    v_mailto         atowner.send_email.email_address;
    v_mailfrom       atowner.send_email.email_address;
    v_mailcc         atowner.send_email.email_address;
    v_mailsubject    VARCHAR2(200)   := '';
    v_mailmessage    VARCHAR2(32000)   := '';
    v_db             VARCHAR2(30);

    CURSOR sdlperr_by_proc_c
    IS
      SELECT sdlperr_guid,
             sdlperr_error,
             sdlperr_data_origin
        FROM sdlperr
       WHERE TRUNC(sdlperr_activity_date) = TRUNC(SYSDATE)
         AND sdlperr_data_origin = p_proc_name;
          
    err_rec    sdlperr_by_proc_c%ROWTYPE; 
   
  BEGIN 
    --Get DB environment
    SELECT ora_database_name 
    INTO v_db
    FROM dual;
    IF v_db = 'PROD.WORLD'
    THEN
      v_db := '**PROD** ';
    ELSIF v_db = 'DEVX.WORLD'
    THEN
      v_db := '**DEVX** ';
    ELSIF v_db = 'PPOD.WORLD'
    THEN
      v_db := '**PPOD** ';
    END IF;
    
    
    /* Check for open cursor */
    IF p_proc_name IS NOT NULL
    THEN    
      IF sdlperr_by_proc_c%ISOPEN
      THEN
        CLOSE sdlperr_by_proc_c;
      END IF;
    
      OPEN sdlperr_by_proc_c;
      LOOP 
        FETCH sdlperr_by_proc_c INTO err_rec; 
        EXIT WHEN sdlperr_by_proc_c%NOTFOUND;
        
        IF sdlperr_by_proc_c%ISOPEN 
        THEN 
          l_message := l_message || ( 
                'Slate Ref ID#: '||err_rec.sdlperr_guid ||chr(10)||chr(13)||
                'Error message: '||err_rec.sdlperr_error ||chr(10)||chr(13))||       
                '================================================='||chr(10)||chr(13);
          l_count := l_count+1;
        END IF;
      END LOOP; 
      CLOSE sdlperr_by_proc_c;     
      
      IF l_count > 0 
      THEN   
        v_mailfrom.email  :=  'tnguyen@smith.edu';
        v_mailsubject   :=  v_db || 'Error with '|| p_proc_name || ' on '||to_char(sysdate, 'MM/DD/YYYY') ;
        
        v_mailto.email  := 'jdrawe@smith.edu';
        v_mailcc.email   := 'kmessier@smith.edu';
        atowner.send_email.MESSAGE(
                  v_email
                , v_mailto
                , v_mailfrom
                , v_mailcc 
                , v_mailsubject
                , SUBSTR(l_message, 1, 18000)
                );
        atowner.send_email.message_end(v_email);
        --send to AT too to troubleshoot
        v_mailto.email  :=  'tnguyen@smith.edu';
        v_mailcc.email   := 'kkatamay@smith.edu';
        atowner.send_email.MESSAGE(
                  v_email
                , v_mailto
                , v_mailfrom
                , v_mailcc 
                , v_mailsubject
                , SUBSTR(l_message, 1, 18000)
                );
        atowner.send_email.message_end(v_email);
      END IF; 
    END IF;
  END p_email_error;
  -----------
  PROCEDURE p_write_error_log
  IS 
    l_message       VARCHAR2(1000);
    l_count         NUMBER:=0;      
    outfile_loc     VARCHAR2 (40)       :='BANJOBS_DIR';
    output_file     UTL_FILE.file_type;
    outfile_name    VARCHAR2(50);
    v_db            VARCHAR2(30);
    CURSOR sdlperr_c 
    IS 
      SELECT sdlperr_guid,
             sdlperr_error,
             sdlperr_data_origin
        FROM sdlperr
       WHERE TRUNC(sdlperr_activity_date) = TRUNC(SYSDATE) ;

    sdlperr_rec    sdlperr_c%ROWTYPE; 
   
  BEGIN 
    --Get DB environment
    SELECT ora_database_name 
    INTO v_db
    FROM dual;
    IF v_db = 'PROD.WORLD'
    THEN
      v_db := '**PROD** ';
    ELSIF v_db = 'DEVX.WORLD'
    THEN
      v_db := '**DEVX** ';
    ELSIF v_db = 'PPOD.WORLD'
    THEN
      v_db := '**PPOD** ';
    END IF;
    /* Check for open cursor */
    IF sdlperr_c%ISOPEN
    THEN
      CLOSE sdlperr_c;
    END IF;
    outfile_name := 'importSlateErr.txt';
    output_file := UTL_FILE.fopen(outfile_loc,outfile_name, 'W');
    UTL_FILE.put_line (output_file, v_db || 'Error Log from import_slate_data_2_pkg Process');
    UTL_FILE.put_line (output_file, to_char(sysdate, 'MM/DD/YYYY'));
    UTL_FILE.put_line (output_file, 'Starting Log....');
    OPEN sdlperr_c;
    LOOP 
        FETCH sdlperr_c INTO sdlperr_rec; 
        EXIT WHEN sdlperr_c%NOTFOUND;
        UTL_FILE.put_line (output_file, 'Slate Ref ID#: '||sdlperr_rec.sdlperr_guid); 
        UTL_FILE.put_line (output_file, 'Error message: '||sdlperr_rec.sdlperr_error);
        UTL_FILE.put_line (output_file, 'Process name: '||sdlperr_rec.sdlperr_data_origin);
        UTL_FILE.put_line (output_file, '=================================================');
  
        l_count := l_count+1;
      
    END LOOP; 
    CLOSE sdlperr_c;   
    UTL_FILE.put_line (output_file, 'End Log.');
    UTL_FILE.fclose_all; 
  END p_write_error_log;
  -----------
  PROCEDURE p_write_error_log(
    p_proc_name      VARCHAR2)
  IS 
    l_message       VARCHAR2(1000);
    l_count         NUMBER:=0;      
    outfile_loc     VARCHAR2 (40)       :='BANJOBS_DIR';
    output_file     UTL_FILE.file_type;
    outfile_name    VARCHAR2(50);
    v_db            VARCHAR2(30);

    CURSOR sdlperr_by_proc_c
    IS
      SELECT sdlperr_guid,
             sdlperr_error,
             sdlperr_data_origin
        FROM sdlperr
       WHERE TRUNC(sdlperr_activity_date) = TRUNC(SYSDATE)
         AND sdlperr_data_origin = p_proc_name;
         
    sdlperr_rec    sdlperr_by_proc_c%ROWTYPE; 
   
  BEGIN 
    --Get DB environment
    SELECT ora_database_name 
    INTO v_db
    FROM dual;
    IF v_db = 'PROD.WORLD'
    THEN
      v_db := '**PROD** ';
    ELSIF v_db = 'DEVX.WORLD'
    THEN
      v_db := '**DEVX** ';
    ELSIF v_db = 'PPOD.WORLD'
    THEN
      v_db := '**PPOD** ';
    END IF;
    /* Check for open cursor */
    IF p_proc_name IS NOT NULL THEN
      IF sdlperr_by_proc_c%ISOPEN
      THEN
        CLOSE sdlperr_by_proc_c;
      END IF;
      outfile_name := 'importContactsErr.txt';
      output_file := UTL_FILE.fopen(outfile_loc,outfile_name, 'W');
      UTL_FILE.put_line (output_file, v_db || 'Error Log from ' || p_proc_name || ' procedure');
      UTL_FILE.put_line (output_file, 'Starting Log....');
      OPEN sdlperr_by_proc_c;
      LOOP 
        FETCH sdlperr_by_proc_c INTO sdlperr_rec; 
        EXIT WHEN sdlperr_by_proc_c%NOTFOUND;
        UTL_FILE.put_line (output_file, 'Slate Ref ID#: '||sdlperr_rec.sdlperr_guid); 
        UTL_FILE.put_line (output_file, 'Error message: '||sdlperr_rec.sdlperr_error);
        UTL_FILE.put_line (output_file, '=================================================');
  
        l_count := l_count+1;
      
      END LOOP; 
      CLOSE sdlperr_by_proc_c;   
      UTL_FILE.put_line (output_file, 'End Log.');
      UTL_FILE.fclose_all;   
    END IF;
  END p_write_error_log;
  --------
  PROCEDURE p_get_srrprel_err(
    p_file_loc VARCHAR2,
    p_file_name VARCHAR2
    )
  IS
    v_guid          srtiden.srtiden_additional_id%TYPE;
    output_file     UTL_FILE.file_type;
    v_output_line   VARCHAR2(700);
    v_error_mesg    VARCHAR2(700); 
    v_prospect_id   VARCHAR2(20);
  BEGIN
    output_file := utl_file.fopen(p_file_loc, p_file_name, 'R');
    LOOP
      BEGIN
        utl_file.get_line(output_file, v_output_line);
  
        IF v_output_line LIKE '%Prospect ID:%' --contains Prospect ID:
        THEN
          v_error_mesg := TRIM(BOTH ' ' FROM v_output_line);
          v_prospect_id := SUBSTR(v_error_mesg, INSTR(v_error_mesg, 'Prospect ID:', 1) + 13, 9) ; 
          BEGIN
            SELECT srtiden_additional_id 
            INTO v_guid
            FROM srtiden 
            WHERE srtiden_id = v_prospect_id
            AND srtiden_adid_code = 'SL';
          EXCEPTION
            WHEN NO_DATA_FOUND THEN
              v_guid := 'UNKNOWN';
          END;
          atowner.import_slate_data_2_pkg.p_handle_err(v_guid, v_error_mesg, 'SRRPREL');
          
        END IF;
      EXCEPTION
        WHEN NO_DATA_FOUND THEN
          EXIT;
      END;
    END LOOP;
    utl_file.fclose(output_file);

  END p_get_srrprel_err;
  ------------
  PROCEDURE p_email_multi_guids_err(
    p_adid_code     VARCHAR2)
  IS
    l_message        CLOB;
    l_count          NUMBER:=0;      
    v_email          atowner.send_email.email_con;
    v_mailto         atowner.send_email.email_address;
    v_mailfrom       atowner.send_email.email_address;
    v_mailcc         atowner.send_email.email_address;
    v_mailsubject    VARCHAR2(200)   := '';
    v_mailmessage    VARCHAR2(32000)   := '';
    v_db             VARCHAR2(30);
    
   --Many Slate Ref ID to PIDM/ID
    CURSOR multiple_guids_c
    IS
      SELECT 
        spriden_id AS banner_id, 
        spriden_pidm As banner_pidm, 
        goradid_additional_id AS slate_refid,
        spriden_last_name || ', ' || spriden_first_name || ' ' || spriden_mi AS full_name, 
        goradid_user_id AS user_id, 
        goradid_activity_date AS activity_date
      FROM goradid 
      JOIN spriden ON goradid_pidm = spriden_pidm 
      WHERE goradid_pidm IN 
        (SELECT goradid_pidm 
           FROM goradid 
          WHERE goradid_adid_code = p_adid_code
          GROUP BY goradid_pidm HAVING COUNT(*) >1) 
      AND goradid_adid_code = p_adid_code 
      AND spriden_change_ind is null
      ORDER BY goradid_pidm ASC, goradid_activity_date asc ;
    err_rec     multiple_guids_c%ROWTYPE;
    v_count NUMBER;
  
  BEGIN
    SELECT ora_database_name 
    INTO v_db
    FROM dual;
    IF v_db = 'PROD.WORLD'
    THEN
      v_db := '**PROD** ';
    ELSIF v_db = 'DEVX.WORLD'
    THEN
      v_db := '**DEVX** ';
    ELSIF v_db = 'PPOD.WORLD'
    THEN
      v_db := '**PPOD** ';
    END IF;
    --Many Slate Ref ID to PIDM/ID
    v_count := 0;
    IF multiple_guids_c%ISOPEN
    THEN
      CLOSE multiple_guids_c;
    END IF;
    OPEN multiple_guids_c;
    LOOP
      FETCH multiple_guids_c INTO err_rec;
      EXIT WHEN multiple_guids_c%NOTFOUND;
      l_message := l_message || ( 
                  'Slate Ref ID#: '||err_rec.slate_refid ||chr(10)||chr(13)||
                  'Full Name: '||err_rec.full_name ||chr(10)||chr(13)||
                  'Banner ID: '||err_rec.banner_id||chr(10)||chr(13)||
                  'Banner PIDM: '||err_rec.banner_pidm||chr(10)||chr(13)||    
                  'By User: '||err_rec.user_id||chr(10)||chr(13)|| 
                  'Time Loaded: '||err_rec.activity_date||chr(10)||chr(13))||  
                  '================================================='||chr(10)||chr(13);
      v_count := v_count + 1;
      
    END LOOP;
    CLOSE multiple_guids_c;
    IF v_count > 0
    THEN
      v_mailfrom.email  :=  'tnguyen@smith.edu';
      v_mailsubject   :=  v_db || 'Please Resolve Multiple Slate IDs Mapped to Banner ID'||to_char(sysdate, 'MM/DD/YYYY') ;
        
      v_mailto.email  := 'jdrawe@smith.edu';
      v_mailcc.email   :='kmessier@smith.edu';
      atowner.send_email.MESSAGE(
                  v_email
                , v_mailto
                , v_mailfrom
                , v_mailcc 
                , v_mailsubject
                , SUBSTR(l_message, 1, 18000)
                );
      atowner.send_email.message_end(v_email);
      --send to AT too to troubleshoot
      v_mailto.email  := 'tnguyen@smith.edu';
      v_mailcc.email   := 'kkatamay@smith.edu';
      atowner.send_email.MESSAGE(
                v_email
              , v_mailto
              , v_mailfrom
              , v_mailcc 
              , v_mailsubject
              , SUBSTR(l_message, 1, 18000)
              );
      atowner.send_email.message_end(v_email);
    END IF;
  END p_email_multi_guids_err;
  ----------
  PROCEDURE p_email_multi_pidms_err(
    p_adid_code     VARCHAR2)
  IS
    l_message        CLOB;
    l_count          NUMBER:=0;      
    v_email          atowner.send_email.email_con;
    v_mailto         atowner.send_email.email_address;
    v_mailfrom       atowner.send_email.email_address;
    v_mailcc         atowner.send_email.email_address;
    v_mailsubject    VARCHAR2(200)   := '';
    v_mailmessage    VARCHAR2(32000)   := '';
    v_db             VARCHAR2(30);
    --Many PIDM/ID TO ONE Slate Ref ID
    CURSOR multiple_pidms_c
    IS
      SELECT 
        spriden_id AS banner_id, 
        spriden_pidm As banner_pidm, 
        goradid_additional_id AS slate_refid,
        spriden_last_name || ', ' || spriden_first_name || ' ' ||  spriden_mi AS full_name, 
        goradid_user_id AS user_id, 
        goradid_activity_date AS activity_date 
      FROM goradid 
      JOIN spriden ON goradid_pidm = spriden_pidm 
      WHERE goradid_additional_id IN 
        (SELECT goradid_additional_id 
          FROM goradid 
          WHERE goradid_adid_code = p_adid_code
         GROUP BY goradid_additional_id HAVING COUNT(*) >1) 
      AND goradid_adid_code = p_adid_code
      AND spriden_change_ind is null
      ORDER BY goradid_additional_id ASC, goradid_activity_date asc;
      
    err_rec     multiple_pidms_c%ROWTYPE;
    v_count     NUMBER;
  BEGIN

    SELECT ora_database_name 
    INTO v_db
    FROM dual;
    IF v_db = 'PROD.WORLD'
    THEN
      v_db := '**PROD** ';
    ELSIF v_db = 'DEVX.WORLD'
    THEN
      v_db := '**DEVX** ';
    ELSIF v_db = 'PPOD.WORLD'
    THEN
      v_db := '**PPOD** ';
    END IF;
      
    --Many PIDM/ID TO ONE Slate Ref ID
    v_count := 0;
    IF multiple_pidms_c%ISOPEN
    THEN
      CLOSE multiple_pidms_c;
    END IF;
    OPEN multiple_pidms_c;
    LOOP
      FETCH multiple_pidms_c INTO err_rec;
      EXIT WHEN multiple_pidms_c%NOTFOUND;
      l_message := l_message || ( 
                  'Slate Ref ID#: '||err_rec.slate_refid ||chr(10)||chr(13)||
                  'Full Name: '||err_rec.full_name ||chr(10)||chr(13)||
                  'Banner ID: '||err_rec.banner_id||chr(10)||chr(13)||
                  'Banner PIDM: '||err_rec.banner_pidm||chr(10)||chr(13)||    
                  'By User: '||err_rec.user_id||chr(10)||chr(13)|| 
                  'Time Loaded: '||err_rec.activity_date||chr(10)||chr(13))||  
                  '================================================='||chr(10)||chr(13);
      v_count := v_count + 1;
      
    END LOOP;
    CLOSE multiple_pidms_c;
    IF v_count > 0
    THEN
      v_mailfrom.email  :=  'tnguyen@smith.edu';
      v_mailsubject   :=  v_db || 'Please Resolve Multiple Banner IDs Mapped to Same Slate ID '||to_char(sysdate, 'MM/DD/YYYY') ;
        
      v_mailto.email  := 'jdrawe@smith.edu';
      v_mailcc.email   :='kmessier@smith.edu';
      atowner.send_email.MESSAGE(
                  v_email
                , v_mailto
                , v_mailfrom
                , v_mailcc 
                , v_mailsubject
                , SUBSTR(l_message, 1, 18000)
                );
      atowner.send_email.message_end(v_email); 
      --send to AT too to troubleshoot
      v_mailto.email  := 'tnguyen@smith.edu';
      v_mailcc.email   := 'kkatamay@smith.edu';
      atowner.send_email.MESSAGE(
                v_email
              , v_mailto
              , v_mailfrom
              , v_mailcc 
              , v_mailsubject
              , SUBSTR(l_message, 1, 18000)
              );
      atowner.send_email.message_end(v_email);
    END IF;
  END p_email_multi_pidms_err;
  ----------

  PROCEDURE p_run_email_procedures
  IS
  BEGIN
    p_email_multi_guids_err('SL');
    p_email_multi_pidms_err('SL');
    p_email_error;
  END p_run_email_procedures;
END import_slate_data_2_pkg;
