*****           Implementation of object type ZIMAGE               *****
INCLUDE <object>.
begin_data object. " Do not change.. DATA is generated
* only private members may be inserted into structure private
DATA:
  " begin of private,
  "   to declare private attributes remove comments and
  "   insert private attributes here ...
  " end of private,
  BEGIN OF key,
    imagelinkarchiveid LIKE toav0-archiv_id,
    archivedocid       LIKE toav0-arc_doc_id,
  END OF key,
  description TYPE toasp-objecttext.
end_data object. " Do not change.. DATA is generated

get_property description changing container.
DATA: lt_doc_links TYPE cl_alink_connection=>toav0_tab,
      lt_files     TYPE dms_tbl_file,
      ls_files     TYPE dms_rec_phio,
      lv_file      TYPE filep.

" DMS-Objekt ermitteln
SELECT FROM dms_ph_cd1 AS cd1                          " Logisches Dokument
            INNER JOIN dms_doc2loio AS lo              " DMS-Objekt
                    ON lo~lo_objid EQ cd1~loio_id
     FIELDS lo~dokar,  lo~doknr,  lo~dokvr,  lo~doktl
      WHERE cd1~phio_id  EQ @object-key-archivedocid
       INTO @DATA(ls_dms_doc2loio)
            UP TO 1 ROWS.
ENDSELECT.

" Dokumente zum DMS-Objekt lesen
CALL FUNCTION 'CV120_KPRO_MASTER_DATA_GET'
  EXPORTING
    pf_dokar  = ls_dms_doc2loio-dokar
    pf_doknr  = ls_dms_doc2loio-doknr
    pf_dokvr  = ls_dms_doc2loio-dokvr
    pf_doktl  = ls_dms_doc2loio-doktl
  TABLES
    ptx_data  = lt_files
  EXCEPTIONS
    not_found = 1
    error     = 2
    OTHERS    = 3.

IF sy-subrc EQ 0.
  LOOP AT lt_files INTO DATA(ls_main_file) WHERE updateflag <> 'D'.
    READ TABLE ls_main_file-tbl_phios INTO ls_files
    WITH KEY active_version = 'X'
             default_langu  = 'X'
             ph_objid       = object-key-archivedocid.

    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    IF ls_main_file-description IS NOT INITIAL.
      object-description = ls_main_file-description.
    ELSE.    " Filename lesen

      CALL FUNCTION 'CV120_SPLIT_PATH'
        EXPORTING
          pf_path  = ls_files-filename
        IMPORTING
          pfx_file = lv_file.

      IF lv_file IS NOT INITIAL.
        object-description = lv_file.
      ENDIF.
    ENDIF.
  ENDLOOP.
ENDIF.

IF object-description IS INITIAL.
  " Beschlagwortung Dokument --> Attribute aus Tabelle TOAAT
  SELECT SINGLE FROM toaat
              FIELDS descr, filename
               WHERE arc_doc_id EQ @object-key-archivedocid
                INTO @DATA(ls_doc_attributes).

  IF ls_doc_attributes-descr IS NOT INITIAL.
    object-description = ls_doc_attributes-descr.

  ELSEIF ls_doc_attributes-filename IS NOT INITIAL.
    object-description = ls_doc_attributes-filename.

  ELSE.
    cl_alink_connection=>find_by_doc_key(
                                    EXPORTING
                                      archiv_id  = object-key-imagelinkarchiveid
                                      arc_doc_id = object-key-archivedocid
                                    IMPORTING
                                      link_tab   = lt_doc_links
                                    EXCEPTIONS
                                      not_found  = 1
                                      OTHERS     = 2 ).
    IF lt_doc_links IS NOT INITIAL.
      DATA(lv_doc_type) = lt_doc_links[ 1 ]-ar_object.
      SELECT SINGLE FROM toasp
                  FIELDS objecttext
                   WHERE language  EQ @sy-langu
                     AND ar_object EQ @lv_doc_type
                    INTO @object-description.
      IF sy-subrc NE 0 AND
         sy-langu NE 'D' ##no_text.
        SELECT SINGLE FROM toasp
                    FIELDS objecttext
                     WHERE language  EQ 'D'
                       AND ar_object EQ @lv_doc_type
                      INTO @object-description ##no_text.
      ENDIF.
    ENDIF.
  ENDIF.
ENDIF.

" Bezeichnung Dokumentart
" ist nicht eindeutig --> keine Bezeichnung ermittelbar

swc_set_element container 'Description' object-description ##no_text.
end_property.
