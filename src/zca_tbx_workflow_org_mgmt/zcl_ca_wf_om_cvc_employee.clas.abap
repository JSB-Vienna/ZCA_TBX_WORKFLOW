"! <p class="shorttext synchronized" lang="en">WF-OM: Constants and value checks for org. object Employee</p>
CLASS zcl_ca_wf_om_cvc_employee DEFINITION PUBLIC
                                           FINAL
                                           CREATE PRIVATE.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Employment status</p>
      BEGIN OF employment_status,
        is_leaving  TYPE stat2 VALUE pfrd3_c_stat2_leave,      "= 0
        is_inactive TYPE stat2 VALUE pfrd3_c_stat2_inactive,   "= 1
        is_retired  TYPE stat2 VALUE pfrd3_c_stat2_retire,     "= 2
        is_active   TYPE stat2 VALUE pfrd3_c_stat2_active,     "= 3
      END   OF employment_status,

      "! <p class="shorttext synchronized" lang="en">Communication types in PA0105</p>
      BEGIN OF comm_type,
        "! <p class="shorttext synchronized" lang="en">Communication type: SAP user Id</p>
        sap_user_id    TYPE usrty VALUE '0001' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Communication type: eMail address</p>
        email_address  TYPE usrty VALUE '0010' ##no_text,
        "! <p class="shorttext synchronized" lang="en">Communication type: Windows Net Id</p>
        windows_net_id TYPE usrty VALUE '9001' ##no_text,
      END OF comm_type,

      "! <p class="shorttext synchronized" lang="en">Column name in PA0105</p>
      BEGIN OF column_name,
        "! <p class="shorttext synchronized" lang="en">Column name for USRID</p>
        usrid      TYPE fieldname VALUE 'USRID',
        "! <p class="shorttext synchronized" lang="en">Column name for USRID_LONG</p>
        usrid_long TYPE fieldname VALUE 'USRID_LONG',
      END OF column_name.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Class constructor</p>
      class_constructor,

      "! <p class="shorttext synchronized" lang="en">Get instance</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">Class instance</p>
      get_instance
        RETURNING
          VALUE(result) TYPE REF TO zcl_ca_wf_om_cvc_employee.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Valid column name passed?</p>
      "!
      "! @parameter iv_column_name        | <p class="shorttext synchronized" lang="en">Column name in PA0105</p>
      "! @parameter result                | <p class="shorttext synchronized" lang="en">X = Column name is valid</p>
      "! @raising   zcx_ca_wf_om_employee | <p class="shorttext synchronized" lang="en">WF-OM: BC Employee exceptions</p>
      is_column_name_valid
        IMPORTING
          iv_column_name TYPE field_name
        RETURNING
          VALUE(result)  TYPE abap_boolean
        RAISING
          zcx_ca_wf_om_employee,

      "! <p class="shorttext synchronized" lang="en">Valid communication passed?</p>
      "!
      "! @parameter iv_comm_type          | <p class="shorttext synchronized" lang="en">Communication type</p>
      "! @parameter result                | <p class="shorttext synchronized" lang="en">X = Communication type is valid</p>
      "! @raising   zcx_ca_wf_om_employee | <p class="shorttext synchronized" lang="en">WF-OM: BC Employee exceptions</p>
      is_comm_type_valid
        IMPORTING
          iv_comm_type  TYPE usrty
        RETURNING
          VALUE(result) TYPE abap_boolean
        RAISING
          zcx_ca_wf_om_employee,

      "! <p class="shorttext synchronized" lang="en">Valid employment status passed?</p>
      "!
      "! @parameter iv_employment_status  | <p class="shorttext synchronized" lang="en">Employment status</p>
      "! @parameter result                | <p class="shorttext synchronized" lang="en">X = Employment status is valid</p>
      "! @raising   zcx_ca_wf_om_employee | <p class="shorttext synchronized" lang="en">WF-OM: BC Employee exceptions</p>
      is_employment_status_valid
        IMPORTING
          iv_employment_status TYPE stat2
        RETURNING
          VALUE(result)        TYPE abap_boolean
        RAISING
          zcx_ca_wf_om_employee,

      "! <p class="shorttext synchronized" lang="en">Is the long Id field used in the communication type?</p>
      "!
      "! @parameter iv_comm_type | <p class="shorttext synchronized" lang="en">Communication type</p>
      "! @parameter result       | <p class="shorttext synchronized" lang="en">X = The long Id is used in the communication type</p>
      is_long_id_used_in_comm_type
        IMPORTING
          iv_comm_type  TYPE usrty
        RETURNING
          VALUE(result) TYPE abap_boolean.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Instance of the class itself</p>
      mo_instance       TYPE REF TO zcl_ca_wf_om_cvc_employee,
      "! <p class="shorttext synchronized" lang="en">Columns of DB table PA0105</p>
      mt_pa0105_compnts TYPE abap_component_view_tab.

ENDCLASS.



CLASS zcl_ca_wf_om_cvc_employee IMPLEMENTATION.

  METHOD class_constructor.
    "-----------------------------------------------------------------*
    "   Class constructor
    "-----------------------------------------------------------------*
    mt_pa0105_compnts = NEW zcl_ca_ddic( iv_name = 'PA0105' )->get_component_list( ) ##no_text.
  ENDMETHOD.                    "get_columns_of_pa0105


  METHOD get_instance.
    "-----------------------------------------------------------------*
    "   Get instance
    "-----------------------------------------------------------------*
    IF zcl_ca_wf_om_cvc_employee=>mo_instance IS NOT BOUND.
      zcl_ca_wf_om_cvc_employee=>mo_instance = NEW #( ).
    ENDIF.

    result = zcl_ca_wf_om_cvc_employee=>mo_instance.
  ENDMETHOD.                    "get_instance


  METHOD is_column_name_valid.
    "-----------------------------------------------------------------*
    "   Valid column name passed?
    "-----------------------------------------------------------------*
    result = xsdbool( NOT line_exists( mt_pa0105_compnts[ name = iv_column_name ] ) ).

    IF result EQ abap_false    AND
       result IS NOT SUPPLIED.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_wf_om_employee
        EXPORTING
          textid   = zcx_ca_wf_om_employee=>param_invalid
          mv_msgty = 'E'
          mv_msgv1 = 'IV_COLUMN_NAME'
          mv_msgv2 = CONV #( iv_column_name ) ##no_text.
    ENDIF.
  ENDMETHOD.                    "is_column_name_valid


  METHOD is_comm_type_valid.
    "-----------------------------------------------------------------*
    "   Valid communication type passed?
    "-----------------------------------------------------------------*
    SELECT SINGLE FROM t591a
                FIELDS @abap_true
                 WHERE infty EQ '0105'
                   AND subty EQ @iv_comm_type
                  INTO @result.                           "#EC CI_SUBRC
    IF result EQ abap_false    AND
       result IS NOT SUPPLIED.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_wf_om_employee
        EXPORTING
          textid   = zcx_ca_wf_om_employee=>param_invalid
          mv_msgty = 'E'
          mv_msgv1 = 'IV_COMM_TYPE'
          mv_msgv2 = CONV #( iv_comm_type ) ##no_text.
    ENDIF.
  ENDMETHOD.                    "is_comm_type_valid


  METHOD is_employment_status_valid.
    "-----------------------------------------------------------------*
    "   Valid employment status passed?
    "-----------------------------------------------------------------*
    result = xsdbool( iv_employment_status BETWEEN employment_status-is_leaving
                                               AND employment_status-is_active ).
    IF result EQ abap_false    AND
       result IS NOT SUPPLIED.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_wf_om_employee
        EXPORTING
          textid   = zcx_ca_wf_om_employee=>param_invalid
          mv_msgty = 'E'
          mv_msgv1 = 'IV_EMPLOYMENT_STATUS'
          mv_msgv2 = CONV #( iv_employment_status ) ##no_text.
    ENDIF.
  ENDMETHOD.                    "is_employment_status_valid


  METHOD is_long_id_used_in_comm_type.
    "-----------------------------------------------------------------*
    "   Is the long Id field used in the communication type?
    "-----------------------------------------------------------------*
    SELECT SINGLE FROM t591c
                FIELDS long_id
                 WHERE subty EQ @iv_comm_type
                  INTO @result.                           "#EC CI_SUBRC
  ENDMETHOD.                    "is_comm_type_valid

ENDCLASS.
