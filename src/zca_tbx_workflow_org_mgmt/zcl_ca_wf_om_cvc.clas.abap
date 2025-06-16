"! <p class="shorttext synchronized" lang="en">WF-OM: Constants and value checks for org. management</p>
CLASS zcl_ca_wf_om_cvc DEFINITION PUBLIC
                                  FINAL
                                  CREATE PRIVATE.
* P U B L I C   S E C T I O N
  PUBLIC SECTION.
*   c o n s t a n t s
    CONSTANTS:
      "! <p class="shorttext synchronized" lang="en">Evaluation path</p>
      BEGIN OF evaluation_path,
        manager_2_orgunit TYPE wegid VALUE 'BOSSONLY' ##no_text,
        orgunit_2_orgunit TYPE wegid VALUE 'O-O' ##no_text,
        person_2_orgunit  TYPE wegid VALUE 'P-S-O' ##no_text,
        staff_2_orgunit   TYPE wegid VALUE 'SBES' ##no_text,
      END OF evaluation_path,

      "! <p class="shorttext synchronized" lang="en">Scope of determination</p>
      BEGIN OF scope,
        manager      TYPE char1 VALUE '1' ##no_text,
        members_only TYPE char1 VALUE '2' ##no_text,
        all          TYPE char1 VALUE '3' ##no_text,
      END OF scope.

*   s t a t i c   m e t h o d s
    CLASS-METHODS:
      "! <p class="shorttext synchronized" lang="en">Get instance</p>
      "!
      "! @parameter result | <p class="shorttext synchronized" lang="en">Class instance</p>
      get_instance
        RETURNING
          VALUE(result) TYPE REF TO zcl_ca_wf_om_cvc.

*   i n s t a n c e   m e t h o d s
    METHODS:
      "! <p class="shorttext synchronized" lang="en">Valid scope passed?</p>
      "!
      "! @parameter iv_scope | <p class="shorttext synchronized" lang="en">Scope</p>
      "! @parameter result   | <p class="shorttext synchronized" lang="en">X = Scope is valid</p>
      "! @raising   zcx_ca_wf_om_org_model | <p class="shorttext synchronized" lang="en">WF-OM: Org. model determination exceptions</p>
      is_scope_valid
        IMPORTING
          iv_scope      TYPE char1
        RETURNING
          VALUE(result) TYPE abap_boolean
        RAISING
          zcx_ca_wf_om_org_model.


* P R I V A T E   S E C T I O N
  PRIVATE SECTION.
*   s t a t i c   a t t r i b u t e s
    CLASS-DATA:
*     o b j e c t   r e f e r e n c e s
      "! <p class="shorttext synchronized" lang="en">Instance of the class itself</p>
      mo_instance       TYPE REF TO zcl_ca_wf_om_cvc.

ENDCLASS.



CLASS zcl_ca_wf_om_cvc IMPLEMENTATION.

  METHOD get_instance.
    "-----------------------------------------------------------------*
    "   Get instance
    "-----------------------------------------------------------------*
    IF zcl_ca_wf_om_cvc=>mo_instance IS NOT BOUND.
      zcl_ca_wf_om_cvc=>mo_instance = NEW #( ).
    ENDIF.

    result = zcl_ca_wf_om_cvc=>mo_instance.
  ENDMETHOD.                    "get_instance


  METHOD is_scope_valid.
    "-----------------------------------------------------------------*
    "   Valid scope passed?
    "-----------------------------------------------------------------*
    result = xsdbool( iv_scope BETWEEN scope-manager AND scope-all ).

    IF result EQ abap_false    AND
       result IS NOT SUPPLIED.
      "Parameter '&1' has invalid value '&2'
      RAISE EXCEPTION TYPE zcx_ca_wf_om_org_model
        EXPORTING
          textid   = zcx_ca_wf_om_org_model=>param_invalid
          mv_msgty = 'E'
          mv_msgv1 = 'IV_SCOPE'
          mv_msgv2 = CONV #( iv_scope ) ##no_text.
    ENDIF.
  ENDMETHOD.                    "is_scope_valid

ENDCLASS.
