*&---------------------------------------------------------------------*
*& Include     LZCA_WF_OM_IMGF02 = ZCAWF_VM_OMTASK
*&---------------------------------------------------------------------*


FORM vm_omtask_ev05_create_entry ##called.    "Is called dynamically for modification event 05
  "-----------------------------------------------------------------*
  "   Event 05 - While creating a new entry
  "   Provide additional data to the new entry.
  "-----------------------------------------------------------------*
  DATA:
    ls_org_obj_key TYPE swhactor,
    ls_org_object  TYPE zca_wf_s_om_object.

  ls_org_obj_key = VALUE #( otype = swfco_org_task
                            objid = zcawf_vm_omtask-task_id ).
  PERFORM get_description_to_org_unit USING ls_org_obj_key
                                   CHANGING ls_org_object.
  zcawf_vm_omtask-stext    = ls_org_object-name.
  zcawf_vm_omtask-short    = ls_org_object-short_name.
  zcawf_vm_omtask-begda_om = ls_org_object-begda.
  zcawf_vm_omtask-endda_om = ls_org_object-endda.
ENDFORM.                    "vm_omtask_ev05_create_entry


FORM vm_omtask_evaa_get_data ##called.    "Is called dynamically for modification event AA
  "-----------------------------------------------------------------*
  "   Event AA - Instead of the standard data read routine
  "   Replacement of standard routine reading data from DB table.
  "   Uses standard routine and actualize after that the names to
  "   the org. object.
  "-----------------------------------------------------------------*
  "Local data definitions
  FIELD-SYMBOLS:
    <ls_task>            TYPE zcawf_vm_omtask.

  DATA:
    ls_org_obj_key TYPE swhactor,
    ls_org_object  TYPE zca_wf_s_om_object.

  "Call standard subroutine -> fills table TOTAL
  PERFORM get_data_zcawf_vm_omtask.

  "Enhance DB data from personnel master data
  LOOP AT total ASSIGNING <ls_task> CASTING.
    ls_org_obj_key = VALUE #( otype = swfco_org_task
                              objid = <ls_task>-task_id ).
    PERFORM get_description_to_org_unit USING ls_org_obj_key
                                     CHANGING ls_org_object.
    <ls_task>-stext    = ls_org_object-name.
    <ls_task>-short    = ls_org_object-short_name.
    <ls_task>-begda_om = ls_org_object-begda.
    <ls_task>-endda_om = ls_org_object-endda.
  ENDLOOP.
ENDFORM.                    "vm_omtask_evaa_get_data
