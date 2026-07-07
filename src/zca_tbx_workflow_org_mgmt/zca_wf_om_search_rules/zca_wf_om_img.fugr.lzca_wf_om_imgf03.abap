*&---------------------------------------------------------------------*
*& Include     LZCA_WF_OM_IMGF01 = Common methods
*&---------------------------------------------------------------------*


FORM get_description_to_org_unit USING is_org_obj_key TYPE swhactor
                              CHANGING cs_org_object  TYPE zca_wf_s_om_object.
    "-----------------------------------------------------------------*
    "   Get the description and other details of an org. object
    "-----------------------------------------------------------------*
  TRY.
      "This class is buffering all the element. So it is a one-time reading.
      cs_org_object = zcl_ca_wf_om_org_model=>get_instance( is_key = is_org_obj_key )->ms_data.

    CATCH zcx_ca_error INTO DATA(lx_caught).
      cs_org_object-name = lx_caught->get_text( ).
  ENDTRY.
ENDFORM.                    "get_description_to_org_unit
