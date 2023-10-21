"! <p class="shorttext synchronized" lang="en">CA-TBX Workflow: Constants with predefined MM BO types</p>
INTERFACE zif_mm_c_wf_bos PUBLIC.
* c o n s t a n t s
  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">BO type LFA1 - SAP vendor</p>
    BEGIN OF cbo_lfa1,
      instid TYPE sibfboriid VALUE space,
      typeid TYPE sibftypeid VALUE 'LFA1' ##no_text,
      catid  TYPE sibfcatid  VALUE swfco_objtype_bor,
    END   OF cbo_lfa1,

    "! <p class="shorttext synchronized" lang="en">BO type LFM1 - SAP vendor MM / level purchasing org.</p>
    BEGIN OF cbo_lfm1,
      instid TYPE sibfboriid VALUE space,
      typeid TYPE sibftypeid VALUE 'LFM1' ##no_text,
      catid  TYPE sibfcatid  VALUE swfco_objtype_bor,
    END   OF cbo_lfm1,

    "! <p class="shorttext synchronized" lang="en">BO type BUS2012 - SAP MM purchase order</p>
    BEGIN OF cbo_bus2012,
      instid TYPE sibfboriid VALUE space,
      typeid TYPE sibftypeid VALUE 'BUS2012' ##no_text,
      catid  TYPE sibfcatid  VALUE swfco_objtype_bor,
    END   OF cbo_bus2012,

    "! <p class="shorttext synchronized" lang="en">BO type BUS2009 - SAP MM purchase requisition item</p>
    BEGIN OF cbo_bus2009,
      instid TYPE sibfboriid VALUE space,
      typeid TYPE sibftypeid VALUE 'BUS2009' ##no_text,
      catid  TYPE sibfcatid  VALUE swfco_objtype_bor,
    END   OF cbo_bus2009,

    "! <p class="shorttext synchronized" lang="en">BO type BUS2105 - SAP MM purchase requisition (all items)</p>
    BEGIN OF cbo_bus2105,
      instid TYPE sibfboriid VALUE space,
      typeid TYPE sibftypeid VALUE 'BUS2105' ##no_text,
      catid  TYPE sibfcatid  VALUE swfco_objtype_bor,
    END   OF cbo_bus2105,

    "! <p class="shorttext synchronized" lang="en">BO type BUS2081 - SAP MM incoming invoice</p>
    BEGIN OF cbo_bus2081,
      instid TYPE sibfboriid VALUE space,
      typeid TYPE sibftypeid VALUE 'BUS2081' ##no_text,
      catid  TYPE sibfcatid  VALUE swfco_objtype_bor,
    END   OF cbo_bus2081.

ENDINTERFACE.
