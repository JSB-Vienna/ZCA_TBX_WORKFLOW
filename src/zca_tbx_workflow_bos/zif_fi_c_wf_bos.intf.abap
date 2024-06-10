"! <p class="shorttext synchronized" lang="en">CA-TBX Workflow: Constants with predefined FI BO types</p>
INTERFACE zif_fi_c_wf_bos PUBLIC.
* c o n s t a n t s
  CONSTANTS:
    "! <p class="shorttext synchronized" lang="en">BO type BKPF - SAP FI accounting document</p>
    BEGIN OF cbo_bkpf,
      instid TYPE sibfboriid VALUE space,
      typeid TYPE sibftypeid VALUE 'BKPF' ##no_text,
      catid  TYPE sibfcatid  VALUE swfco_objtype_bor,
    END   OF cbo_bkpf,

    "! <p class="shorttext synchronized" lang="en">BO type BUS0002 - Company code</p>
    BEGIN OF cbo_bus0002,
      instid TYPE sibfboriid VALUE space,
      typeid TYPE sibftypeid VALUE 'BUS0002' ##no_text,
      catid  TYPE sibfcatid  VALUE swfco_objtype_bor,
    END   OF cbo_bus0002,

    "! <p class="shorttext synchronized" lang="en">BO type BUS3008 - SAP creditor account FI / level comp. code</p>
    BEGIN OF cbo_bus3008,
      instid TYPE sibfboriid VALUE space,                 "= COMPANY CODE + creditor number
      typeid TYPE sibftypeid VALUE 'BUS3008' ##no_text,
      catid  TYPE sibfcatid  VALUE swfco_objtype_bor,
    END   OF cbo_bus3008,

    "! <p class="shorttext synchronized" lang="en">BO type FIPP - SAP FI parked document</p>
    BEGIN OF cbo_fipp,
      instid TYPE sibfboriid VALUE space,
      typeid TYPE sibftypeid VALUE 'FIPP' ##no_text,
      catid  TYPE sibfcatid  VALUE swfco_objtype_bor,
    END   OF cbo_fipp,

    "! <p class="shorttext synchronized" lang="en">BO type LFA1 - SAP vendor</p>
    BEGIN OF cbo_lfa1,
      instid TYPE sibfboriid VALUE space,
      typeid TYPE sibftypeid VALUE 'LFA1' ##no_text,
      catid  TYPE sibfcatid  VALUE swfco_objtype_bor,
    END   OF cbo_lfa1,

    "! <p class="shorttext synchronized" lang="en">BO type LFB1 - SAP vendor FI / level company code</p>
    BEGIN OF cbo_lfb1,
      instid TYPE sibfboriid VALUE space,                 "= Creditor number + COMPANY CODE
      typeid TYPE sibftypeid VALUE 'LFB1' ##no_text,
      catid  TYPE sibfcatid  VALUE swfco_objtype_bor,
    END   OF cbo_lfb1.

ENDINTERFACE.
