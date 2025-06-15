*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZCAWF_VM_ACTAPPL................................*
TABLES: ZCAWF_VM_ACTAPPL, *ZCAWF_VM_ACTAPPL. "view work areas
CONTROLS: TCTRL_ZCAWF_VM_ACTAPPL
TYPE TABLEVIEW USING SCREEN '0001'.
DATA: BEGIN OF STATUS_ZCAWF_VM_ACTAPPL. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZCAWF_VM_ACTAPPL.
* Table for entries selected to show on screen
DATA: BEGIN OF ZCAWF_VM_ACTAPPL_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZCAWF_VM_ACTAPPL.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZCAWF_VM_ACTAPPL_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZCAWF_VM_ACTAPPL_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZCAWF_VM_ACTAPPL.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZCAWF_VM_ACTAPPL_TOTAL.

*...processing: ZCAWF_VM_WFSTA..................................*
TABLES: ZCAWF_VM_WFSTA, *ZCAWF_VM_WFSTA. "view work areas
CONTROLS: TCTRL_ZCAWF_VM_WFSTA
TYPE TABLEVIEW USING SCREEN '0002'.
DATA: BEGIN OF STATUS_ZCAWF_VM_WFSTA. "state vector
          INCLUDE STRUCTURE VIMSTATUS.
DATA: END OF STATUS_ZCAWF_VM_WFSTA.
* Table for entries selected to show on screen
DATA: BEGIN OF ZCAWF_VM_WFSTA_EXTRACT OCCURS 0010.
INCLUDE STRUCTURE ZCAWF_VM_WFSTA.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZCAWF_VM_WFSTA_EXTRACT.
* Table for all entries loaded from database
DATA: BEGIN OF ZCAWF_VM_WFSTA_TOTAL OCCURS 0010.
INCLUDE STRUCTURE ZCAWF_VM_WFSTA.
          INCLUDE STRUCTURE VIMFLAGTAB.
DATA: END OF ZCAWF_VM_WFSTA_TOTAL.

*.........table declarations:.................................*
TABLES: BALOBJ                         .
TABLES: BALOBJT                        .
TABLES: BALSUB                         .
TABLES: BALSUBT                        .
TABLES: ZCAWF_ACTVAPPL                 .
TABLES: ZCAWF_STATUS                   .
TABLES: ZCAWF_STATUSTX                 .
