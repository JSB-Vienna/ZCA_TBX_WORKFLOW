PROCESS BEFORE OUTPUT.
 MODULE LISTE_INITIALISIEREN.
 LOOP AT EXTRACT WITH CONTROL
  TCTRL_ZCAWF_VM_ACTAPPL CURSOR NEXTLINE.
   MODULE LISTE_SHOW_LISTE.
 ENDLOOP.
*
PROCESS AFTER INPUT.
 MODULE LISTE_EXIT_COMMAND AT EXIT-COMMAND.
 MODULE LISTE_BEFORE_LOOP.
 LOOP AT EXTRACT.
   MODULE LISTE_INIT_WORKAREA.
   CHAIN.
    FIELD ZCAWF_VM_ACTAPPL-OBJECT .
    FIELD ZCAWF_VM_ACTAPPL-SUBOBJECT .
    FIELD ZCAWF_VM_ACTAPPL-IS_ACTIVE .
    FIELD ZCAWF_VM_ACTAPPL-OBJTXT .
    FIELD ZCAWF_VM_ACTAPPL-SUBOBJTXT .
    MODULE SET_UPDATE_FLAG ON CHAIN-REQUEST.
    MODULE COMPLETE_ZCAWF_VM_ACTAPPL ON CHAIN-REQUEST.
   ENDCHAIN.
   FIELD VIM_MARKED MODULE LISTE_MARK_CHECKBOX.
   CHAIN.
    FIELD ZCAWF_VM_ACTAPPL-OBJECT .
    FIELD ZCAWF_VM_ACTAPPL-SUBOBJECT .
    MODULE LISTE_UPDATE_LISTE.
   ENDCHAIN.
 ENDLOOP.
 MODULE LISTE_AFTER_LOOP.
