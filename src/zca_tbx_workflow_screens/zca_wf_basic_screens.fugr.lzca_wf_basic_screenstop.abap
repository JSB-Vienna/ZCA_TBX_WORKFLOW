FUNCTION-POOL zca_wf_basic_screens.      "MESSAGE-ID ..

* g l o b a l   d e f i n i t i o n s
DATA:
* s t r u c t u r e s
  "! <p class="shorttext synchronized" lang="en">Labels/descriptions for subscreen frames</p>
  BEGIN OF gs_frame_label,
    "! <p class="shorttext synchronized" lang="en">Subscreen 0001: Subscreen pattern</p>
    d0001 TYPE text70,
    "! <p class="shorttext synchronized" lang="en">Subscreen 0900: Note editor</p>
    d0900 TYPE text70,
    "! <p class="shorttext synchronized" lang="en">Subscreen 0905: Archive content</p>
    d0905 TYPE text70,
    "! <p class="shorttext synchronized" lang="en">Subscreen 0906: Dummy screen instead of Archive content</p>
    d0906 TYPE text70,
  END   OF gs_frame_label,

* s i n g l e   v a l u e s
  "! <p class="shorttext synchronized" lang="en">Screen field name the value-request was triggered</p>
  gv_pov_field         TYPE dynfnam ##needed ##decl_modul.


* INCLUDE LZCA_SCR_FW_COPY_PATTERND...       " Local class definition
