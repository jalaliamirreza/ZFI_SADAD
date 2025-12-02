"Name: \PR:RFIDTRBOE2\FO:USER_COMMAND\SE:BEGIN\EI
ENHANCEMENT 0 ZRFIDTRBOE2.
*
DATA: lv_txt(50).

CASE 'X'.
  WHEN bill or x2_c or x6_c or x4_c or x10_c.
    lv_txt = 'Deleted'.
  WHEN x3_c or x7_c or x5_c or x11_c or x14_c.
    lv_txt = ''.
  WHEN x8_c or x15_c.
    lv_txt = 'Presented to Bank'.
  WHEN x9_c or x13_c.
    lv_txt = 'Bounced at Bank'.
  WHEN x17_C.
    lv_txt = 'Cleared at Bank'.
  WHEN x12_c or x16_c.
    lv_txt = 'Presented to Vendor'.
ENDCASE.

EXPORT lv_txt TO MEMORY ID 'RFIDTRBOE1'.       "" Import in ZPROCESS_00001120

ENDENHANCEMENT.
