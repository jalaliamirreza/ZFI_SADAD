"Name: \PR:SAPMF05A\FO:PAI_ANZAHLUNGSKONTO\SE:BEGIN\EI
ENHANCEMENT 0 ZFI48.
*

BREAK omrani.


IF  ( SY-DYNNR =  '0110' AND
      SY-TCODE =  'FBA7' AND
      SY-TITLE CS 'Post Vendor Down Payment'   ) OR
    ( SY-DYNNR =  '0111' AND
      SY-TCODE =  'FBA2' AND
      SY-TITLE CS 'Post Customer Down Payment' ).
  IF OK-CODE = SPACE.
    OK-CODE = 'AF'.
  ENDIF.
ENDIF.

ENDENHANCEMENT.
