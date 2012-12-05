;+
; NAME:
;    jd2year
; PURPOSE: (one line)
;    Convert Julian date to decimal year.
; DESCRIPTION:
;
; CATEGORY:
;    Astronomy
; CALLING SEQUENCE:
;    jd2year, jd, year
; INPUTS:
;    jd     : Julian Date (double precision ONLY)
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;    year   : Decimal year
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;    Written by Doug Loucks, Lowell Observatory, September 30, 1993.
;-
PRO jd2year, in_jd, out_year

IF N_PARAMS() NE 2 THEN BEGIN
   PRINT, 'jd2year, jd, year'
   RETURN
ENDIF

IF badpar( in_jd, 5, [0,1], CALLER='JD2YEAR ' ) THEN RETURN

caldatm, in_jd, year, month, day, hour, minute, second

jdcnv, year, 1, 1, 0, jd0

out_year = year + ( in_jd - jd0 ) / 365.25D0

END
