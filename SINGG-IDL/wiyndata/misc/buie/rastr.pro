;+
; NAME:
;    rastr
; PURPOSE: (one line)
;    Convert RA in radians to hours, minutes, and seconds (ASCII string).
; DESCRIPTION:
;
; CATEGORY:
;    Astronomy
; CALLING SEQUENCE:
;    rastr, radians, places, str, carry
; INPUTS:
;    ra     : Right Ascension, in radians, to be converted to a string.
;             May a vector, in which case the outputs will be vectors.
;    places : Resolution of output string (integer).
;             = -8     Decimal hours                     HH.hhhhh
;             = -7     nearest hour.                     HH
;             = -6     nearest 30 minutes.               HH:(00,30)
;             = -5     nearest 15 minutes.               HH:(00,15,30,45)
;             = -4     nearest 10 minutes.               HH:M0
;             = -3     nearest 5 minutes.                HH:M(0,5)
;             = -2     nearest minute.                   HH:MM
;             = -1     nearest ten seconds.              HH:MM:S0
;             =  0     nearest second.                   HH:MM:SS
;             =  1     nearest tenth of a second.        HH:MM:SS.s
;             =  2     nearest hundredth of a second.    HH:MM:SS.ss
;             =  3     nearest thousandth of a second.   HH:MM:SS.sss
;             =  4     nearest thousandth of a second.   HH:MM:SS.ssss
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
;    SEPCHAR - Separator character for the fields in the position.
;                 Default=':'.  Warning, if you do not use ':' then the
;                 string may not be parsable by raparse.
; OUTPUTS:
;    str   - Output string for the converted right ascension.
;    carry - Flag that indicates ra rolled over after rounding.
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;    Right ascension is reduced to the range [0,24) then converted to a string.
;    There are no imbedded blanks.
;    Calls external routine radtohms.
; MODIFICATION HISTORY:
;  Copyright (C) 1987, by Marc W. Buie
;  Version dated 87/4/21
;    Ported by Doug loucks, Lowell Observatory, July, 1993, from the
; C-Language version written by Marc Buie.
; 97/07/14, MWB, added places=4
;  99/1/12, MWB, added places = -8
;  2001/11/04, MWB, fixed bug in places=-7 case
;  2003/05/09, MWB, added SEPCHAR keyword
;-
; ------------------------------------------------------------------------------
; Local procedure dadjust
; ------------------------------------------------------------------------------
pro dadjust, hh, mm, carry
i = where( mm eq 60, count )
if count gt 0 then begin
   hh[i] = hh[i] + 1
   mm[i] =  0
   j = where( hh[i] eq 24, count )
   if count gt 0 then begin
      hh[i[j]] = 0
      carry[i[j]] = 1
   endif
endif
end

; ------------------------------------------------------------------------------
; Local procedure madjust
; ------------------------------------------------------------------------------
pro madjust, hh, mm, ss, carry
i = where( ss ge 60.0, count )
if count gt 0 then begin
   mm[i] = mm[i] + 1
   ss[i] =  0
   j = where( mm[i] eq 60, count )
   if count gt 0 then begin
      mm[i[j]] = 0
      hh[i[j]] = hh[i[j]] + 1
      k = where( hh[i[j]] eq 24, count )
      if count gt 0 then begin
         hh[i[j[k]]] = 0
         carry[i[j[k]]] = 1
      endif
   endif
endif
end

; ------------------------------------------------------------------------------
; Main procedure rastr
; ------------------------------------------------------------------------------
pro rastr, ra, places, str, carry, SEPCHAR=sepchar

if n_params() LT 3 then begin
   ; Display the calling sequence.
   print, 'rastr, radians, places, str, carry'
   return
endif

self='RASTR:'

; Allow input radians to be integer, long, floating, or double, and scalar
; or vector.
if badpar( ra, [2,3,4,5], [0,1], CALLER=self+' (ra) ' ) then return

; Allow places to be integer or long scalar.
if badpar( places, [2,3], 0, CALLER=self+' (places) ' ) then return

if badpar( sepchar, [0,7], 0, CALLER=self+' (SEPCHAR) ',default=':') then return

true  = 1
false = 0

; First, convert to numerical values of hours, minutes, and seconds.
radtohms, ra, hh, mm, ss

; Initialize the carry parameter.  This statement, although not the most
; efficient, automatically sets 'carry' to have the same rank as 'ra'.
carry = fix( ra ) * 0

; This is the big decision table that generates the requested format.
case places OF
   -8 : begin   ; HH.hhhhh, Decimal hours
      hh = double(hh) + double(mm)/60.0d0 + ss/3600.0d0
      hh = double(long(hh * 100000.0d0 + 0.5d0))/100000.0d0
      fhh = hh - fix( hh )
      hh  = fix(hh)
      i = where( hh eq 24, count)
      if count gt 0 then begin
         hh[i] = hh[i]-24
         carry[i] = true
      endif
      str = string( hh, format='(i2.2)' ) + $
            strmid( string( fhh, format='(F7.5)' ), 1, 6 )
   end

   -7 : begin   ; HH, Nearest hour.
      i = where( float(mm) + ss/60.0 ge 30.0, count )
      if count gt 0 then hh[i]=hh[i]+1
      i = where( hh eq 24, count )
      if count gt 0 then begin
         hh[i] = 0
         carry[i] = true
      endif
      str = string( hh, format='(I2.2)' )
   end

   -6 : begin  ; HH:(00,30), Nearest 30 minutes.
      mm = nint( ( mm + ss / 60.0 ) / 30.0 ) * 30.0
      dadjust, hh, mm, carry
      str = string( hh, format='(I2.2)' ) + sepchar + $
            string( mm, format='(I2.2)' )
   end

   -5 : begin  ; HH:(00,15,30,45), Nearest 15 minutes.
      mm = nint( ( mm + ss / 60.0 ) / 15.0 ) * 15.0
      dadjust, hh, mm, carry
      str = string( hh, format='(I2.2)' ) + sepchar + $
            string( mm, format='(I2.2)' )
   end

   -4 : begin  ; HH:M0, Nearest 10 minutes.
      mm = nint( ( mm + ss / 60.0 ) / 10.0 ) * 10.0
      dadjust, hh, mm, carry
      str = string( hh, format='(I2.2)' ) + sepchar + $
            string( mm, format='(I2.2)' )
   end

   -3 : begin   ; HH:M(0,5), Nearest 5 minutes.
      mm = nint( ( mm + ss / 60.0 ) / 5.0 ) * 5.0
      dadjust, hh, mm, carry
      str = string( hh, format='(I2.2)' ) + sepchar + $
            string( mm, format='(I2.2)' )
   end

   -2 : begin    ; HH:MM, Nearest minute.
      mm = nint( mm + ss / 60.0 )
      dadjust, hh, mm, carry
      str = string( hh, format='(I2.2)' ) + sepchar + $
            string( mm, format='(I2.2)' )
   end

   -1 : begin  ; HH:MM:S0, Nearest 10 seconds.
      ss = nint( ss / 10.0 ) * 10.0
      madjust, hh, mm, ss, carry
      str = string( hh, format='(I2.2)' ) + sepchar + $
            string( mm, format='(I2.2)' ) + sepchar + $
            string( ss, format='(I2.2)' )
   end

   0 : begin    ; HH:MM:SS, Nearest second.
      ss = nint( ss )
      madjust, hh, mm, ss, carry
      str = string( hh, format='(I2.2)' ) + sepchar + $
            string( mm, format='(I2.2)' ) + sepchar + $
            string( ss, format='(I2.2)' )
   end

   1 : begin  ; HH:MM:SS.s, Nearest tenth of a second.
      ss = nint( ss / 0.1 ) * 0.1
      ; Save the fractional part of the seconds.
      fss = ss - fix( ss )
      madjust, hh, mm, ss, carry
      str = string( hh, format='(I2.2)' ) + sepchar + $
            string( mm, format='(I2.2)' ) + sepchar + $
            string( ss, format='(I2.2)' ) + $
            strmid( string( fss, format='(F3.1)' ), 1, 2 )
   end

   2 : begin  ; HH:MM:SS.ss, Nearest hundredth of a second.
      ss = nint( ss / 0.01 ) * 0.01
      ; Save the fractional part of the seconds.
      fss = ss - fix( ss )
      madjust, hh, mm, ss, carry
      str = string( hh, format='(I2.2)' ) + sepchar + $
            string( mm, format='(I2.2)' ) + sepchar + $
            string( ss, format='(I2.2)' ) + $
            strmid( string( fss, format='(F4.2)' ), 1, 3 )
   end

   3 : begin  ; HH:MM:SS.sss, Nearest thousandth of a second.
      ss = nint( ss / 0.001, /LONG ) * 0.001
      ; Save the fractional part of the seconds.
      fss = ss - fix( ss )
      madjust, hh, mm, ss, carry
      str = string( hh, format='(I2.2)' ) + sepchar + $
            string( mm, format='(I2.2)' ) + sepchar + $
            string( ss, format='(I2.2)' ) + $
            strmid( string( fss, format='(F5.3)' ), 1, 4 )
   end

   4 : begin  ; HH:MM:SS.ssss, Nearest ten thousandth of a second.
      ss = nint( ss / 0.0001, /LONG ) * 0.0001
      ; Save the fractional part of the seconds.
      fss = ss - fix( ss )
      madjust, hh, mm, ss, carry
      str = string( hh, format='(I2.2)' ) + sepchar + $
            string( mm, format='(I2.2)' ) + sepchar + $
            string( ss, format='(I2.2)' ) + $
            strmid( string( fss, format='(F6.4)' ), 1, 5 )
   end

   else : begin
      message, 'PLACES parameter must be in the range -8 to +4.', /INFO
      return
   end

endcase

end
