;+
; NAME:
;    decstr
; PURPOSE: (one line)
;    Convert declination in radians to an ASCII string.
; DESCRIPTION:
;
; CATEGORY:
;    Astronomy
; CALLING SEQUENCE:
;    decstr, declination, places, str
; INPUTS:
;    declination - Declination, in radians, to be converted to a string.  May
;                  a vector, in which case the output will be a vector.
;    places      - Resolution of output string.
;             = -7     nearest hour.                    +DD
;             = -6     nearest 30 minutes.              +DD:(00,30)
;             = -5     nearest 15 minutes.              +DD:(00,15,30,45)
;             = -4     nearest 10 minutes.              +DD:M0
;             = -3     nearest 5 minutes.               +DD:M(0,5)
;             = -2     nearest minute.                  +DD:MM
;             = -1     nearest ten seconds.             +DD:MM:S0
;             =  0     nearest second.                  +DD:MM:SS
;             =  1     nearest tenth of a second.       +DD:MM:SS.s
;             =  2     nearest hundredth of a second.   +DD:MM:SS.ss
;             =  3     nearest thousandth of a second.  +DD:MM:SS.sss
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
;    SEPCHAR - Separator character for the fields in the position.
;                 Default=':'.  Warning, if you do not use ':' then the
;                 string may not be parsable by raparse.
; OUTPUTS:
;    str         - Output string for the converted declination.
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;    Declination (in the range [-pi/2,pi/2]) is converted to a string.
;    The sign of the declination is always provided and there are no imbedded
;    blanks.  PLACES indicates the reported precision of the string.
;    Calls external routine radtodms.
; MODIFICATION HISTORY:
;  Copyright (C) 1987,91, by Marc W. Buie
;  Version dated 91/8/5
;      Ported by Doug Loucks, Lowell Observatory, August 11, 1993, from the
;         C-Language version written by Marc Buie.
;  2003/05/09, MWB, added SEPCHAR keyword
;-
;
; ------------------------------------------------------------------------------
; Local procedure d_adjust
; ------------------------------------------------------------------------------
pro d_adjust, deg, mm
   i = where( mm eq 60, count )
   if count ne 0 then begin
      deg[i] = deg[i] + 1
      mm[i] =  0
   endif
end

; ------------------------------------------------------------------------------
; Local procedure m_adjust
; ------------------------------------------------------------------------------
pro m_adjust, mm, ss
   i = where( ss ge 60.0, count )
   if count ne 0 then begin
      mm[i] = mm[i] + 1
      ss[i] = 0.0
   endif
end

; ------------------------------------------------------------------------------
; Main Procedure decstr
; ------------------------------------------------------------------------------
pro decstr, declination, places, str, SEPCHAR=sepchar

if n_params() ne 3 then begin
   ; Display the calling sequence.
   print, 'decstr, declination, places, str'
   return
endif

self='DECSTR:'

; Allow declination parameter to be integer, long, float, or double, and
; scalar or vector.
if badpar( declination, [2,3,4,5], [0,1], CALLER=self+' (dec) ' ) then return

; Allow places to be integer or long scalar.
if badpar( places, [2,3], 0, CALLER=self+' (places) ' ) then return

if badpar( sepchar, [0,7], 0, CALLER=self+' (SEPCHAR) ',default=':') then return

pi_2 = !dpi / 2.0

; Initialize the output string to null.
str = ''

i = where( (declination gt pi_2) OR (declination lt -pi_2), count )
if count ne 0 then begin
   message, 'Declination out of range, |dec| > pi/2.', /INFO
   return
endif

; First, convert to numerical values of degrees, minutes, seconds.
radtodms, declination, sgn, dd, mm, ss

sign = string( fix( sgn eq 1 ), format='(I1)' )
p = where( sign eq '1', count )
if count ne 0 then sign[p] = '+'
n = where( sign eq '0', count )
if count ne 0 then sign[n] = '-'

deg = abs( dd )

; This is the big decision table that generates the requested format.

case places OF
   -7 : begin     ; +DD, Nearest degree.
      i = where( float(mm) + ss/60.0 ge 30.0, count )
      if count ne 0 then deg[i] = deg[i] + 1
      str = sign + string( deg, format='(I2.2)' )
   end

   -6 : begin     ;  +DD:(00,30), Nearest 30 minutes.
      mm = nint( ( mm + ss/60.0 ) / 30.0 ) * 30.0
      d_adjust, deg, mm
      str = sign + string( deg, format='(I2.2)' ) + sepchar + $
      string( mm, format='(I2.2)' )
   end

   -5 : begin     ;  +DD:(00,15,30,45), Nearest 15 minutes.
      mm = nint( ( mm + ss/60.0 ) / 15.0 ) * 15.0
      d_adjust, deg, mm
      str = sign + string( deg, format='(I2.2)' ) + sepchar + $
      string( mm, format='(I2.2)' )
   end

   -4 : begin     ;  +DD:M0, Nearest 10 minutes.
      mm = nint( ( mm + ss/60.0 ) / 10.0 ) * 10.0
      d_adjust, deg, mm
      str = sign + string( deg, format='(I2.2)' ) + sepchar + $
      string( mm, format='(I2.2)' )
   end

   -3 : begin     ;  +DD:M(0,5), Nearest 5 minutes.
      mm = nint( ( mm + ss/60.0 ) / 5.0 ) * 5.0
      d_adjust, deg, mm
      str = sign + string( deg, format='(I2.2)' ) + sepchar + $
      string( mm, format='(I2.2)' )
   end

   -2 : begin     ;  +DD:MM, Nearest minute.
      mm = nint( mm + ss/60.0 )
      d_adjust, deg, mm
      str = sign + string( deg, format='(I2.2)' ) + sepchar + $
      string( mm, format='(I2.2)' )
   end

   -1 : begin     ;  +DD:MM:S0, Nearest 10 seconds.
      ss = nint( ss / 10.0 ) * 10.0
      d_adjust, mm, ss
      d_adjust, deg, mm
      str = sign + string( deg, format='(I2.2)' ) + sepchar + $
      string( mm, format='(I2.2)' ) + sepchar + $
      string( ss, format='(I2.2)' )
   end

   0  : begin     ;  +DD:MM:SS, Nearest second.
      ss = nint( ss )
      d_adjust, mm, ss
      d_adjust, deg, mm
      str = sign + string( deg, format='(I2.2)' ) + sepchar + $
      string( mm, format='(I2.2)' ) + sepchar + $
      string( ss, format='(I2.2)' )
   end

   1  : begin     ;  +DD:MM:SS.s, Nearest tenth of a second.
      ss = nint( ss / 0.1 ) * 0.1
      ; Save the fractional part of seconds.
      fss = ss - fix( ss )
      m_adjust, mm, ss
      d_adjust, deg, mm
      str = sign + string( deg, format='(I2.2)' ) + sepchar + $
      string( mm, format='(I2.2)' ) + sepchar + $
      string( ss, format='(I2.2)' ) + $
      strmid( string( fss, format='(F3.1)' ), 1, 2 )
   end

   2  : begin     ;  +DD:MM:SS.ss, Nearest hundredth of a second.
      ss = nint( ss / 0.01 ) * 0.01
      ; Save the fractional part of seconds.
      fss = ss - fix( ss )
      m_adjust, mm, ss
      d_adjust, deg, mm
      str = sign + string( deg, format='(I2.2)' ) + sepchar + $
      string( mm, format='(I2.2)' ) + sepchar + $
      string( ss, format='(I2.2)' ) + $
      strmid( string( fss, format='(F4.2)' ), 1, 3 )
   end

   3  : begin     ;  +DD:MM:SS.sss, Nearest thousandth of a second.
      ss = nint( ss / 0.001, /LONG ) * 0.001
      ; Save the fractional part of seconds.
      fss = ss - fix( ss )
      m_adjust, mm, ss
      d_adjust, deg, mm
      str = sign + string( deg, format='(I2.2)' ) + sepchar + $
      string( mm, format='(I2.2)' ) + sepchar + $
      string( ss, format='(I2.2)' ) + $
      strmid( string( fss, format='(F5.3)' ), 1, 4 )
   end

   else : begin
      message, 'PLACES parameter must be in the range -7 to +3.', /INFO
      return
   end

endcase

end
