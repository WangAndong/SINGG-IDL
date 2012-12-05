;+
; NAME:
;  strb36
; PURPOSE:
;  Convert an integer into a Base 36 formatted string.
; DESCRIPTION:
;
; CATEGORY:
;  Utility
; CALLING SEQUENCE:
;  str=strb36(val)
; INPUTS:
;  val - Integer (byte, int, long) to be converted to a string
;          or
;        String to be converted to long.  Leading and trailing blanks are
;          ignored but once trimmed all characters must be letters or numbers.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  PAD - Length to pad the output string to.  Leading zeros will be prepended
;          to the string to make the string of length PAD.  If the string is
;          already longer than PAD nothing is done.  The default is zero (ie.,
;          no padding).  This keyword has no effect if the input is a string.
;
; OUTPUTS:
;  return is the string
; KEYWORD OUTPUT PARAMETERS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  98/03/16, Written by Marc W. Buie, Lowell Observatory
;  2000/02/16, MWB, added inverse function operation.
;-
FUNCTION strb36,in_val,PAD=pad

   IF badpar(in_val,[1,2,3,7],0,caller='STRB36: (val) ',type=valtype) THEN return,''
   IF badpar(pad,[0,1,2,3],0,caller='STRB36: (PAD) ',default=0) THEN return,''

   ; convert from a string to a number
   if valtype eq 7 then begin
      val=strtrim(strupcase(in_val),2)
      number = 0L
      for i=0,strlen(val)-1 do begin
         c=strmid(val,i,1)
         case 1 OF
            c ge '0' and c le '9': begin
               digit = fix(c)
            end
            c ge 'A' and c le 'Z': begin
               digit = byte(c) - 55b
            end
            else: begin
               print,'STRB36: Error! Illegal character in input string. [',val,']  [',c,'] is bad.'
               return,-1
            end
         endcase
         number = number*36 + digit[0]
      endfor
      return,number

   ; convert from a number to a string
   endif else begin

      str=''
      val=in_val

      REPEAT BEGIN
         digit = val mod 36
         val   = val / 36
         IF digit le 9 THEN $
            str = string(48b+byte(digit))+str $
         else $
            str = string(55b+byte(digit))+str
      ENDREP UNTIL val eq 0

      FOR i=0,pad-strlen(str)-1 DO str='0'+str

      RETURN,str

   endelse

END
