;+
; NAME:
;     rangepar
; PURPOSE: (one line)
;     Parse a string with numbers and ranges to get an expanded list of numbers.
; DESCRIPTION:
;   You can provide single numbers or a range of numbers.  Ranges are
;     delineated by a hyphen (for example 120-125).  Ranges or single numbers
;     are separated by either a comma or space.  Spaces (even multiples) are
;     tolerated anywhere.  But, only one comma is allowed between numbers or
;     ranges.  The string can have an optional exclusion descriptor.  Anything
;     after the first 'x' is used as an exclusion set (ranges or numbers,
;     same syntax).
;
;   So, the string '100-105x103' would return [100,101,102,104,105] as an array
;      of integers.
; CATEGORY:
;  Utility
; CALLING SEQUENCE:
;     rangepar,str,range
; INPUTS:
;     str      - String with number range (see description)..
; OPTIONAL INPUT PARAMETERS:
; KEYWORD PARAMETERS:
; OUTPUTS:
;     range    - Array of numbers collected from str.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;     2001/07/25 - Written by Marc W. Buie, Lowell Observatory.
;-
pro rangepar_single,str,range

   ; split the string by blank delimiters
   parts = strsplit(str,' ',/extract)

   ; split each of the parts by hyphens and build the list of numbers
   nvals = 0
   for i=0,n_elements(parts)-1 do begin
      val=long(strsplit(parts[i],'-',/extract))
      if n_elements(val) eq 1 then begin
         val1 = val[0]
         val2 = val[0]
      endif else begin
         val1 = val[0]
         val2 = val[1]
      endelse

      if nvals eq 0 then begin
         range = lindgen(val2-val1+1)+val1
         nvals = val2-val1+1
      endif else begin
         range = [range,lindgen(val2-val1+1)+val1]
         nvals = nvals + val2-val1+1
      endelse
      
   endfor

end

pro rangepar,str0,range

   if badpar(str0,7,0,caller='rangepar (str) ') then return

   ; make a working copy of the input string
   str=str0

   ; replace any commas with a blank
   repeat begin
      pos=strpos(str ,',')
      if pos ge 0 then begin
         strput,str,' ',pos
      endif
   endrep until pos lt 0

   ; eliminate leading and trailing blanks then compress multiple blanks to one.
   str = strcompress(strtrim(str,2))

   ; now break on any 'x', first part goes in, second part goes out.
   parts=strsplit(str,'x',/extract)

   rangepar_single,parts[0],range
   if n_elements(parts) gt 1 then begin
      rangepar_single,parts[1],remove
      intrsect,range,remove,overlap,noverlap
      if noverlap gt 0 then begin
         intrsect,range,overlap,keep,nfound,/nnot
         if nfound gt 0 then begin
            range = keep
         endif else begin
            print,'Everything is excluded, excluding nothing.'
         endelse
      endif
   endif

end
