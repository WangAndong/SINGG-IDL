;+
; NAME:
;   quote
; PURPOSE:   (one line only)
;   Convert a string into one safe for including in a mySQL query
; DESCRIPTION:
;   Similary to the mysql function quote.  Leading and trailing blanks
;   are removed.  Leading and trailing quotes are preserved but are considered
;   to be delimiters for the string provided there is a leading and a trailing
;   quote.  If there is just one it is considered to be interior to the string.
;   An interior single quote is converted to \' and a backslash is converted
;   to \\.  On output, the string will have leading and trailing quotes added
;   if they weren't already there.
; CATEGORY:
;   Database
; CALLING SEQUENCE:
;   result = quote(string)
; INPUTS:
;   string - String to be processed
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;   return value is a string that is ready for mySQL
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2003/08/12
;-
function quote,str

   self='QUOTE: '
   if badpar(str,7,0,caller=self+'(str) ') then return,''

   t = "'"

   ; trim leading and trailing blanks
   newstr = strtrim(str,2)

   ; check for special value NULL
   if newstr eq 'NULL' then return,newstr

   ; are there leading and trailing single quotes?  If so, trim them off
   if strlen(newstr) ge 2 then begin
      if strmid(newstr,0,1) eq t and $
         strmid(newstr,strlen(newstr)-1,1) eq t then $
            newstr = strmid(newstr,1,strlen(newstr)-2)
   endif

   ; find all single quotes and \ and prepend a \
   patt = "('|\\)"
   buildstr=''
   repeat begin
      pos = stregex(newstr,patt)
      if pos ge 0 then begin
         if pos gt 0 then buildstr = buildstr+strmid(newstr,0,pos)
         buildstr = buildstr+'\'+strmid(newstr,pos,1)
         newstr = strmid(newstr,pos+1,strlen(newstr))
      endif
   endrep until pos lt 0
   buildstr = buildstr+newstr

   ; add leading and trailing single quote and return 
   newstr = t+buildstr+t
   return,newstr

end
