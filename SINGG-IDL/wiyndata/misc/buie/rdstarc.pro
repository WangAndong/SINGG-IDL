;+
; NAME:
;  rdstarc
; PURPOSE:
;  Read refnet format star catalog files.
; DESCRIPTION:
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  rdstarc,starfile,ra,dec,bmag,rmag
; INPUTS:
;  starfile - Name of file to read.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
;  ra   - right ascension in radians (J2000)
;  dec  - declination in radians (J2000)
;  bmag - Blue magnitude
;  rmag - Red magnitude  (see the USNO A2.0 catalog docs to see what this means)
;  nstars - Number of stars found (can be zero).
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2002/01/07
;  02/03/10 - MWB, added nstars return value
;-
pro rdstarc,starfile,ra,dec,bmag,rmag,nstars

   if exists(starfile) then begin
      info=file_info(starfile)
      if info.size eq 0 then begin
         nstars=0
      endif else begin
         readcol,starfile,hr,m1,s1,dgas,m2,s2,rmag,bmag, $
            format='d,d,d,a,d,d,f,f',/silent
         nstars=n_elements(hr)
         signas = strmid(dgas,0,1)
         dg = fix(strmid(dgas,1,2))
         hmstorad,hr,m1,s1,ra
         sign = replicate(1.0,nstars)
         z=where(signas eq '-',count)
         if count ne 0 then sign[z] = -1.0
         dmstorad,sign,abs(dg),m2,s2,dec
      endelse
   endif else begin
      print,'Star catalog file ',starfile,' cannot be found.'
      nstars=0
   endelse

end
