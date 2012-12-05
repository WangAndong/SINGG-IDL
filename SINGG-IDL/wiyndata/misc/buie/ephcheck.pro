;+
; NAME:
;  ephcheck
; PURPOSE:
;  Compare a set of astrometry observations against an ephemeris.
; DESCRIPTION:
;
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  ephcheck,objcode,obs,astfile
; INPUTS:
;  objcode - Standard object code for ephemeris to check against.  See EPHEM.
;  obs     - Standard observatory code (see RDOBSCOD)
; OPTIONAL INPUT PARAMETERS:
;  astfile - Raw astrometry file to read and compare to ephemeris, default
;              is "objcode.ast"  where the input objcode is first stripped of
;              the leading prefix character.
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;  All information is printed to screen.
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
;  98/01/14, Written by Marc W. Buie, Lowell Observatory
;
;-
PRO ephcheck,objcode,obs,astfile

   if n_params() eq 0 then begin
      print,'ephcheck,objcode,obs,astfile'
      return
   endif

   if badpar(objcode,7,0,CALLER='EPHCHECK: (objcode) ') then return
   if badpar(obs,[2,3],0,CALLER='EPHCHECK: (obs) ') then return
   if badpar(astfile,[0,7],0,CALLER='EPHCHECK: (astfile) ', $
                             default=strmid(objcode,1,99)+'.ast') then return

   rdrawast,astfile,fn,jd,raobs,decobs,mag,nlines
   IF nlines eq 0 THEN return

   if nlines eq 1 then begin
      print,'The astrometry file has only one observation.'
   endif

   ; Generate the ephemeris
   ephem,jd,obs,2+50,objcode,eph
   err=angsep(raobs,decobs,eph[0,*],eph[1,*])*!radeg*3600.0
   IF n_elements(err) gt 3 THEN BEGIN
      robomean,err,3.0,0.5,merr
   ENDIF ELSE BEGIN
      merr=mean(err)
   ENDELSE

   ; Compute total motion from first to last point.
   hmotobj=angsep(raobs[0],decobs[0],raobs[nlines-1],decobs[nlines-1]) $
        / ((jd[nlines-1]-jd[0]) * 24.0 ) * !radeg * 3600.0
   hmoteph=angsep(eph[0,0],eph[1,0],eph[0,nlines-1],eph[1,nlines-1]) $
        / ((jd[nlines-1]-jd[0]) * 24.0 ) * !radeg * 3600.0

   ; Compute direction of motion using first and last points.
   objdir = atan(decobs[nlines-1]-decobs[0],raobs[0]-raobs[nlines-1])*!radeg
   ephdir = atan(eph[1,nlines-1] -eph[1,0], eph[0,0]-eph[0,nlines-1])*!radeg

   FOR i=0,nlines-1 DO BEGIN
      dis=angsep(raobs[0],decobs[0],raobs[i],decobs[i])*!radeg*3600.0
      jdstr,jd[i],0,str1
      print,fn[i],str1,err[i],err[i]-merr,dis, $
         format='(a,1x,a,1x,f10.3,2x,f7.3,2x,f6.1)'
   ENDFOR

   print,'Mean offset from ephemeris ... ',merr,' arcsec', $
         format='(a,f8.1,a)'
   print,'Angular motion rate Eph= ',hmoteph,' Obj= ',hmotobj,'  arcsec/hr', $
         format='(a,f6.1,a,f6.1,a)'
   print,'Motion direction    Eph= ',fix(ephdir+0.5), ' Obj= ',fix(objdir+0.5), '  degrees EofN', $
         format='(a,i4,2x,a,i4,2x,a)'
END
