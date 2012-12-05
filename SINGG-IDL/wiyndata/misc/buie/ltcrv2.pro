;+
; NAME:
;  ltcrv2
; PURPOSE: (one line)
;  Photometric lightcurve reductions with known transformation.
; DESCRIPTION:
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;     ltcrv2,stand,fil,jd,time,am,serial,inst,instsig,color,colorsig, $
;        doobj,doser,dofil,tran,transig,jdref,jdobs,tobs,std,stdsig
; INPUTS:
;  stand    - String array of standard names.  (See coord.)
;  fil      - String array of filter names for observations.
;  jd       - Double precision array of the JD of observations.
;  time     - Floating point array of the UT time of observations.
;  am       - Floating point array of the airmass of observations.
;  serial   - Serial number of observation.
;  inst     - Instrumental magnitude
;  instsig  - Uncertainty of the instrumental magnitude
;  color    - Standard system color for object.
;  colorsig - Uncertainty on the standard color
;  doobj    - Name of object to reduce.
;  doser    - Serial number of object to reduce.
;  dofil    - Name of filter to reduce.
;  tran     - Transformation coefficients (vector)
;                tran(0) = principal extinction coefficient
;                tran(1) = second order extinction coefficient
;                tran(2) = color term
;                tran(3) = zero-point
;  transig  - Uncertainty on the transformation coefficients (vector).
;  jdref    - Time reference point for extinction
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  NOPLOT   - Flag, if set, suppresses a plot of the final lightcurve.
;  FILTNAME - String name for selected filter, default for 2=V and 3=R.
;  FILE     - If supplied, the reduced lightcurve will be saved to this file.
;  APPEND   - Flag, if true, data will be appended to FILE
;  BADFLAGS - Array of flags that mark data bad (if true).
;  NOEDIT - Flag, if set inhibits final interactive editing of fitted points.
;             This keyword has no effect and is not necessary if the current
;             plotting device is 'PS'.
; KEYWORD OUTPUT PARAMETERS:
;  BADFLAGS - Array of flags that mark data bad (if true).
; OUTPUTS:
;  jdobs    - JD of observation for each point.
;  tobs     - UT Time of observation for each point.
;  std      - Standard magnitude.
;  stdsig   - Uncertainty of the standard magnitude.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written: 1992 Mar 31, Marc W. Buie, Lowell Observatory.
;  96/10/16 - MWB - added NOPLOT keyword
;  96/01/24, MWB, added BADFLAGS
;-
pro ltcrv2,stand,fil,jd,time,am,serial,inst,instsig,in_color,in_colorsig, $
   doobj,doser,dofil,tran,transig,jdref,jdobs,tobs,std,stdsig, $
   NOPLOT=noplot,FILTNAME=filtname,FILE=file,APPEND=append,BADFLAGS=bad,NOEDIT=noedit

   if n_params() eq 0 then begin
      print,'ltcrv2,stand,fil,jd,am,serial,inst,instsig,in_color,in_colorsig, $'
      print,'   doobj,doser,dofil,tran,transig,jdobs,std,stdsig'
      return
   endif

   if badpar(stand, 7,        1,caller='LTCRV2: (stand) ', npts=n1) then return
   if badpar(fil,   7,        1,caller='LTCRV2: (fil) ',   npts=n2) then return
   if badpar(jd,    5,        1,caller='LTCRV2: (jd) ',    npts=n3) then return
   if badpar(time,  [4,5],    1,caller='LTCRV2: (time) ',  npts=n12) then return
   if badpar(am,    [4,5],    1,caller='LTCRV2: (am) ',    npts=n4) then return
   if badpar(serial,[1,2,3],  1,caller='LTCRV2: (serial) ',npts=n5) then return
   if badpar(inst,  [4,5],    1,caller='LTCRV2: (inst) ',  npts=n6) then return
   if badpar(instsig,[4,5],   1,caller='LTCRV2: (instsig) ',npts=n7) then return
   if badpar(in_color,[1,2,3,4,5],[0,1], $
                                caller='LTCRV2: (color) ',npts=n8) then return
   if badpar(in_colorsig,[1,2,3,4,5],[0,1], $
                                caller='LTCRV2: (colorsig) ',npts=n9) then return
   if badpar(doobj, 7,        0,caller='LTCRV2: (doobj) '         ) then return
   if badpar(doser, [1,2,3],  0,caller='LTCRV2: (doser) '         ) then return
   if badpar(dofil, 7,        0,caller='LTCRV2: (dofil) '         ) then return
   if badpar(tran,  [4,5],    1,caller='LTCRV2: (tran) ',npts=n10 ) then return
   if badpar(transig,[4,5],   1,caller='LTCRV2: (transig) ',npts=n11) then return
   if badpar(jdref, 5,        0,caller='LTCRV2: (jdref) ') then return
   if badpar(filtname,[0,7],  0,caller='LTCRV2: (filtname) ', $
                                default='default') then return
   if badpar(file,  [0,7],    0,caller='LTCRV2: (file) ', $
                                default='no save') then return
   if badpar(append,[0,1,2,3],0,caller='LTCRV2: (append) ',default=0) then return
   if badpar(bad,[0,1,2,3],[0,1],caller='LTCRV2: (BADFLAGS) ', $
                                default=intarr(n1)) then return
   if badpar(noedit,[0,1,2,3],0,caller='LTCRV2: (NOEDIT) ', $
                                default=0) then return

   alln=[n1,n2,n3,n4,n5,n6,n7,n12]
   if min(alln) ne max(alln) then begin
      print,'LTCRV2: Error!  stand,fil,jd,am,serial,mag,err must be the same length.'
      return
   endif

   if n8 eq 1 then color = replicate(in_color,n1) $
   else if n8 ne n1 then begin
      print,'LTCRV2: Error!  color must be scalar or same length as the others.'
      return
   endif else color=in_color

   if n9 eq 1 then colorsig = replicate(in_colorsig,n1) $
   else if n9 ne n1 then begin
      print,'LTCRV2: Error!  colorsig must be scalar or same length as the others.'
      return
   endif else colorsig=in_colorsig

   if n10 ne 5 or n11 ne 5 then begin
      print,'LTCRV2: Error!  tran and transig must 4 element vectors.'
      return
   endif

   ; Select out all the measurements for the requested filter and object.
   z=where(fil eq dofil and stand eq doobj and serial eq doser and bad ne 1,count)
   if count eq 0 then begin
      print,'No measurements found for ',doobj,' in filter ',dofil,'.  Quitting.'
      return
   endif

   edit = not keyword_set(noedit) and !d.name ne 'PS'

   if filtname eq 'default' then begin
      if dofil eq '2' then begin
         filstr = 'V'
      endif else if dofil eq '3' then begin
         filstr = 'R'
      endif else begin
         filstr = dofil
      endelse
   endif else begin
      filstr = filtname
   endelse

   ; Compute standard magnitudes for observations.
   inst2std,jd[z],am[z],inst[z],instsig[z],color[z],colorsig[z], $
      tran,transig,jdref,std,stdsig

   jdobs = jd[z]
   tobs  = time[z]

   jd0=long(jdobs[0]+0.5)-0.5
   jdstr,jd0,100,date

   if not keyword_set(noplot) then begin
      ploterror,tobs,std,stdsig,psym=8,yr=maxmin(std), $
         xtit='UT time in hours',ytit='Apparent '+filstr+' magnitude', $
         tit=doobj+', '+date,xr=minmax(tobs),charsize=1.5
   endif

   if edit then begin
      l_bad = bad[z]
      markdata,tobs,std,l_bad,/yflip, $
         xtitle='UT time in hours',ytitle='Apparent '+filstr+' magnitude'
      bad[z] = l_bad
   endif

   if file ne 'no save' then begin
      if append then $
         print,filstr,' data appended to file ',file $
      else $
         print,filstr,' data written  to file ',file
      wrphot,jdobs,filstr,std,stdsig,file,APPEND=append,bad=bad[z]
   endif

end
