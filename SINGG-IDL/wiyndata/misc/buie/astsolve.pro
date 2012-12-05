;+
; NAME:
;  astsolve
; PURPOSE:
;  Solve for astrometric transformation from image to sky coordinates.
; DESCRIPTION:
;
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  astsolve,x,y,xi,eta,xiterms,etaterms,renormfac,bad,cxi,ceta
; INPUTS:
;  x        - Image x-coordinate  (should be "normalized" to range from -1 to 1)
;  y        - Image y-coordinate  (should be "normalized" to range from -1 to 1)
;  xi       - Standard tanget plane coordinate (should be in arcsec)
;  eta      - Standard tanget plane coordinate (should be in arcsec)
;  xiterms  - Which fitting terms to use (see ASTTERMS.PRO)
;  etaterms - Which fitting terms to use (see ASTTERMS.PRO)
;  renormfac - Re-normalization factor for converting from normalized x,y to
;                the original x,y values.
;  bad      - array of flags that mark bad data on input (modified).
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;  EDIT - Flag, if set allows interactive bad point editing.
;  XFLIP - Flag, if set flips x axis plot when editing bad points.
;  YFLIP - Flag, if set flips y axis plot when editing bad points.
;
; OUTPUTS:
;  cxi  - coefficients of xi fit.
;  ceta - coefficients of eta fit.
;  bad  - array of flags that mark bad data on output.
;
; KEYWORD OUTPUT PARAMETERS:
;  WORSTRESID - Worst residual in "good" data in either axis (arcsec)
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
;  98/03/13, Written by Marc W. Buie, Lowell Observatory
;  98/11/23, MWB, added renormfac and fixed documentation
;  2000/09/14, MWB, added WORSTRESID keyword
;  2001/08/27, MWB, changed to auto-scale errors with /edit
;  2003/06/24, MWB, changed call to MARKDATA to use new features.
;  2003/10/27, MWB, fixed subtle bug during bad point cleanup.  The test
;                       was done on O-C against the std deviation of a
;                       robust mean.  The test needs to remove the mean of
;                       the surviving sample.
;
;-
PRO astsolve,x,y,xi,eta,xiterms,etaterms,renormfac,bad,cxi,ceta, $
       EDIT=edit,XFLIP=xflip,YFLIP=yflip,WORSTRESID=worstresid

   IF badpar(x,[4,5],1,caller='ASTSOLVE: (x) ') THEN return
   IF badpar(y,[4,5],1,caller='ASTSOLVE: (y) ') THEN return
   IF badpar(xi,[4,5],1,caller='ASTSOLVE: (xi) ') THEN return
   IF badpar(eta,[4,5],1,caller='ASTSOLVE: (eta) ') THEN return
   IF badpar(bad,[1,2,3],1,caller='ASTSOLVE: (bad) ') THEN return
   IF badpar(xiterms,[2,3],1,caller='ASTSOLVE: (xiterms) ') THEN return
   IF badpar(etaterms,[2,3],1,caller='ASTSOLVE: (etaterms) ') THEN return
   IF badpar(renormfac,[0,2,3,4,5],0,caller='ASTSOLVE: (renormfac) ') THEN return
   IF badpar(edit,[0,1,2,3],0,caller='ASTSOLVE: (EDIT) ',default=0) THEN return
   IF badpar(xflip,[0,1,2,3],0,caller='ASTSOLVE: (XFLIP) ',default=0) THEN return
   IF badpar(yflip,[0,1,2,3],0,caller='ASTSOLVE: (YFLIP) ',default=0) THEN return

   xfit=1
   efit=1
   var=1
   chisq=1
   sing=1

   pass=1
   REPEAT BEGIN
      worstresid=0.0
      oldbad=bad

      ; Now setup independent variable vectors for x
      zg = where(bad eq 0,countgood)
      xind=astterms(x[zg],y[zg],xiterms)
      weight  = replicate(1.0,countgood)
if countgood lt 2 then begin
   message,'Error! not enough points!'
endif

      cxi = mysvdfit(xind,xi[zg],1,weight=weight, $
                     yfit=xfit,var=var,chisq=chisq,sing=sing)

      robomean,xi[zg]-xfit,3.0,0.5,avg,avgdev,stddev,vars,skew,kurt,nfinal
      cxisig   = sqrt(var)
      worstresid = max([worstresid,abs(xi[zg]-xfit)])
      print,'xi:  chisq=',chisq/float(countgood-1),', scatter=',stddev, $
         ', worst resids',minmax(xi[zg]-xfit),countgood,pass, $ $
         format='(a,f6.2,a,f4.2,a,2(1x,f5.2),1x,i4," stars, pass ",i2)'

      IF edit THEN BEGIN
         setwin,10,xsize=400,ysize=800
         !p.multi=[0,1,5]
         plot,x[zg]*renormfac,xi[zg]-xfit,psym=7,symsize=0.5
         plot,y[zg]*renormfac,xi[zg]-xfit,psym=7,symsize=0.5
         plot,sqrt(x[zg]^2+y[zg]^2)*renormfac,xi[zg]-xfit,psym=7,symsize=0.5
         plot,(x[zg]*renormfac)^2,xi[zg]-xfit,psym=7,symsize=0.5
         plot,(y[zg]*renormfac)^2,xi[zg]-xfit,psym=7,symsize=0.5
      ENDIF

      zbad=where(abs(xi[zg]-xfit-avg) gt 3.0*stddev,countbad)
      IF countbad ne 0 THEN bad[zg[zbad]]=1

      ; Eta fit setup
      zg = where(bad eq 0,countgood)
      xind=astterms(x[zg],y[zg],etaterms)
      weight = replicate(1.0,countgood)

      ; Fit to eta coordinate
      ceta = mysvdfit(xind,eta[zg],1,weight=weight, $
                      yfit=efit,var=var,chisq=chisq,sing=sing)
      robomean,eta[zg]-efit,3.0,0.5,avg,avgdev,stddev,vars,skew,kurt,nfinal
      cetasig = sqrt(var)
      worstresid = max([worstresid,abs(eta[zg]-efit)])
      print,'eta: chisq=',chisq/float(countgood-1),', scatter=',stddev, $
         ', worst resids',minmax(eta[zg]-efit),countgood,pass, $ $
         format='(a,f6.2,a,f4.2,a,2(1x,f5.2),1x,i4," stars, pass ",i2)'
      IF edit THEN BEGIN
         setwin,11,xsize=400,ysize=800
         plot,x[zg]*renormfac,eta[zg]-efit,psym=7,symsize=0.5
         plot,y[zg]*renormfac,eta[zg]-efit,psym=7,symsize=0.5
         plot,sqrt(x[zg]^2+y^2)*renormfac,eta[zg]-efit,psym=7,symsize=0.5
         plot,(x[zg]*renormfac)^2,eta[zg]-efit,psym=7,symsize=0.5
         plot,(y[zg]*renormfac)^2,eta[zg]-efit,psym=7,symsize=0.5
         !p.multi=0
      ENDIF
      zbad=where(abs(eta[zg]-efit-avg) gt 3.0*stddev,countbad)
      IF countbad ne 0 THEN bad[zg[zbad]]=1

      ; Manual edit (if requested)
      IF edit THEN BEGIN
         zchg = where(bad ne oldbad, countchange)
         if countchange eq 0 then $
            print,'All points kept on this pass.'
         newbad=bad[zg]
;         xirange=max(xi[zg])-min(xi[zg])
;         etarange=max(eta[zg])-min(eta[zg])
;         range = max([xirange,etarange])
;         errbar = sqrt((xi[zg]-xfit)^2+(eta[zg]-efit)^2)
;         if (max(errbar) lt 0.05*range) then $
;            errbar = errbar/max(errbar)*0.05*range
;         markdata,xi[zg],eta[zg],errbar, $
;            newbad,xtitle='xi (arcsec)',ytitle='eta (arcsec)', $
;            xsize=600,ysize=600,xflip=xflip,yflip=yflip
         markdata,xi[zg],abs(xi[zg]-xfit),eta[zg],abs(eta[zg]-efit), $
            newbad,xtitle='xi (arcsec)',ytitle='eta (arcsec)', $
            xsize=600,ysize=600,xflip=xflip,yflip=yflip,plottype=12
         bad[zg]=newbad
      ENDIF

      zg = where(bad eq 0,countgood)

      ; Check to see if any additional bad points were flagged.
      zchg = where(bad ne oldbad, countchange)
      pass=pass+1
   ENDREP UNTIL countchange eq 0

END
