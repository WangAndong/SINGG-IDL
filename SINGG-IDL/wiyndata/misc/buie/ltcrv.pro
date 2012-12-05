;+
; NAME: 
;  ltcrv
; PURPOSE: 
;  Photometric lightcurve reductions against a single comparison star.
; DESCRIPTION:
; CATEGORY:
;  Photometry
; CALLING SEQUENCE:
;  pro ltcrv,stand,fil,jd,am,serial,mag,err,object,objno,comp,dofil, $
;       jdobs,redmag,rederr, $
;       NOPRINT=noprint, NOPLOT=noplot, BINFAC=binfac, FORCE=force
; INPUTS:
;  stand  - String array of standard names.  (See coord.)
;  fil    - String array of filter names for observations.
;  jd     - Double precision array of Julian dates.
;  am     - Floating point array of the airmass of observations.
;  serial - Serial number of observation.
;  mag    - Raw instrumental magnitudes.
;  err    - Uncertainties on the raw magnitudes.
;  object - Standard name of program object to reduce against comp star.
;  objno  - Serial number of program object.  (Usually 0)
;  comp   - Standard name of comparison star.
;  dofil  - Name of filter to reduce.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  BAD     - Set of flags that will mark data bad (if 1), good if 0.
;  BINFAC  - Maximum amount of comparison star point to bin (default=6)
;  FILTNAME- Proper name of filter, should be just one or two characters.
;              if not given, 1=B, 2=V, 3=R is used for the default
;  OBJNAME - Proper name of object, default = object:objno
;  FORCE   - Two element vector that contains override values for the
;              mean extinction and its uncertainty.  This replaces the
;              initial fit for mean extinction.
;  NOPLOT  - Flag, if true will inhibit the summary plot.
;  NOPRINT - Flag, if true, will inhibit the summary printout to the screen.
;
;  K2      - Second order extinction coefficient and error (default=[0,0])
;  CTERM   - Color term and error (default=[0,0])
;  OCOLOR  - Standard color and error for object (default=[0,0])
;  SCOLOR  - Standard color and error for star (default=[0,0])
;  STDMAG  - Standard magnitude and error for star (default=[0,0])
;
; OUTPUTS:
;  jdobs  - Final Julian date of reduced observation of program object.
;  redmag - Final reduced magnitude of object compared to comp star.
;  rederr - Final uncertainty.
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  93/07/28 - Written by Marc W. Buie.  Patterned after a similiar program
;              of David Tholen's.
;  93/10/25, MWB, added optional transformation coefficients.
;  93/12/10 - MWB, added extinction override (FORCE)
;  96/02/21 - MWB, total rewrite, added BAD flags
;-
pro ltcrv,stand,fil,jd,am,serial,mag,err,object,objno,comp,dofil, $
       jdobs,redmag,rederr, $
       NOPRINT=noprint, NOPLOT=noplot, BINFAC=binfac, $
       K2=in_k2, CTERM=in_cterm, OCOLOR=ocolor, SCOLOR=scolor, STDMAG=stdmag, $
       FORCE=in_force, FILTNAME=filtname, OBJNAME=objname, NOFILES=nofiles, $
       FILE=file,BAD=bad

   if n_params() eq 0 then begin
      print,'ltcrv,stand,fil,jd,am,serial,mag,err,object,objno,comp,dofil, $'
      print,'   jdobs,redmag,rederr, [NOPRINT, NOPLOT, BINFAC, K2, CTERM, $'
      print,'                         OCOLOR, SCOLOR, STDMAG, FORCE]'
      return
   endif

   if badpar(stand, 7,        1,caller='LTCRV: (stand) ', npts=n1) then return
   if badpar(fil,   7,        1,caller='LTCRV: (fil) ',   npts=n2) then return
   if badpar(jd,    5,        1,caller='LTCRV: (jd) ',    npts=n3) then return
   if badpar(am,    [4,5],    1,caller='LTCRV: (am) ',    npts=n4) then return
   if badpar(serial,[1,2,3],  1,caller='LTCRV: (serial) ',npts=n5) then return
   if badpar(mag,   [4,5],    1,caller='LTCRV: (mag) ',   npts=n6) then return
   if badpar(err,   [4,5],    1,caller='LTCRV: (err) ',   npts=n7) then return
   if badpar(object,7,        0,caller='LTCRV: (object) '        ) then return
   if badpar(objno ,[1,2,3],  0,caller='LTCRV: (objno) '         ) then return
   if badpar(comp  ,7,        0,caller='LTCRV: (comp) '          ) then return
   if badpar(dofil ,7,        0,caller='LTCRV: (dofil) '         ) then return
   if badpar(binfac,[0,1,2,3],0,caller='LTCRV: [BINFAC] ', default=6) then return
   if badpar(in_k2,    [0,4,5],1,caller='LTCRV: (k2) ',default=[999.,0.],npts=n8) then return
   if badpar(in_cterm, [0,4,5],1,caller='LTCRV: (cterm) ',default=[999.,0.],npts=n9) then return
   if badpar(ocolor,[0,4,5],1,caller='LTCRV: (ocolor) ',default=[0.,0.],npts=n10) then return
   if badpar(scolor,[0,4,5],1,caller='LTCRV: (scolor) ',default=[0.,0.],npts=n11) then return
   if badpar(stdmag,[0,4,5],1,caller='LTCRV: (stdmag) ',default=[0.,0.],npts=n12) then return
   if badpar(in_force, [0,4,5],1,caller='LTCRV: (force) ',default=[999.,0.],npts=n9) then return
   if badpar(filtname,[0,7],  0,caller='LTCRV: (filtname) ', $
                                default='default') then return
   if badpar(objname,[0,7],  0,caller='LTCRV: (objname) ', $
                                default='default') then return
   if badpar(file,  [0,7],    0,caller='LTCRV: (file) ', $
                                 default='no save') then return
   if badpar(append,[0,1,2,3],0,caller='LTCRV: (append) ',default=0) then return
   if badpar(bad,[0,1,2,3],1,caller='LTCRV: (BAD) ',default=intarr(n1),npts=n13) then return

   optpar=[n8,n9,n10,n11,n12]
   optbad=where(optpar ne 2, count_bad)
   if count_bad ne 0 then begin
      messarr=['K2','CTERM','OCOLOR','SCOLOR','STDMAG']
      for i=0,count_bad-1 do $
         print,'Optional parameter ',messarr[optbad[i]],' must contain only two elements.'
      return
   endif

   IF in_k2[0] eq 999.0 THEN k2=[0.,0.] ELSE k2=in_k2
   IF in_cterm[0] eq 999.0 THEN cterm=[0.,0.] ELSE cterm=in_cterm

   IF in_force[0] eq 999.0 THEN force=0 ELSE force=1

   if filtname eq 'default' then begin
      if dofil eq '1' then begin
         filstr = 'B'
      endif else if dofil eq '2' then begin
         filstr = 'V'
      endif else if dofil eq '3' then begin
         filstr = 'R'
      endif else if dofil eq '9' then begin
         filstr = 'M'
      endif else begin
         filstr = dofil
      endelse
   endif else begin
      filstr = filtname
   endelse

   plotit  = not keyword_set(noplot)
   saveit  = not keyword_set(nofiles)

   IF objname eq 'default' THEN $
      objname = object+':'+string(objno,form='(i4.4)')

   ; Name of file where a summary of the fit is to be written
   sumfile = nobname(object)+':'+string(objno,form='(i4.4)')+'_'+filstr+'.dft'

   ;Compute the time part of the Julian Date.
   time = (jd - long(jd[0]-0.5d0)-0.50d0)*24.0
   jdstr,jd[0],100,datestr

   ; Select out the star and program object observations to reduce.
   star=where(stand eq comp   and                     fil eq dofil and bad eq 0,cntstar)
   obj =where(stand eq object and serial eq objno and fil eq dofil and bad eq 0,cntobj)

   if cntstar eq 0 then begin
      print,'LTCRV: No data found for ',comp,' in filter ',dofil,'.  Aborting.'
      return
   endif

   if cntobj eq 0 then begin
      print,'LTCRV: No data found for ',object,':', $
            string(objno,format='(i4.4)'),' in filter ',dofil,'.  Aborting.'
      return
   endif

   ; Correct the star for second order extintion and color term.  This will
   ;   be used for the extinction fit.
   starmag = mag[star] - k2[0]*scolor[0]*am[star] + cterm[0]*scolor[0]
   starerr = sqrt( err[star]^2 + (k2[1]*scolor[0]*am[star])^2 $
                               + (k2[0]*scolor[1]*am[star])^2 $
                               + (cterm[1]*scolor[0])^2 $
                               + (cterm[0]*scolor[1])^2 )

   ; Find/Set the mean extinction coefficient.
   if force then begin
      meanext    = in_force[0]
      meanexterr = in_force[1]
      strcom = 'forced'
   endif else begin
      ; Do linear extinction fit to the comparison star.
      coeff=polyfitw(am[star],starmag,1.0/starerr,1,yfit,yband,sigma,a)
      meanext=coeff[1]
      meanexterr=a[1,1]
      strcom = 'fitted'
   endelse

   ; Compute mean airmass of comparison star.
   meanam=mean(am[star])

   ; Correct all star measurements to the mean airmass.
   dams=meanam-am[star]
   staratmean=starmag+dams*meanext
   staratmeanerr=sqrt( starerr^2 + (dams*meanexterr)^2 )

   ; Compute mean, avgdev, and stddev of corrected star.
   moment4,staratmean,meanstar,scatter,meanstarsig

   ; Set the Zero airmass magnitude
   if force then begin
      zammag = meanstar - meanext*meanam
      zamerr = sqrt((meanam*meanexterr)^2 + meanstarsig^2)
   endif else begin
      zammag=coeff[0]
      zamerr=a[0,0]
   endelse

   ; Compute chi-squared and yfit from mean fit.
   yfit = meanext*am[star] + zammag
   redchi = sqrt(total(((starmag-yfit)/starerr)^2)/(n_elements(star)-2))

   ; Bin the data to it's "natural" grouping.  That is, if there were 3
   ;   consequtive comp star measurments, they would be averaged into one
   ;   final point.  This will be used to derive the time-variable extinction.
   if binfac ne 1 then begin
      avger,time[star],starmag,starerr,binfac,3,tavgstar,avgmagstar,avgmagstarerr
      avger,time[star],staratmean,staratmeanerr,binfac,3,tavgstar,avgstar,avgstarerr
      avger,time[star],am[star],starerr,binfac,3,tavgstar,avgamstar
      avger,time[star],mag[star],err[star],binfac,3,tavgstar,avgrawstar,avgrawstarerr
   endif else begin ; Ignore binning if binfac is 1.
      tavgstar = time[star]
      avgmagstar=starmag
      avgmagstarerr=starerr
      avgstar=staratmean
      avgstarerr=staratmeanerr
      avgamstar=am[star]
      avgrawstar=mag[star]
      avgrawstarerr=err[star]
   endelse
   nsets = n_elements(tavgstar)

   ; From the binned comp and the binned comp corrected to the mean airmass,
   ;   compute the effective extinction.
   extin=(avgmagstar-zammag)/avgamstar
   extinerr = sqrt( ( avgmagstarerr^2 + zamerr^2 ) / avgamstar )

   ; Interpolate the time-variable extinction to the individual star measurement
   ;   times and apply the correction to see the scatter.
   interp,tavgstar,extin,E1=extinerr,time[star],kstar,kstarerr
   finalstar = starmag + kstar*(0.0-am[star]) + meanext*meanam
   finalstarerr = sqrt(starerr^2+(am[star]*kstarerr)^2)
   meanerr,finalstar,finalstarerr,finalmeanstar,finalmeanstarerr
   finalredchi = sqrt(total(((finalstar-finalmeanstar)/finalstarerr)^2)/ $
                                  (n_elements(finalstar)-2))
   finalscat = mean(abs(finalstar-finalmeanstar))

   ; Interpolate the time-variable extinction to the program object measurements
   interp,tavgstar,extin,E1=extinerr,time[obj],kobj,kobjerr

   ; Find the nearest (early) observation to each of the program object measurements
   staratobj = fltarr(cntobj)
   staratobjerr = fltarr(cntobj)
   stamatobj = fltarr(cntobj)
   FOR i=0,cntobj-1 DO BEGIN
      dt = tavgstar - time[obj[i]]
      zp = where(dt lt 0,count_prior)
      zf = where(dt gt 0,count_follow)
      IF count_prior gt 0 and count_follow gt 0 THEN BEGIN
         ip = where(dt eq max(dt[zp]))
         ia = where(dt eq min(dt[zf]))
         idx=[ip[0],ia[0]]
         meanerr,avgrawstar[idx],avgrawstarerr[idx],tmpmag,tmperr
         staratobj[i] = tmpmag
         staratobjerr[i] = tmperr
         stamatobj[i] = mean(avgamstar[[ip[0],ia[0]]])
      ENDIF ELSE IF count_prior gt 0 THEN BEGIN
         ip = where(dt eq max(dt[zp]))
         staratobj[i] = avgrawstar[ip[0]]
         staratobjerr[i] = avgrawstarerr[ip[0]]
         stamatobj[i] = avgamstar[ip[0]]
      ENDIF ELSE IF count_follow gt 0 THEN BEGIN
         ia = where(dt eq min(dt[zf]))
         staratobj[i] = avgrawstar[ia[0]]
         staratobjerr[i] = avgrawstarerr[ia[0]]
         stamatobj[i] = avgamstar[ia[0]]
      ENDIF ELSE BEGIN
         print,'fatal error, cannot happen'
         return
      ENDELSE
   ENDFOR

   ;Extract the time of observations.
   jdobs  = jd[obj]

   ; Reduce the object differentially against the star
   redmag = mag[obj] - staratobj $
                     - kobj*(am[obj]-stamatobj) $
                     - k2[0]*(ocolor[0]*am[obj] - scolor[0]*stamatobj) $
                     + cterm[0]*(ocolor[0] - scolor[0]) $
                     + stdmag[0]

   ; now the uncertainties
   rederr = err[obj]^2 + staratobjerr^2 $
                       + (kobjerr*(am[obj]-stamatobj))^2 $
                       + (k2[1]*(ocolor[0]*am[obj] - scolor[0]*stamatobj))^2 $
                       + (k2[0]*ocolor[1]*am[obj])^2 $
                       + (k2[0]*scolor[1]*stamatobj)^2 $
                       + (cterm[1]*(ocolor[0] - scolor[0]))^2 $
                       + (cterm[0]*ocolor[1])^2 $
                       + (cterm[0]*scolor[1])^2 $
                       + stdmag[1]^2
   rederr = sqrt(rederr)
   ;FOR i=0,cntobj-1 DO BEGIN
   ;   print,i,mag(obj(i)),err(obj(i)),am(obj(i)), $
   ;      staratobj(i),staratobjerr(i),stamatobj(i), $
   ;      redmag(i),rederr(i), $
   ;      format='(i2,1x,f7.4,1x,f6.4,1x,f4.2' + $
   ;              ',2x,f7.4,1x,f6.4,1x,f4.2' + $
   ;              ',2x,f7.4,1x,f6.4' + $
   ;              ')'
   ;ENDFOR

   ;Correct for extinction, second order extinction, color term and zero point.
   ;redmag = mag(obj) $
   ;         + kobj*(meanam-am(obj)) $ ; Extinction, correcting to mean airmass
   ;         - finalmeanstar $         ; Zero point from extinction fit.
   ;         + cterm(0)*(ocolor(0)-scolor(0)) $ ; Color term correction (differential).
   ;         + stdmag(0)            ; Standard magnitude of star.

   ;rederr = sqrt(err(obj)^2+((meanam-am(obj))*kobjerr)^2 + $
   ;              finalmeanstarerr^2 + $
   ;              (cterm(1)*(ocolor(0)-scolor(0)))^2 + $
   ;              cterm(0)^2 * (ocolor(1)^2 + scolor(1)^2) + $
   ;              stdmag(1)^2 )

   ; Print results to the screen.
   line=strarr(17)

   fmt1 = '(f8.4," +/- ",f6.4,1x,a)'
   fmt2 = '(f8.4)'
   line[0] = objname+' with respect to '+comp+'.  Filter - '+filstr+ $
             ', Date '+datestr
   line[1] = 'Extinction             = '+string(meanext,meanexterr,strcom,form=fmt1)
   IF k2[0] eq 0.0 THEN BEGIN
      line[2] = 'No second order extinction term'
   ENDIF ELSE BEGIN
      line[2] = 'Second order extinction= '+string(k2,format=fmt1)
   ENDELSE
   IF cterm[0] eq 0.0 THEN BEGIN
      line[3] = 'No color term'
   ENDIF ELSE BEGIN
      line[3] = 'Color term             = '+string(cterm,format=fmt1)
   ENDELSE
   line[4] = 'Zero airmass magnitude = '+string(zammag,zamerr,form=fmt1)
   line[5] = 'Mean magnitude         = '+string(meanstar,meanstarsig,form=fmt1)
   line[6] = 'Mean airmass           = '+string(meanam,form=fmt2)
   line[7] = 'Reduced chi-squared    = '+string(redchi,form=fmt2)
   line[8] = 'Scatter (avgdev)       = '+string(scatter,form=fmt2)
   line[9] = 'Number of observations = '+string(n_elements(star),form='(i3)')
   line[11] = 'Number of sets         = '+string(nsets,form='(i3)')
   line[12] = 'Corrected star mean    = '+string(finalmeanstar,finalmeanstarerr,form=fmt1)
   line[13] = 'Reduced chi-squared    = '+string(finalredchi,form=fmt2)
   line[14] = 'Scatter (avgdev)       = '+string(finalscat,form=fmt2)
   line[16] = 'Mean object error      = '+string(mean(rederr),form=fmt2)

   if not keyword_set(noprint) then $
      for i=0,n_elements(line)-1 do print,line[i]

   ; This is for the output summary file.
   if saveit then begin
      openw,lusum,sumfile,/get_lun
      for i=0,n_elements(line)-1 do printf,lusum,line[i]
      printf,lusum,' '
      printf,lusum,'  Set#   Time   Airmass   Star @ mean X   Diff       Extinction'
      FOR i=0,nsets-1 DO BEGIN
         printf,lusum,i,tavgstar[i],avgamstar[i],avgstar[i],avgstar[i]-meanstar,extin[i],extinerr[i], $
            format='(3x,i2,3x,f5.2,5x,f4.2,7x,f7.4,4x,f7.4,3x,f6.4," +/- ",f6.4)'
      ENDFOR
      printf,lusum,' '
      printf,lusum,objname,ocolor,format='(3x,a,2x,"color =",1x,f7.4," +/- ",f6.4)'
      printf,lusum,comp,filstr,stdmag,scolor, $
         format='(3x,a,2x,a,"=",f7.4," +/- ",f6.4,3x,"color =",1x,f7.4," +/- ",f6.4)'
      printf,lusum,k2,format='(3x,"second order extinction =",1x,f7.4," +/- ",f6.4)'
      printf,lusum,cterm,format='(3x,"color term              =",1x,f7.4," +/- ",f6.4)'
      free_lun,lusum
   endif

   print,' '
   print,'  Set#   Time   Airmass   Star @ mean X   Diff       Extinction'
   FOR i=0,nsets-1 DO BEGIN
      print,i,tavgstar[i],avgamstar[i],avgstar[i],avgstar[i]-meanstar,extin[i],extinerr[i], $
         format='(3x,i2,3x,f5.2,5x,f4.2,7x,f7.4,4x,f7.4,3x,f6.4," +/- ",f6.4)'
   ENDFOR

   ; Saving data
   if file ne 'no save' then begin
      if append then $
         print,filstr,' data appended to file ',file $
      else $
         print,filstr,' data written  to file ',file
      wrphot,jdobs,filstr,redmag,rederr,file,APPEND=append;,bad=bad(z)
   endif

   ; Plotting stuff.
   if plotit then begin
      pmult=!p.multi
      !p.multi=[0,2,4]

      cs=1.5
      ss=0.5

      plot,am[star],mag[star],psym=8,yr=[max(mag[star]),min(mag[star])], $
              xtit='Airmass',ytit='Inst. mag',chars=cs,syms=ss
      oplerr,am[star],mag[star],err[star],psym=3

      plot,time[star],am[star],psym=8,yr=[max(am[star]),min(am[star])], $
              xtit='UT time (hours)',ytit='Airmass',chars=cs,syms=ss

      plot,am[star],mag[star]-yfit,psym=8,xtit='Airmass',ytit='mag residuals', $
              chars=cs,syms=ss
      oplerr,am[star],mag[star]-yfit,err[star],psym=3

      plot,time[star],mag[star]-yfit,psym=8,xtit='UT time (hours)', $
              ytit='mag residuals',chars=cs,syms=ss
      oplerr,time[star],mag[star]-yfit,err[star],psym=3

      yr=[min(extin-extinerr),max(extin+extinerr)]
      ploterror,tavgstar,extin,extinerr,psym=7,chars=cs,xtit='UT time (hours)', $
              ytit='Extinction',syms=ss,xr=minmax([time[obj],tavgstar]),yr=yr, $
              symsize=2
      setusym,-1
      oplerr,time[obj],kobj,kobjerr,psym=8,syms=ss*0.8
      setusym,1

      yr=[max(finalstar-meanstar+finalstarerr),min(finalstar-meanstar-finalstarerr)]
      plot,time[star],finalstar-meanstar,psym=8,chars=cs,yr=yr, $
              xtit='UT time (hours)',ytit='star mag residuals',syms=ss
      oplerr,time[star],finalstar-meanstar,finalstarerr,psym=3

      yr=[max(redmag+rederr),min(redmag-rederr)]
      plot,time[obj],redmag,psym=8,chars=cs,yr=yr,xtit='UT time (hours)', $
              ytit=object+' - '+comp,syms=ss,xr=minmax([time[obj],tavgstar])
      oplerr,time[obj],redmag,rederr,psym=3

      x=.6
      y=0.23-findgen(n_elements(line))/80
      for i=0,n_elements(line)-1 do xyouts,x,y[i],line[i],/normal,chars=0.5

      !p.multi=pmult
   endif

end
