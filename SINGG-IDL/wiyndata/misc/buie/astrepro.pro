;+
; NAME:
;  astrepro
; PURPOSE:
;  Re-reduce existing astrometry originally measured with ASTROM
; DESCRIPTION:
;  This file attempts to rereduce astrometric measurements in the current
;    directory.  These measures are x,y positions that are found in the
;    file, position.dat.  The files fitcoeff.dat and centers.dat are
;    scanned for the corresponding transformation relations between x,y
;    and RA,DEC.  Any object.ast files in this directory will be overwritten.
;
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  astrepro
; INPUTS:
;  All input information comes from files.
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;  All output information is sent to files.
;
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
;  97/06/20, Written by Marc W. Buie, Lowell Observatory
;  99/03/18, MWB, extracted fitcoeff reader (see rdastfc.pro)
;  2000/01/19, MWB, modified for new rdastfc version.
;
;-
PRO astrepro

   ; first check to see if the three required files exist.  If not, quit.
   err=''
   IF not exists('centers.dat') THEN err=[err,'centers.dat']
   IF not exists('position.dat') THEN err=[err,'position.dat']
   IF not exists('fitcoeff.dat') THEN err=[err,'fitcoeff.dat']

   IF n_elements(err) ne 1 THEN BEGIN
      IF n_elements(err) eq 2 THEN BEGIN
         print,'Input file ',err[1],' is not present.'
      ENDIF ELSE IF n_elements(err) eq 3 THEN BEGIN
         print,'Input files ',err[1],' and ',err[2],' are not present.'
      ENDIF ELSE IF n_elements(err) eq 4 THEN BEGIN
         print,'Input files ',err[1],', ',err[2],', and ',err[3],' are not present.'
      ENDIF
      print,'Unable to continue.'
      return
   ENDIF

   ; Pre-declare strings for later use.
   version=''
   line=''

   ; First load the position.dat file.
   readcol,'position.dat',pfn,pname,pobjrad,px,py,format='a,a,f,d,d',/silent
   npos=n_elements(pfn)
   print,npos,' total object positions found.'

   ; Next load the fit coefficients file.
   rdastfc,'fitcoeff.dat',ffn,ftype,xc,yc,cra,cdec,photzp,flagarr,coeffarr,ncoeffs,nlines

   IF nwords ne nterms+14 THEN BEGIN
      print,'fitcoeff.dat file is empty, aborting.'
      return
   ENDIF
   print,nlines,' total coefficient sets found.'

   ; Create a list of unique objects in position data.
   oblist=pname[uniq(pname,sort(pname))]
   nobj=n_elements(oblist)

   ; Now, loop over the objects
   FOR j=0,nobj-1 DO BEGIN
      obfile = oblist[j]+'.ast'
      obnew  = oblist[j]+'.astnew'
      obold  = oblist[j]+'.old'

      IF exists(obfile) THEN BEGIN
         readcol,obfile,ofn,jd,mag,format='a,a,x,x,a',/silent
         print,obfile,': ',n_elements(ofn),' points'

         openw,lun,obnew,/get_lun
      
         FOR i=0,n_elements(ofn)-1 DO BEGIN

            ; Locate index for coefficients information
            z=where(ofn[i] eq ffn,count)
            IF count ne 2 THEN BEGIN
               print,'Error: No coefficients found for file: ',ofn[i]
               ploc = -1
            ENDIF ELSE BEGIN
               IF ftype[z[0]] eq 'eta' THEN BEGIN
                  floce = z[0]
                  flocx = z[1]
               ENDIF ELSE BEGIN
                  floce = z[1]
                  flocx = z[0]
               ENDELSE
               ploc = 0
            ENDELSE

            ; Locate index for centroid information
            IF ploc ne -1 THEN BEGIN
               z=where(ofn[i] eq pfn and oblist[j] eq pname,count)
               IF count ne 1 THEN BEGIN
                  print,'Error: No ',oblist[j],' centroid found for file: ',ofn[i]
                  ploc = -1
               ENDIF ELSE BEGIN
                  ploc = z[0]
               ENDELSE
            ENDIF

            IF ploc ne -1 THEN BEGIN
               cxi  = coeffarr[flocx,0:ncoeffs[flocx]-1]
               ceta = coeffarr[floce,0:ncoeffs[floce]-1]
               cxi  = cxi[*]
               ceta = ceta[*]
               xiterms  = flagarr[flocx,*]
               etaterms = flagarr[floce,*]
               xiterms  = xiterms[*]
               etaterms = etaterms[*]

               xi = asteval(px[ploc]-xc[flocx],py[ploc]-yc[flocx],cxi,xiterms)/3600.0d0*!dpi/180.0d0
               eta= asteval(px[ploc]-xc[floce],py[ploc]-yc[floce],ceta,etaterms)/3600.0d0*!dpi/180.0d0
               astsn2rd,xi,eta,cra[flocx],cdec[floce],ra,dec
               rastr,ra,4,ras
               decstr,dec,3,decs
               printf,lun,ofn[i],jd[i],ras,decs,mag[i], $
                  format='(a,1x,a,1x,a,1x,a,1x,a4)'
;               print,ofn[i],' ',jd[i],' ',ras,' ',decs,' ',mag[i]
            ENDIF

         ENDFOR ; loop over object points
         free_lun,lun
         spawn,'mv '+obfile+' '+obold+' ; mv '+obnew+' '+obfile

      ENDIF ELSE BEGIN
         print,'File ',obfile,' not found.'
      ENDELSE

   ENDFOR ; object for loop

END
