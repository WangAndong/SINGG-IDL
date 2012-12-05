;+
; NAME:
;  astprmt
; PURPOSE:
;  Promote version of an astrometry fit coefficient file to highest version.
; DESCRIPTION:
;
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  astprmt,fitfile,centers
; INPUTS:
;
;  fitfile - File with astrometry fit coefficients (Default=fitcoeff.dat)
;
;  centers - File with image centers.  (Default=centers.dat)
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;
;  The fitfile is updated to the most recent version.  Not changed if already
;    current.
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
;  97/10/09, Written by Marc W. Buie, Lowell Observatory
;  2000/01/19, MWB, added version 1.1 support
;
;-
pro astprmt,fitfile,centers

   if badpar(fitfile,[0,7],0,caller='ASTPRMT (fitfile) ',default='fitcoeff.dat') then return
   if badpar(centers,[0,7],0,caller='ASTPRMT (centers) ',default='centers.dat') then return

   ; If not present, don't do anything.
   IF not exists(fitfile) THEN return

   ; Check the fit coeff file version.
   version=''
   openr,lun,fitfile,/get_lun
   readf,lun,version,format='(a)'

   ; It's current, do nothing.
   latest='ASTFIT v1.1'
   IF version eq latest THEN BEGIN
      free_lun,lun
      return
   ENDIF

   ; Count the number of lines in file.
   line=''
   nlines=1L
   WHILE not eof(lun) DO BEGIN
      readf,lun,line,format='(a1)'
      nlines=nlines+1
   ENDWHILE

   if version ne 'ASTFIT v1.0' then begin
      version=strtrim(strcompress(version),2)
      words=strsplit(version,/extract)
      nwords=n_elements(words)
      IF nwords le 15 THEN BEGIN
         print,'fitcoeff.dat file is of an unrecognized format, aborting.'
         print,'version tag seen: [',version,']'
         return
      ENDIF
      nterms = fix(total(fix(words[4:13])))
      IF nwords ne nterms+14 THEN BEGIN
         print,'fitcoeff.dat file is of an unrecognized format, aborting.'
         print,'version tag seen: [',version,']'
         return
      ENDIF
      version='ASTFIT v0.0'
      point_lun,lun,0
   endif else begin
      if version ne 'ASTFIT v1.0' then begin
         print,'fitcoeff.dat file is of an unrecognized format, aborting.'
         print,'version tag seen: [',version,']'
         return
      endif
      nlines=nlines-1
      point_lun,lun,0
      readf,lun,version,format='(a)'
   endelse

   ffn      = strarr(nlines)
   ftype    = strarr(nlines)
   xc       = strarr(nlines)
   yc       = strarr(nlines)
   cra      = strarr(nlines)
   cdec     = strarr(nlines)
   flagarr  = intarr(nlines,10)
   coeffarr = strarr(nlines,10)
   ncoeffs  = intarr(nlines)
   photzp   = replicate(99.0,nlines)
   print,'ASTPRMT: Upgrading file from ',version,' to ',latest
   print,nlines,' total coefficient sets found.'

   IF version eq 'ASTFIT v0.0' THEN BEGIN
      FOR i=0,nlines-1 DO BEGIN
         readf,lun,line,format='(a)'
         line=strtrim(strcompress(line),2)
         words=strsplit(line,/extract)
         ffn[i] = words[0]
         ftype[i] = words[1]
         xc[i]    = words[2]
         yc[i]    = words[3]
         flagarr[i,*]=fix(words[4:13])
         ncoeffs[i]=fix(total(flagarr[i,*])+0.5)
         coeffarr[i,0:ncoeffs[i]-1] = words[14:14+ncoeffs[i]-1]
      ENDFOR
      free_lun,lun

      ; Load the centers.dat file
      readcol,'centers.dat',cfn,cras,cdecs,format='a,a,a',/silent
      ncen=n_elements(cfn)
      print,ncen,' total centers found.'

      ; Copy the centers to cra,cdec
      FOR i=0,nlines-1 DO BEGIN
         ; Locate index for center information
         z=where(ffn[i] eq cfn,count)
         IF count eq 0 THEN BEGIN
            print,'Error: No center found for file: ',ffn[i]
            ffn[i] = ''
         ENDIF ELSE IF count eq 1 THEN BEGIN
            cra[i]  = cras[z[0]]
            cdec[i] = cdecs[z[0]]
         ENDIF ELSE BEGIN
            print,'Error: Multiple centers found for file: ',ffn[i]
            ffn[i] = ''
         ENDELSE
      ENDFOR

      ; Now write out the new file
      openw,lun,fitfile,/get_lun
      printf,lun,latest
      FOR i=0,nlines-1 DO BEGIN
         IF ffn[i] ne '' THEN BEGIN
            printf,lun,ffn[i],ftype[i],xc[i],yc[i],cra[i],cdec[i], $
               flagarr[i,*],coeffarr[i,0:ncoeffs[i]-1], $
               format='(6(a,1x),10(i1,1x),10(1x,e19.12))'
         ENDIF
      ENDFOR
      free_lun,lun

   endif else if version eq 'ASTFIT v1.0' then begin

      ; Read current file.
      FOR i=0,nlines-1 DO BEGIN
         readf,lun,line,format='(a)'
         line=strtrim(strcompress(line),2)
         words=strsplit(line,/extract)
         ffn[i] = words[0]
         ftype[i] = words[1]
         xc[i]    = words[2]
         yc[i]    = words[3]
         cra[i]   = words[4]
         cdec[i]  = words[5]
         flagarr[i,*]=fix(words[6:15])
         ncoeffs[i]=fix(total(flagarr[i,*])+0.5)
         coeffarr[i,0:ncoeffs[i]-1] = words[16:16+ncoeffs[i]-1]
      ENDFOR
      free_lun,lun

      ; Now write out the new file
      openw,lun,fitfile,/get_lun
      printf,lun,latest
      FOR i=0,nlines-1 DO BEGIN
         IF ffn[i] ne '' THEN BEGIN
            printf,lun,ffn[i],ftype[i],xc[i],yc[i],cra[i],cdec[i], $
               photzp[i],flagarr[i,*],coeffarr[i,0:ncoeffs[i]-1], $
               format='(6(a,1x),f6.3,1x,10(i1,1x),10(1x,a))'
         ENDIF
      ENDFOR
      free_lun,lun

   endif
 
end

