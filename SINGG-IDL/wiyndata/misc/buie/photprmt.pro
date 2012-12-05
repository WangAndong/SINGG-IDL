;+
; NAME:
;  photprmt
; PURPOSE:
;  Promote version of a photometry log file to highest version.
; DESCRIPTION:
;
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
;  photprmt,photfile
; INPUTS:
;
;  photfile - File with photometry data.
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;
;  The photfile is updated to the most recent version.  Not changed if already
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
;  2000/06/02, Written by Marc W. Buie, Lowell Observatory
;  2002/09/03, MWB, changed Str_sep call to strsplit
;
;-
pro photprmt,photfile

   if badpar(photfile,7,0,caller='PHOTPRMT (photfile) ') then return

   ; If not present, don't do anything.
   IF not exists(photfile) THEN return

   ; Check the file version.
   version=''
   openr,lun,photfile,/get_lun
   readf,lun,version,format='(a)'

   ; It's current, do nothing.
   latest='PHOTFILE v1.0'
   if version eq latest then begin
      free_lun,lun
      return
   endif

   ; upgrade from anonymous file to version tagged file.

   version=strtrim(strcompress(version),2)
   words=strsplit(version,' ',/extract)
   nwords=n_elements(words)
   if nwords le 16 then begin
         print,photfile,' is of an unrecognized format, aborting.'
         print,'version tag seen: [',version,']'
         free_lun,lun
         return
   endif
   version='PHOTFILE v0.0'
   free_lun,lun

   print,'PHOTPRMT: Upgrading file from ',version,' to ',latest

   if version eq 'PHOTFILE v0.0' then begin

      ; Read the file
      openr,lun,photfile,/get_lun

      ;Read through and count the number of lines.
      line=''
      nobs=0
      while(not eof(lun)) do begin
         readf,lun,line,format='(a1)'
         nobs=nobs+1
      endwhile
      nlines=nobs

      ;Rewind file.
      point_lun,lun,0

      ;Create the output data vectors
      filename = strarr(nobs)
      obj      = strarr(nobs)
      fil      = strarr(nobs)
      jd       = dblarr(nobs)
      exptime  = fltarr(nobs)
      gain     = fltarr(nobs)
      rad      = fltarr(nobs)
      sky1     = fltarr(nobs)
      sky2     = fltarr(nobs)
      serial   = intarr(nobs)
      xpos     = fltarr(nobs)
      ypos     = fltarr(nobs)
      fwhm     = fltarr(nobs)
      maxcnt   = fltarr(nobs)
      sky      = fltarr(nobs)
      skyerr   = fltarr(nobs)
      mag      = fltarr(nobs)
      err      = fltarr(nobs)
      bad      = intarr(nobs)
      jd0 = 0.0d0

      for i=0,nobs-1 do begin

         ; Get the next input line.
         readf,lun,line,format='(a)'

         ; Read the filename, object name, and filter code as string bits.
         filename[i] = gettok(line,' ')
         obj[i]      = gettok(line,"'") ; This is a dummy read to drop the first quote
         obj[i]      = gettok(line,"'")
         fil[i]      = gettok(line,' ')

         ; Read the rest of the data which is all numeric.
         reads,line,format='(d13.5,f9.3,f7.2,3f8.3,i5,2f9.3,f6.2,f8.1,f9.4,f8.4)', $
            jd0,exptime0,gain0,rad0,sky1_0,sky2_0,serial0, $
            xpos0,ypos0,fwhm0,maxcnt0,mag0,err0

         jd[i]       = jd0
         exptime[i]  = exptime0
         gain[i]     = gain0
         rad[i]      = rad0
         sky1[i]     = sky1_0
         sky2[i]     = sky2_0
         serial[i]   = serial0
         xpos[i]     = xpos0
         ypos[i]     = ypos0
         fwhm[i]     = fwhm0
         maxcnt[i]   = maxcnt0
         mag[i]      = mag0
         err[i]      = err0
         bad[i]      = string(strmid(line,107,2))

      endfor

      free_lun,lun

      ; Now write out the new file
      fmt1 = '(a,1x,"''",a,"''",1x,a,1x,f13.5,1x,f8.3,1x,f6.2,1x,f7.3,1x,f7.3,' + $
             '1x,f7.3,1x,i4.4,1x,f8.3,1x,f8.3,1x,f5.2,1x,f7.1,1x,f8.2,1x,f6.2,' + $
             '1x,f8.4,1x,f7.4,1x,i1)'
      openw,lun,photfile,/get_lun
      printf,lun,latest
      for i=0,nobs-1 do begin
         printf, lun, format=fmt1, $
            filename[i], obj[i], fil[i], jd[i], exptime[i], gain[i], rad[i], $
            sky1[i], sky2[i], serial[i], xpos[i], ypos[i], fwhm[i], $
            maxcnt[i], sky[i], skyerr[i], mag[i], err[i], bad[i]
      endfor
      free_lun,lun

   endif

end

