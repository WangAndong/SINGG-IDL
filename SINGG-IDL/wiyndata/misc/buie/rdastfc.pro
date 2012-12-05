;+
; NAME:
;  rdastfc
; PURPOSE:
;  Read an astrometry fit coefficient file.
; DESCRIPTION:
;
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
; rdastfc,file,ffn,ftype,xc,yc,cra,cdec,photzp,flagarr,coeffarr,ncoeffs,nlines
; INPUTS:
;  file     - Input file name to be read.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;  Each of these ouputs are vectors or arrays.  The length of the vectors or
;    the number of rows is equal to the number of fit coefficient sets in
;    the file.  For arrays, the dimensions are [nlines,10]
;  ffn      - File name for this fit.
;  ftype    - Type of fit (eta or xi)
;  xc       - X center of array
;  yc       - Y center of array
;  cra      - Center right acension (radians)
;  cdec     - Center declination (radians)
;  photzp   - Photometric zero-point for this image.  If the value is
;               99.0 the zero point has not been determined.  This will
;               allow you to later compute a real magnitude from instrumental
;               magnitudes on the frame.
;  flagarr  - Array of flags for terms to use (see astterms.pro)
;  coeffarr - Array of astrometric fit coefficients (see astterms.pro)
;  ncoeffs  - Number of fitted coefficients
;  nlines   - Number of coefficients read from file.
;
; KEYWORD OUTPUT PARAMETERS:
;  VERSION - Version tag of file that was read.
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  1999/04/15, Written by Marc W. Buie, Lowell Observatory
;  2000/01/19, MWB, added version 1.1 support (now includes photzp)
;  2002/04/09, MWB, changed from Str_Sep to strsplit
;  2005/06/28, MWB, fixed bug when reading an empty file.
;
;-
pro rdastfc,file,ffn,ftype,xc,yc,cra,cdec,photzp, $
            flagarr,coeffarr,ncoeffs,nlines, $
            VERSION=version

   if badpar(file,7,0,caller='rdastfc (file) ') then return

   ; Pre-declare strings for later use.
   version=''
   line=''

   ; Next load the fit coefficients file, watch for different versions.
   openr,lun,file,/get_lun

   ; Count the number of lines in file.
   nlines=0L
   WHILE not eof(lun) DO BEGIN
      readf,lun,line,format='(a1)'
      nlines=nlines+1
   ENDWHILE
   point_lun,lun,0

   ; First line gives hint to version
   readf,lun,version,format='(a)'
   IF version eq 'ASTFIT v1.0' or version eq 'ASTFIT v1.1' THEN BEGIN
      nlines=nlines-1
   ENDIF ELSE BEGIN
      ; This is no recognized version tag, that means it's either an incorrect
      ;   file type, or, it's the original version that didn't have a tag.
      ;   If the latter, there must be a certain number of blank delimited
      ;   fields in the line.
      version=strtrim(strcompress(version),2)
      words=strsplit(version,' ',/extract)
      nwords=n_elements(words)
      IF nwords le 15 THEN BEGIN
         print,'fitcoeff.dat file is of an unrecognized format, aborting.'
         free_lun,lun
         return
      ENDIF
      nterms = fix(total(fix(words[4:13])))
      IF nwords ne nterms+14 THEN BEGIN
         print,'fitcoeff.dat file is of an unrecognized format, aborting.'
         free_lun,lun
         return
      ENDIF
      version='ASTFIT v0.0'
      point_lun,lun,0

   ENDELSE

   if nlines eq 0 then begin
      free_lun,lun
      return
   endif

   ; Common init
   ffn      = strarr(nlines)
   ftype    = strarr(nlines)
   xc       = dblarr(nlines)
   yc       = dblarr(nlines)
   cra      = dblarr(nlines)
   cdec     = dblarr(nlines)
   flagarr  = intarr(nlines,10)
   coeffarr = dblarr(nlines,10)
   ncoeffs  = intarr(nlines)
   photzp   = replicate(99.0,nlines)

   IF version eq 'ASTFIT v0.0' THEN BEGIN
      FOR i=0,nlines-1 DO BEGIN
         readf,lun,line,format='(a)'
         line=strtrim(strcompress(line),2)
         words=strsplit(line,' ',/extract)
         ffn[i] = words[0]
         ftype[i] = words[1]
         xc[i]    = double(words[2])
         yc[i]    = double(words[3])
         flagarr[i,*]=fix(words[4:13])
         ncoeffs[i]=fix(total(flagarr[i,*])+0.5)
         coeffarr[i,0:ncoeffs[i]-1] = double(words[14:14+ncoeffs[i]-1])
      ENDFOR

      if not exists('centers.dat') then begin
         nlines=0
         free_lun,lun
         print,'RDASTFC: Error! no centers.dat file available to assist conversion.'
         return
      endif

      ; Load the centers.dat file
      readcol,'centers.dat',cfn,cras,cdecs,format='a,a,a',/silent
      ncen=n_elements(cfn)
      cra0=raparse(cras)
      cdec0=decparse(cdecs)

      ; Copy the centers to cra,cdec
      FOR i=0,nlines-1 DO BEGIN
         ; Locate index for center information
         z=where(ffn[i] eq cfn,count)
         IF count eq 0 THEN BEGIN
            print,'Error: No center found for file: ',ffn[i]
            ffn[i] = ''
         ENDIF ELSE IF count eq 1 THEN BEGIN
            cra[i]  = cra0[z[0]]
            cdec[i] = cdec0[z[0]]
         ENDIF ELSE BEGIN
            print,'Error: Multiple centers found for file: ',ffn[i]
            ffn[i] = ''
         ENDELSE
      ENDFOR

   ENDIF ELSE IF version eq 'ASTFIT v1.0' THEN BEGIN

      FOR i=0,nlines-1 DO BEGIN
         readf,lun,line,format='(a)'
         line=strtrim(strcompress(line),2)
         words=strsplit(line,' ',/extract)
         ffn[i] = words[0]
         ftype[i] = words[1]
         xc[i]    = double(words[2])
         yc[i]    = double(words[3])
         cra[i]   = raparse(words[4])
         cdec[i]  = decparse(words[5])
         flagarr[i,*]=fix(words[6:15])
         ncoeffs[i]=fix(total(flagarr[i,*])+0.5)
         coeffarr[i,0:ncoeffs[i]-1] = double(words[16:16+ncoeffs[i]-1])
      ENDFOR

   ENDIF ELSE IF version eq 'ASTFIT v1.1' THEN BEGIN

      FOR i=0,nlines-1 DO BEGIN
         readf,lun,line,format='(a)'
         line=strtrim(strcompress(line),2)
         words=strsplit(line,' ',/extract)
         ffn[i] = words[0]
         ftype[i] = words[1]
         xc[i]    = double(words[2])
         yc[i]    = double(words[3])
         cra[i]   = raparse(words[4])
         cdec[i]  = decparse(words[5])
         photzp[i]= float(words[6])
         flagarr[i,*]=fix(words[7:16])
         ncoeffs[i]=fix(total(flagarr[i,*])+0.5)
         coeffarr[i,0:ncoeffs[i]-1] = double(words[17:17+ncoeffs[i]-1])
      ENDFOR

   ENDIF

   free_lun,lun

end

