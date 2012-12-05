;+
; NAME:
;  wrastfc
; PURPOSE:
;  Write an astrometry fit coefficient file.
; DESCRIPTION:
;
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
; wrastfc,file,ffn,ftype,xc,yc,cra,cdec,photzp,flagarr,coeffarr,ncoeffs
; INPUTS:
;  file     - Output file name to be written.
;
;  Each of these inputs are vectors or arrays.  The length of the vectors or
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
;
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;
; MODIFICATION HISTORY:
;  2003/06/30, Written by Marc W. Buie, Lowell Observatory
;
;-
pro wrastfc,file,ffn,ftype,xc,yc,cra,cdec,photzp, $
            flagarr,coeffarr,ncoeffs

   self='WRASTFC: '
   if badpar(file,7,0,caller=self+'(file) ') then return
   if badpar(ffn,7,1,caller=self+'(ffn) ',npts=nlines) then return
   if badpar(ftype,7,1,caller=self+'(ftype) ') then return
   if badpar(xc,[4,5],1,caller=self+'(xc) ') then return
   if badpar(yc,[4,5],1,caller=self+'(yc) ') then return
   if badpar(cra,[4,5],1,caller=self+'(cra) ') then return
   if badpar(cdec,[4,5],1,caller=self+'(cdec) ') then return
   if badpar(photzp,[4,5],1,caller=self+'(photzp) ') then return
   if badpar(flagarr,[1,2,3],2,caller=self+'(flagarr) ') then return
   if badpar(coeffarr,[4,5],2,caller=self+'(coeffarr) ') then return
   if badpar(ncoeffs,[2,3],1,caller=self+'(ncoeffs) ') then return

   latest='ASTFIT v1.1'

   ; Now write out the new file
   rastr,cra,4,cras
   decstr,cdec,3,cdecs
   openw,lun,file,/get_lun
   printf,lun,latest
   for i=0,nlines-1 do begin
      if ffn[i] ne '' then begin
         printf,lun,ffn[i],ftype[i],xc[i],yc[i],cras[i],cdecs[i], $
            photzp[i],flagarr[i,*],coeffarr[i,0:ncoeffs[i]-1], $
            format='(2(a,1x),2(e15.7,1x),2(a,1x),f8.3,1x,10(i1,1x),10(1x,e20.12))'
      endif
   endfor
   free_lun,lun

end
