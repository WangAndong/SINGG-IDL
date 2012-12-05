;+
; NAME:
;    p6model
; PURPOSE: (one line)
;    Generate synthetic PSF images for the HST Planetary Camera, Chip 6.
; DESCRIPTION:
;    A generated PSF is added to the output array.  If the array is not
; defined, it is created with dimensions 800 by 800.  Successive calls
; will add objects (PSF's) to the array.
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;    p6model, x, y, inten, date, filter, bmvnum, back, image
; INPUTS:
;    x, y    : Position of the PSF in the output array.
;    inten   : Intensity of PSF relative to TinyTim output PSF
;    date    : Date of observation (YYMMDD format).
;    filter  : Filter name in the form Fxxx(W,LP,M,N).
;    bmvnum  : B-V list value (old TinyTim V2.4 values):
;                1) -0.297 (Type O)
;                2) -0.155 (Type B)
;                3)  0.126 (Type A)
;                4)  0.395 (Type F)
;                5)  0.619 (Type G)
;                6)  1.110 (Type K)
;                7)  1.590 (Type M)
;    back    : Background to be added.
; OPTIONAL INPUT PARAMETERS:
;
;    JITTER  : If set contains the gaussian smearing to apply to the image.
;                This is in units of 1/e half width.  If not set, no smearing
;                is applied.
;
;    OBJRAD  : Radius of object in pixels.  If not set, assumed to be a point
;              source.  If set and radius is greater than 0.5, then PSF is
;              convolved with a circular function of this radius.  This models
;              the object as a Lambert disk, crude but useful for barely
;              resolved objects.
;
; KEYWORD PARAMETERS:
;    NEW     : If set, clears the output array to zeros before building the
;              PSF image(s).
;    HSTPATH : Alternate path for HST PSF's.
;    VERBOSE : If set, prints informational message.
; OUTPUTS:
;    image  : The image array into which the PSF is placed.
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURE:
;    The Tiny Tim programs are used, via the procedure HSTPSF, to generate
; a 2X (7.5 micron spacing) PSF at the nearest grid location to the requested
; position. (This grid is defined in hstpsf).
;    From the interpolated maximum returned by hstpsf, shift amounts for x
; and y are computed.
;    The PSF is shifted by these amounts and then compressed to a 1X
; (15 micron spacing) PSF.
;    Finally, the resulting PSF is multiplied by the intensity parameter and
; added into the output array.
;    Calls external procedures BADPAR, BOXM, HSTPSF, and SSHIFT2D.
; MODIFICATION HISTORY:
;    Written by Doug Loucks, Lowell Observatory, December, 1993.
;    12/10/93, DWL, Modified to allow the first three parameters to be
; vectors.
;    12/16/93, DWL, Added VERBOSE keyword.
;    1/19/94, DWL, Added HSTPATH keyword.
;    1/21/94, DWL, Added x-maximum and y-maximum parameters to hstpsf call.
;    2003/1/15, DWL, Modified to call the updated routine HSTPSF, which
; uses TinyTim Version 6.1 Alpha. A PSF is returned from HSTPSF, only if
; the requested PSF file is found in disk cache.
;-

pro p6model, x, y, inten, date, filter, bmvnum, back, image,$
             HSTPATH=hstpath,$
             JITTER=jitter,$
             NEW=new,$
             OBJRAD=objrad,$
             VERBOSE=verbose

if n_params() lt 8 then begin
   print, 'p6model, x, y, inten, date, filter, bmvnum, back, image'
   return
endif

self = '% P6MODEL: '
if badpar(x,   [2,3,4,5], [0,1], caller=self+'(x) ', npts=n1) then return
if badpar(y,   [2,3,4,5], [0,1], caller=self+'(y) ', npts=n2) then return

if badpar(inten, [2,3,4,5], [0,1], caller=self+'(inten) ', npts=n3 ) $
   then return

; Check arguments.
if badpar(date,   7,       0,     caller=self+'(date) ') then return
if badpar(filter, 7,       0,     caller=self+'(filter) ') then return
if badpar(bmvnum, [2,3],   0,     caller=self+'(bmvnum) ') then return
if badpar(back,   [2,3,4,5], 0,   caller=self+'(back) ') then return

; Check keyword parameters.
if badpar(hstpath, [0,7], 0, caller=self+'(hstpath) ', default='') then return
if badpar(new, [0,2], 0, caller=self+'(new) ', default=0) then return

if badpar(objrad, [0,2,3,4,5], [0,1], caller=self+'(objrad) ',$
          npts=n4) then return

if badpar(verbose, [0,2], 0, caller=self+'(verbose) ', default=0) then return


; Check that the first three parameters have equal length.
t = [n1, n2, n3]
w = where(max(t) ne min(t), count)

if count ne 0 then begin
   msg = 'Error. x, y, and intensity parameters must have the same length.'
   message, msg, /info
   return
endif

; Verify valid OBJRAD if supplied
if n_elements(objrad) ne 0 and n4 ne n1 then begin
   msg = 'Error. OBJRAD must have the same length as the x and y parameters.'
   message, msg, /info
   return
endif

stat = size(image)
ndim = stat[0]

if ndim ne 2 then begin
   ; The output array must be created.
   im_xsize = 800
   im_ysize = 800
   image = fltarr(im_xsize, im_ysize)
endif else begin
   ; It exists.  Get its dimensions.
   im_xsize = stat[1]
   im_ysize = stat[2]
endelse

if new then image[0:im_xsize-1, 0:im_ysize-1]=0

; Define the bin factor for compression.
binfact = 0.5

for j=0, n1-1 do begin
   if verbose then begin
      message, 'Working on object at ' + string(x[j], y[j], $
      format='(2F12.4)'), /info
   endif

   hstpsf, x[j], y[j], date, filter, bmvnum, psf, xm, ym, $
           HSTPATH=hstpath,$
           CAMERA=2, CHIP=6, PSFSIZE=5, SAMPFACT=2, VERBOSE=verbose

   psfstat = size(psf)

   if psfstat[0] lt 2 then begin
      message, 'Error: Procedure HSTPSF did not return a PSF array ' +$
         '(The requested PSF was not found in disk cache).', /info
      return
   endif

   xsize = psfstat[1]
   ysize = psfstat[2]

   ; Compute the required shift, allowing for the subsequent rebin operation.
   ix = fix(x[j])
   iy = fix(y[j])

   if (ix lt 0) or (ix ge im_xsize) then begin
      t = string(im_xsize, format='(G0.0)')
      message, 'Error. X-position out of bounds. (0 <= X < ' + t + ').', /info
      return
   endif

   if (iy lt 0) or (iy ge im_ysize) then begin
      t = string(im_ysize, format='(G0.0)')
      message, 'Error. Y-position out of bounds. (0 <= X < ' + t + ').', /info
      return
   endif

   fx = x[j] - ix
   fy = y[j] - iy
   ixm = fix(xm)
   iym = fix(ym)
   fxm = xm - ixm
   fym = ym - iym

   ; Create the shift vector.
   svec = [fx+fx-fxm, fy+fy-fym]

   ; Shift the psf.
   spsf = sshift2d(psf, svec)

   ; Convolve PSF with circular function if requested.
   if n_elements(objrad) ne 0 then begin
      ; Compute size of kernel.
      nk = ceil(objrad[j]/binfact - 0.5) * 2 + 1

      ; If kernel is bigger than one pixel then do the convolution.
      if nk gt 1 then begin
         kc = nk / 2
         idx=indgen(nk)
         unit=replicate(1,nk)
         x=idx#unit
         y=unit#idx
         kernel=pixwt(kc,kc,objrad[j]/binfact,x,y)
         kernel=kernel/total(kernel)
         spsf=convol(spsf,kernel)
      endif
   endif

   ; Rebin the psf.
   fpsf = rebin(spsf, xsize*binfact, ysize*binfact)
   psfstat  = size(fpsf)
   xsize    = psfstat[1]
   ysize    = psfstat[2]
   boxm, fpsf, xsize/2, ysize/2, 10, 10, xmax, ymax

   ; Compute the half-width of the PSF that will fit into the output array
   ; at the requested location.
   hw = min([xmax, ymax, xsize-xmax, ysize-ymax, ix, iy, im_xsize-ix, $
               im_ysize-iy])

   ; Add the appropriate sub-section of the PSF to the output array.
   image[ix-hw : ix+hw-1, iy-hw : iy+hw-1] = $
   image[ix-hw : ix+hw-1, iy-hw : iy+hw-1] + $
   fpsf[xmax-hw : xmax+hw-1, ymax-hw : ymax+hw-1] * inten[j]
endfor

if n_elements(jitter) ne 0 then begin
   x=[-2.0,-1.,0.,1.0,2.0]
   y=[-2.0,-1.,0.,1.0,2.0]
   i=[1.0,1.0,1.0,1.0,1.0]
   rsq = (x^2#i + i#y^2)/jitter^2
   kernel = exp(-rsq/2.)
   kernel=kernel/total(kernel)
   image=convol(image, kernel)
endif

image = image + back

end
