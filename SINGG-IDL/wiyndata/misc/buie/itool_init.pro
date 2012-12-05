;+
; NAME:
;    itool_init
; PURPOSE:
;    Itool initialization.
; DESCRIPTION:
;
;    The contents of the image parameter structure are:
;      These usually come from image header information.
;        airmass  - float,  airmass of image
;        date     - string, UT date of image at start
;        exptime  - float,  exposure time of image in seconds
;        expdelta - float,  time between frames in seconds for image cubes
;        filter   - string, filter used for image
;        imfile   - string, name of file where image came from
;        jd       - double, Julian Date of middle of exposure for image.
;        object   - string, Descriptive name of object
;        ut       - string, UT time of image at start
;        xsize    - int,    width of image(s) in pixels
;        ysize    - int,    height of image(s) in pixels
;        nframes  - long,   Number of frames in image cube
;        imageptr - Pointer to image cube
;
;        asis     - boolean array
;                     false --> call autostrech computer, if NOT ready, then
;                               scl is computed and set to cur, if ready, then
;                               there is no change.
;                     true  --> cur is valid, use it.
;        autophot - int,    Flag, set when auto photometry requested.
;        frame    - long,   Current frame being displayed
;        curmin   - float array, actual display min
;        curmax   - float array, actual display min
;        lastfwhm - float, Last FWHM of object or anchor object
;        lastmag  - float, Last magnitude of object or anchor object
;        lastpos  - intarr(2), Last photometry position
;        lasttype - int, type of last click 0=none, 1=left, 2=right
;        minvalue - float array, minimum in each frame
;        maxvalue - float array, maximum in each frame
;        ready    - boolean array, true --> autoscale values are valid
;        sclmin   - float array, autoscale display min
;        sclmax   - float array, autoscale display max
;        title    - string, name for structure
; CATEGORY:
;    Compound Widgets
; CALLING SEQUENCE:
;    itool_init, image, im_parms
; INPUTS:
;    image : The image array.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
; OUTPUTS:
;    im_parms : The image parameters structure.
; OPTIONAL OUTPUT PARAMETERS:
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
;    2004, Apr, Doug Loucks, Consultant for Lowell Observatory.
;      Adapted from it_init.pro. Minor cosmetic changes. The "imageptr"
; tag in the "im_parms" structure is now a pointer variable, rather than
; a long scalar.
;-

; ------------------------------------------------------------------------------
; Procedure itool_init
; ------------------------------------------------------------------------------
pro itool_init, image, im_parms
   ; Build the image parameter structure from the image size information.
   ;
   ; All of the information itool needs to create the draw windows is computed
   ; here and placed in the image parameters structure. This structure is used
   ; as the 'value' of cw_itool.

   image_size = size(image)
   xsize = image_size[1]
   ysize = image_size[2]
   nframes = 1L
 
   if image_size[0] eq 3 then begin
      ; We have a cube of images.
      nframes = image_size[3]
   endif

   ; Initialize the image parameters structure.
   im_parms = {$
      airmass:0.0,$
      autophot:0,$      ; Flag, set when auto photometry requested.
      date:'',$
      exptime:1.0,$
      expdelta:0.0,$
      filter:'',$
      imageptr:ptr_new(),$
      imfile:'',$
      jd:0.0D0,$
      lastfwhm:0.0,$        ; Last FWHM
      lastmag:0.0,$         ; Last magnitude
      lastpos:intarr(2),$   ; Last photometry position
      lasttype:0,$          ; Type of last click 0=none, 1=left, 2=right
      object:'',$
      ut:'',$
      asis:BYTARR(nframes),$
      frame:0L, nframes:nframes,$
      curmin:FLTARR(nframes), curmax:FLTARR(nframes),$
      minvalue:FLTARR(nframes), maxvalue:FLTARR(nframes),$
      ready:BYTARR(nframes),$
      sclmin:FLTARR(nframes), sclmax:FLTARR(nframes),$
      xsize:xsize, ysize:ysize,$
      title:'Itool'}
end
