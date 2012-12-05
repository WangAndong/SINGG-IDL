;+
; NAME:
;    itool
; PURPOSE: (one line)
;    General purpose image display (front-end for itool\_\_define).
; DESCRIPTION:
;    This procedure simplifies the use of the object-oriented itool GUI,
; (the 'itool' object class) by acting as a "front end" procedure which may
; be called from the IDL command prompt.
;    By default, this procedure runs the itool GUI in non-blocked mode.
; There is a side-effect from running in non-blocked mode: only a copy of
; the input argument is passed to the itool GUI. Hence, any changes to the
; image within the itool GUI will be lost. Currently, the pixel editor in
; the itool GUI is the only mechanism for modifying the image. If the user
; wishes to retrieve a modified image argument, the itool GUI must be
; launched in blocked mode. Setting the BLOCK keyword to this procedure
; will run the itool GUI in blocked mode. Upon closing the GUI, this
; procedure will exit to the IDL command prompt and the input image
; argument will be available. Any changes made to the image while running the
; itool GUI in blocked mode will be reflected in the image argument.
;
; CATEGORY:
;    General-purpose Image display
; CALLING SEQUENCE:
;
; INPUTS:
;    image : The image array.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD PARAMETERS:
;  BLOCK          : If set, forces the itool GUI to run in blocked mode.
;                   Otherwise, the itool GUI is launched in non-blocked mode.
;                   See description, for details and side-effects.
;  FVISIBLE       : Size of the full-view window (128 pixels).
;  PHOTPARMFILE   : Optional photometry parameters file.
;  TMPLFILE       : Optional Photometry template file.
;  SCLMIN         : Stretch range minimum.
;  SCLMAX         : Stretch range maximum.
;  WXVISIBLE      : Creates work window with explicit x-size (500).
;  WYVISIBLE      : Creates work window with explicit y-size (500).
;  WZOOMFACT      : Ceiling for work-view zoom factor (unlimited).
;  ZVISIBLE       : Size of the zoom window (128 pixels).
;
; OUTPUTS:
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
;    Written by Doug Loucks, Lowell Observatory, July 29, 1994. This is a small
; 'front-end' for the new compound widget version of itool (cw_itool).
;
;  94/09/27 - Marc W. Buie, Lowell Observatory.  Modified exit on itool_event
;               to eliminate crash on exit if photometry or template parms
;               were changed.
;
;  2004/04/15 - Doug Loucks, Consultant for Lowell Observatory. Overhauled to
; work with the new object-oriented version of itool (itool__define).
;  2004/05/15 - DWL, Added the BLOCK keyword. See description for details.
;-

; ----------------------------------------------------------------------------
; Procedure Itool
;   Front-end program, to launch an instance of the itool GUI, using the
; 'itool' object class.
; ----------------------------------------------------------------------------
pro itool, image,$
   BLOCK=block,$
   FVISIBLE=fvisible,$
   PHOTPARMFILE=photparmfile, $
   SCLMIN=sclmin,$
   SCLMAX=sclmax, $
   TMPLFILE=tmplfile, $
   WXVISIBLE=wxvisible,$
   WYVISIBLE=wyvisible, $
   WZOOMFACT=wzoomfact, $
   ZVISIBLE=zvisible

   if n_params() ne 1L then begin
      print, 'Usage: itool, image'
      return
   endif

   if keyword_set(block) then no_block=0 else no_block=1
   if not keyword_set(photparmfile) then photparmfile=''
   if not keyword_set(tmplfile) then tmplfile=''

   ; Call the initialization procedure.
   itool_init, image, im_parms

   if keyword_set(sclmin) then begin
      im_parms.ready = 1B
      im_parms.sclmin = sclmin
   endif

   if keyword_set(sclmax) then begin
      im_parms.ready = 1B
      im_parms.sclmax = sclmax
   endif

   ; Create a pointer to the image argument.
   if no_block then begin
      ; Create the heap variable, copying the array data.
      pimage = ptr_new(image)
   endif else begin
      ; Create the heap variable without copying the array data.
      pimage = ptr_new(image, /NO_COPY)
   endelse


   ; Put a copy of the imape pointer into the im_parms structure.
   im_parms.imageptr = pimage

   ; Create a pointer to the im_parms structure.
   if no_block then begin
      ; Create the heap variable, copying the array data.
     pim_parms = ptr_new(im_parms)
   endif else begin
      ; Create the heap variable without copying the array data.
     pim_parms = ptr_new(im_parms, /NO_COPY)
   endelse


   ; Create a new instance of the object-oriented itool GUI.
   oitool = obj_new('itool', pim_parms,$
      FVISIBLE=fvisible,$
      IM_PARMS_CLEANUP=(no_block gt 0),$
      /NODISMISS,$
      PHOTPARMFILE=photparmfile,$
      TMPLFILE=tmplfile,$
      WXVISIBLE=wxvisible,$
      WYVISIBLE=wyvisible,$
      WZOOMFACT=wzoomfact,$
      XSIZE=(*pim_parms).xsize,$
      YSIZE=(*pim_parms).ysize,$
      ZVISIBLE=zvisible)

   if no_block then begin
      ; Realize the itool GUI in non-blocked mode and return to the caller.
      ; Note: The IM_PARMS_CLEANUP keyword passed to the itool GUI will be
      ; true (non-zero), in which case the GUI will free the pimage and
      ; pim_parms pointers, after the user exits the GUI.
      oitool->realize, /NO_BLOCK
      return
   endif else begin
      ; Realize the itool GUI in blocked mode. Note: the IM_PARMS_CLEANUP
      ; keyword passed to the itool GUI will be false (zero), in which
      ; case the GUI will NOT free the pimage and pim_parms pointers. They
      ; will be freed below, in this procedure, after the user exits the
      ; GUI. The caller's image argument will be restored before freeing
      ; the pimage pointer and the image argument will reflect any changes that
      ; were applied to it while running the GUI.
      oitool->realize
   endelse

   ; Come here after closing the blocked instance of the itool GUI.

   ; Recover the image array into the caller's image argument.
   image = *pimage

   ; Free the pointers.
   ptr_free, pimage, pim_parms
end
