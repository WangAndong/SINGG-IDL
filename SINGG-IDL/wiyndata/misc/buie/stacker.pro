;+
; NAME:
;  stacker
; PURPOSE:   (one line only)
;  Stack (co-add) image while registering images.
; DESCRIPTION:
; CATEGORY:
;  CCD data processing
; CALLING SEQUENCE:
;  stacker,fnlist,xpos,ypos,image
; INPUTS:
;  fnlist - String array of fits files to read
;  xpos   - Array of x-positions, one per frame of registration point.
;  ypos   - Array of y-positions, one per frame of registration point.
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  ANCHORPOS - [x,y] position to align registration point to, default is
;                the position of the anchor object.
;  CROP      - [x1,x2,y1,y2] region of image to save.  These values are
;                in the pixel coordinate system of the anchor frame.
;                Default is the full frame.
;  ANCHORFRAME - Index into fnlist for the frame to be used as the anchor.
;                  Default is the first frame.
;  ROBUST    - Flag, if set requests that a robust average of the image
;                  stack be performed.  If the number of images is 2 then
;                  this keyword is ignored.
;  SILENT    - Flat, if set will suppress all messages to screen.
;
;  DEBUG     - Flag, if set turns on extra debug steps and other information.
;                The debug information in not guaranteed to remain static
;                from one verion of the program or another.
; OUTPUTS:
;  image - Floating point array with stacked image
; KEYWORD OUTPUT PARAMETERS:
;  OUTFILE - Name of file to write the image to.  This is output in FITS format
;              and the header is derived from the first image with some
;              modifications.
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2005/07/21
;  2005/11/22, MWB, added DEBUG keyword
;-
pro stacker,fnlist,xpos,ypos,image, $
       CROP=in_crop,ROBUST=robust,EXTENSION=exten,SILENT=silent,DEBUG=debug

   self='STACKER: '
   if badpar(fnlist,7,1,caller=self+'(fnlist) ',npts=n1) then return
   if badpar(xpos,[2,3,4,5,6],1,caller=self+'(xpos) ',npts=n2) then return
   if badpar(ypos,[2,3,4,5,6],1,caller=self+'(ypos) ',npts=n3) then return
   if badpar(in_crop,[0,2,3,4,5],1,caller=self+'(ANCHORPOS) ',npts=n4, $
                default=[-1,-1,-1,-1]) then return
   if badpar(anchorframe,[0,2,3,4,5],0,caller=self+'(ANCHORFRAME) ',default=0) then return
   if badpar(anchorpos,[0,2,3,4,5],1,caller=self+'(ANCHORPOS) ',npts=n5, $
                default=[xpos[anchorframe],ypos[anchorframe]]) then return
   if badpar(robust,[0,1,2,3],0,caller=self+'(ROBUST) ',default=0) then return
   if badpar(silent,[0,1,2,3],0,caller=self+'(SILENT) ',default=0) then return
   if badpar(debug,[0,1,2,3],0,caller=self+'(DEBUG) ',default=0) then return
   if badpar(exten,[0,1,2,3],0,caller=self+'(EXTENSION) ',default=0) then return

   if min([n1,n2,n3]) ne max([n1,n2,n3]) then begin
      print,self,' fnlist, xpos, and ypos must all have the same length.'
      return
   endif

   if n4 ne 4 then begin
      print,self,' crop must be a 4 element vector'
      return
   endif

   if n5 ne 2 then begin
      print,self,' anchorpos must be a 2 element vector'
      return
   endif

   hdr = headfits(fnlist[anchorpos],exten=exten)
   nx = sxpar(hdr,'NAXIS1')
   ny = sxpar(hdr,'NAXIS2')

   if min(in_crop) lt 0 then begin
      crop = [0,nx-1,0,ny-1]
   endif else begin
      crop = fix(in_crop+0.5)
   endelse

   outnx = crop[1]-crop[0]+1
   outny = crop[3]-crop[2]+1

   stack=fltarr(outnx,outny,n1)
   
   for i=0,n1-1 do begin
      raw = readfits(fnlist[i],hdr_raw,exten=exten)
      ; offset from current frame to reference frame
      dx = long(anchorpos[0]-xpos[i]+0.5)
      dy = long(anchorpos[1]-ypos[i]+0.5)
      ; coordinates of array boundaries in reference frame system
      rx1 = dx
      rx2 = (nx-1)+dx
      ry1 = dy
      ry2 = (ny-1)+dy
      ; adjust these coordinates to valid region of reference frame
      rx1v = rx1>crop[0] - crop[0]
      rx2v = rx2<crop[1] - crop[0]
      ry1v = ry1>crop[2] - crop[2]
      ry2v = ry2<crop[3] - crop[2]
      ; convert these coordinates back into current frame coordinates
      rx1r = rx1v+crop[0] - dx
      rx2r = rx2v+crop[0] - dx
      ry1r = ry1v+crop[2] - dy
      ry2r = ry2v+crop[2] - dy
      ; find mean sky level and subtract as image gets poked into the stack
      skysclim,raw[rx1r:rx2r,ry1r:ry2r],lowval,hival,meanval,sigval,NPTS=5000
      ; stuff part of raw that maps onto blank, watch out for edges
      stack[rx1v:rx2v,ry1v:ry2v,i] = raw[rx1r:rx2r,ry1r:ry2r]-meanval
   endfor

   if debug then itool,stack,/block

   if robust and n1 ge 3 then begin
      stack += 1000.0
      avgclip,stack,image,silent=(silent and (debug eq 0))
      image -= 1000.0
   endif else begin
      image = float(stack[*,*,0])
      for i=1,n1-1 do image += float(stack[*,*,i])
      image /= float(n1)
   endelse

end
