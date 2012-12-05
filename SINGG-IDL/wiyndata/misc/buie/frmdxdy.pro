;+
; NAME:
;  frmdxdy
; PURPOSE:
;  Given two lists of source on field, find the dx,dy offset between lists.
; DESCRIPTION:
;
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  frmdxdy,x1,y1,x2,y2,xoff,yoff,error
; INPUTS:
;  x1 - X coordinate from list 1, in pixels.
;  y1 - Y coordinate from list 1, in pixels.
;  x2 - X coordinate from list 2, in pixels.
;  y2 - Y coordinate from list 2, in pixels.
; OPTIONAL INPUT PARAMETERS:
;
; KEYWORD INPUT PARAMETERS:
;
; OUTPUTS:
;  xoff - X offset (2-1) between positions in each list.
;  yoff - Yoffset (2-1) between positions in each list.
;  error - Flag, set if something went wrong in correlating the lists.
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
;  99/03/22, Written by Marc W. Buie, Lowell Observatory
;  2005/06/21, MWB, changed called to robomean to trap errors.
;-
pro frmdxdy,x1,y1,x2,y2,xoff,yoff,error

   nx = long(max([x1,x2]))
   ny = long(max([y1,y2]))
   n1 = n_elements(x1)

   ; The first step is to generate a cross-correlation image and find the
   ;    peak, this gives a crude offset.
   dxdy=intarr(nx,ny)
   for i=0,n1-1 do begin
      dx = fix(x2-x1[i]+0.5+nx/2.0)
      dy = fix(y2-y1[i]+0.5+ny/2.0)
      zd=where(dx ge 0 and dx lt nx and $
               dy ge 0 and dy lt ny,countzd)
      IF countzd gt 0 THEN BEGIN
         dxdy[dx[zd],dy[zd]]=dxdy[dx[zd],dy[zd]]+1
      ENDIF
   endfor
   zd=where(dxdy eq max(dxdy))
   xoff=(zd[0] mod nx)-nx/2.0
   yoff=zd[0]/nx-ny/2.0

   ; Using the rough offset, compute a good offset.
   gdx=fltarr(n1)
   gdy=fltarr(n1)
   for i=0,n1-1 do begin
      tdx=(x2-xoff)-x1[i]
      tdy=(y2-yoff)-y1[i]
      tdr=tdx^2+tdy^2
      zt=where(tdr eq min(tdr))
      zt=zt[0]
      if tdr[zt] lt 3.0 then begin
         gdx[i]=x2[zt[0]]-x1[i]
         gdy[i]=y2[zt[0]]-y1[i]
      endif else begin
         gdx[i]=nx*2
         gdy[i]=ny*2
      endelse
   endfor
   zt=where(gdx lt nx*1.5,countzt)
   if countzt ne 0 then begin
      robomean,gdx[zt],3.0,0.5,avgxoff,error=error
      if error then return
      robomean,gdy[zt],3.0,0.5,avgyoff,error=error
      if error then return
   endif else begin
      print,''
      print,' ERROR!   No matches in b-a offset'
      error=1
      return
   endelse

   if abs(xoff-avgxoff) gt 3.0 or abs(yoff-avgyoff) gt 3.0 then $
      error=1 $
   else $
      error=0

   xoff = avgxoff
   yoff = avgyoff

end

