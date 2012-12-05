;+
; NAME:
;  tvgrab
; PURPOSE:   (one line only)
;  Grab plot window and save to an protable image file.
; DESCRIPTION:
; CATEGORY:
;  File I/O
; CALLING SEQUENCE:
;  tvgrab,fn,win
; INPUTS:
;  fn  - file name for output JPG file
;  win - window number to grab and save
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
;  PNG - flag, if set saves image in PNG format, otherwise saved as JPG
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2002/10/04
;  2005/07/27, MWB, added PNG keyword
;-
pro tvgrab,fn,win,PNG=png

   if n_params() eq 0 then begin
      print,'tvgrab,filename,window'
      return
   endif

   self='TVGRAB: '
   if badpar(win,[0,1,2,3],0,caller=self+'(win)',default=-1) then return
   if badpar(png,[0,1,2,3],0,caller=self+'(PNG)',default=0) then return

   if win ge 0 then setwin,win

   img=tvrd(/true)
   if png then begin
      write_png,fn,img
   endif else begin
      write_jpeg,fn,img,true=1,quality=100
   endelse

end
