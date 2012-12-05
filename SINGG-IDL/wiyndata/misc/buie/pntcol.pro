;+
; NAME:
;  pntcol
; PURPOSE:
;  Collate pointing data from astrometry results and image headers.
; DESCRIPTION:
; CATEGORY:
;  Astrometry
; CALLING SEQUENCE:
;  pntcol
; INPUTS:
; OPTIONAL INPUT PARAMETERS:
; KEYWORD INPUT PARAMETERS:
; OUTPUTS:
; KEYWORD OUTPUT PARAMETERS:
; COMMON BLOCKS:
; SIDE EFFECTS:
; RESTRICTIONS:
; PROCEDURE:
; MODIFICATION HISTORY:
;  Written by Marc W. Buie, Lowell Observatory, 2001/12/19
;-
pro pntcol,KEYLIST=keylist,DDIR=ddir

   if badpar(ddir,[0,7],0,CALLER='PNTCOL: (DDIR) ', $
                              default='') then return
   if badpar(keylist,[0,7],0,CALLER='PNTCOL: (KEYLIST) ', $
                              default='[[DEFAULT]]') then return

   if not exists('centers.dat') then begin
      print,'centers.dat not found, aborting.'
      return
   endif

   readcol,'centers.dat',fn,cra,cdec,format='a,a,a'

   npts = n_elements(cra)

   hra  = dblarr(npts)
   hdec = dblarr(npts)
   hjd  = dblarr(npts)

   ; Get header correspondence list.
   loadkeys,keylist,hdrlist

   for i=0,npts-1 do begin
      hdr=headfits(ddir+fn[i])
      parsekey,hdr,hdrlist,hdrinfo
      hra[i]  = hdrinfo.ra
      hdec[i] = hdrinfo.dec
      hjd[i]  = hdrinfo.jd
   endfor

   rastr,hra,4,hras
   decstr,hdec,3,hdecs

   openw,lun,'pnt.dat',/get_lun
   for i=0,npts-1 do begin
      printf,lun,hjd[i],hras[i],hdecs[i],cra[i],cdec[i],format='(f13.5,4(1x,a))'
   endfor
   free_lun,lun

end
